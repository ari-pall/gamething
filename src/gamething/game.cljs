(ns gamething.game
  (:require
   [applied-science.js-interop :as j]
   [gamething.db :as db]
   [gamething.prototypes :as p]
   ;; [schema.core :as s :include-macros true]
   ))


(defn valid-transaction? [inv t]
  (not-any? neg? (vals (merge-with + t (select-keys inv (keys t))))))
(defn do-transaction [inv t] (merge-with + inv t))

(defn vec- ([a b]
            (mapv - a b))
  ([& vs] (apply mapv - vs)))
(defn vec+ ([a b]
            (mapv + a b))
  ([& vs] (apply mapv - vs)))
(defn kw->str
  ([kw] (clojure.string/replace (.-name kw) #"-" " "))
  ([n kw] (let [name (kw->str kw)]
            (if (= 1 n)
              (str (if (contains? #{"e" "y" "u" "i" "o" "a" "E" "Y" "U" "I" "O" "A"} (first name))
                     "an "
                     "a ")
                   name)
              (str n " " name "s")))))
(defn add-message [db message]
  (update db :message-log conj message))
(def crafting-recipes
  {:snowball       {}
   :large-snowball {:snowball 3}
   :snowman        {:snowball 1 :large-snowball 2 :stick 2}
   :stick          {}
   :thing-maker    {:conveyor-belt 1 :metal-gear 1 :electric-motor 1}
   })
(def store-prices {:conveyor-belt 3 :metal-gear 2 :electric-motor 6 :factory 30 :crate 2 :flower-seed 2})
(defn try-to-buy [db id]
  (let [t {:money (- (store-prices id)) id 1}]
    (if (valid-transaction? (db :inventory) t)
      (let [db (-> db
                   (update :inventory do-transaction t)
                   (add-message (str "You bought " (kw->str 1 id))))]
        (if (= id :factory)
          ;; (add-place db :factory)
          db))
      (add-message db "You don't have enough money"))))
(defn mouse-down-on-card [db k arg]
  (-> db
      (assoc-in [:drag :held-card] k)
      (assoc-in [:drag :held-card-relative-pos] (vec- (get-in db [:cards k :pos])
                                                      [(.-clientX arg) (.-clientY arg)]))))
(defn mouse-up [db] (assoc-in db [:drag :held-card] nil))
(defn mouse-move [db arg]
  (if-let [k (get-in db [:drag :held-card])]
    (assoc-in db [:cards k :pos] (vec+ (get-in db [:drag :held-card-relative-pos])
                                       [(.-clientX arg) (.-clientY arg)]))
    db))
(defn hash-set-conj [set new]
  (conj (or set #{}) new))
(defn create-item [db item-kw item-pos]
  (update-in db [:c->e->v :container item-pos item-kw] inc))
(defn create-entity [{:keys [entity-count] :as db} prototype entity-pos]
  (-> db
      (update :c->e->v #(reduce (fn [c->e->v [c v]]
                                  (assoc-in c->e->v [c entity-count] v))
                                %
                                (assoc prototype :pos entity-pos)))
      (update-in [:c->e->v :container entity-pos entity-count] inc)
      (update :entity-count inc)))
(defn create-tile [{:keys [c->e->v] :as db} {:keys [tile] :as tile-prototype} tile-pos]
  (let [{:keys [type bg-color]} tile]
    (assert (and type bg-color) (str "tile " tile " must have a color and type"))
    (update db :c->e->v #(reduce (fn [c->e->v [c v]]
                                   (assoc-in c->e->v [c tile-pos] v))
                                 %
                                 (assoc tile-prototype :container {})))))
(def view-radius 12)
(def level-radius 100)
(defn prob [p] (> p (rand)))

(defn dist [v1 v2]
  (js/Math.sqrt (reduce + (map #(* % %) (map - v1 v2)))))
(defn generate-level [db]
  (-> (reduce (fn [db pos]
                (cond (>= (dist pos [0 0]) (- level-radius view-radius)) (create-tile db p/water pos)
                      (>= (dist pos [0 0]) (- level-radius view-radius 3)) (create-tile db p/sand pos)
                      (prob 0.1)  (create-tile db p/tree pos)
                      (prob 0.03) (create-tile db p/rock pos)
                      (prob 0.01 ) (-> db
                                       (create-tile p/grass pos)
                                       (create-entity p/sheep pos))
                      (prob 0.001 ) (-> db
                                        (create-tile p/grass pos)
                                        (create-entity p/duck pos))
                      (prob 0.001 ) (-> db
                                        (create-tile p/grass pos)
                                        (create-entity p/rabbit pos))
                      (prob 0.001 ) (-> db
                                        (create-tile p/grass pos)
                                        (create-entity p/enemy pos))
                      (prob 0.01) (-> db
                                       (create-tile p/grass pos)
                                       (create-item :loot pos))
                      true        (create-tile db p/grass pos)))
              db
              (for [x (range (- level-radius) (inc level-radius))
                    y (range (- level-radius) (inc level-radius))]
                [x y]))
      (update :c->e->v #(reduce (fn [c->e->v [c kw v]]
                                  (assoc-in c->e->v [c kw] v))
                                %
                                (for [kw    (keys p/items)
                                      [c v] (p/items kw)]
                                  [c kw v])))
      (create-tile p/grass [0 0])
      (create-entity p/player [0 0])))
(defn key-up [db key]
  (if-let [i (case key
               "a"          0
               "ArrowLeft"  0
               "s"          1
               "ArrowDown"  1
               "d"          0
               "ArrowRight" 0
               "w"          1
               "ArrowUp"    1
               nil)]
    (assoc-in db [:current-dir i] 0)
    db))
(defn key-down [db key]
  (if-let [[i val]
           (case key
             "a"          [0 -1]
             "ArrowLeft"  [0 -1]
             "s"          [1 -1]
             "ArrowDown"  [1 -1]
             "d"          [0 1]
             "ArrowRight" [0 1]
             "w"          [1 1]
             "ArrowUp"    [1 1]
             nil)]
    (-> db
        (assoc-in [:move-dir i] val)
        (assoc-in [:current-dir i] val))
    db))
(defn get-player-id [{:keys [c->e->v] :as db}] (first (first (get-in c->e->v [:player]))))
(defn get-player-component [{:keys [c->e->v] :as db} component] (get-in c->e->v [component (get-player-id db)]))
(defn get-player-pos [{:keys [c->e->v] :as db}] (get-player-component db :pos))
(defn get-entities-on-relative-coord [{:keys [c->e->v] :as db} relative-coord]
  (let [t (vec+ relative-coord (get-player-pos db))]
    (conj (keys (get-in c->e->v [:container t])) t)))
(defn clamp [a b c]
  (min (max a b) c))
(defn scroll [{:keys [scroll-pos mouse-over-relative-coord] :as db} d]
  (let [l (dec (count (get-entities-on-relative-coord db mouse-over-relative-coord)))]
    (assoc db :scroll-pos (clamp 0 (+ d (clamp 0 scroll-pos l)) l))))
(defn container-transfer [c->e->v c1 c2 transaction]
  (assert (not-any? neg? (vals transaction)))
  (reduce (fn [c->e->v [item-id num]]
            (let [num-in-c1 (get-in c->e->v [:container c1 item-id])
                  c->e->v   (if (keyword? item-id)
                              c->e->v
                              (assoc-in c->e->v [:pos item-id] c2))]
              (if (>= num num-in-c1)
                (-> c->e->v
                    (update-in [:container c1] dissoc item-id)
                    (update-in [:container c2 item-id] + num))
                (-> c->e->v
                    (update-in [:container c1 item-id] - num)
                    (update-in [:container c2 item-id] + num)))))
          c->e->v
          transaction))
(defn component-values [c->e->v c es]
  (map (c->e->v c) es))
(defn entity-components [c->e->v e cs]
  (map #(% e) (map c->e->v cs)))
(defn pick-up-items-on-tile [c->e->v t player-id]
  (let [tile-contents  (keys (get-in c->e->v [:container t]))
        takeable-items (map first (filter second (map vector tile-contents (component-values c->e->v :takeable tile-contents))))]
    (container-transfer c->e->v t player-id (zipmap takeable-items (map #(get-in c->e->v [:container t %]) takeable-items)))))
(defn try-to-move [c->e->v e [dirx diry]]
  (let [pos  (get-in c->e->v [:pos e])
        m    (vec+ pos [dirx diry])
        dest (if (some zero? [dirx diry])
               (if (= :wall (get-in c->e->v [:tile m :type]))
                 pos
                 m)
               (let [[l r]      (map #(vec+ pos %) (case [dirx diry]
                                                     [1 1]   [[0 1] [1 0]]
                                                     [1 -1]  [[0 -1] [1 0]]
                                                     [-1 -1] [[0 -1] [-1 0]]
                                                     [-1 1]  [[0 1] [-1 0]]))
                     [lt mt rt] (map #(get-in c->e->v [:tile % :type]) [l m r])]
                 (cond (= :wall lt rt) pos
                       (= :wall mt)    (first (rand-nth (filter #(not= :wall (% 1)) [[l lt] [r rt]])))
                       true            m)))]
    (container-transfer c->e->v pos dest {e 1})))
(defn random-movement [c->e->v]
  (let [affected (keys (c->e->v :random-movement))]
    (reduce (fn [c->e->v e]
              (if (prob 0.1)
                (try-to-move c->e->v e (rand-nth [[0 1] [0 -1] [-1 0] [1 0]]))
                c->e->v))
            c->e->v
            affected)))
(defn enemy-movement [{:keys [c->e->v] :as db}]
  (let [player-pos (get-player-pos db)
        affected   (keys (c->e->v :enemy-movement))
        pos-es     (component-values c->e->v :pos affected)]
    (assoc db :c->e->v
           (reduce (fn [c->e->v [e pos]]
                     (if (prob 0.9)
                       (try-to-move
                         c->e->v
                         e
                         (if (prob 0.5)
                           (mapv #(clamp -1 % 1)
                                 ;; #(cond (<= % -1) -1
                                 ;;            (>= % 1)  1
                                 ;;            true      0)
                                 (vec- player-pos pos))
                           (rand-nth [[0 1] [0 -1] [-1 0] [1 0]])))
                       c->e->v))
                   c->e->v
                   (map vector affected pos-es)))))
(defn harm [c->e->v e]
  (update-in c->e->v [:hp e] dec))
(defn adjacent? [pos1 pos2]
  (every? #(<= -1 % 1) (map - pos1 pos2)))
(defn attack-player [{:keys [c->e->v] :as db}]
  (let [player-pos (get-player-pos db)
        player-id  (get-player-id db)
        affected   (keys (c->e->v :attack-player))
        pos-es     (component-values c->e->v :pos affected)]
    (assoc db :c->e->v (reduce (fn [c->e->v pos]
                                 (if (adjacent? pos player-pos)
                                   (harm c->e->v player-id)
                                   c->e->v))
                               c->e->v
                               pos-es))))
(defn player-movement [{:keys [c->e->v current-dir move-dir] :as db}]
  (let [player-id (get-player-id db)
        pos       (get-player-pos db)]
    (-> db
        (assoc :move-dir current-dir)
        (assoc :c->e->v (-> c->e->v
                            (pick-up-items-on-tile pos player-id)
                            (try-to-move player-id move-dir))))))
(def grid-side-length (inc (* 2 view-radius)))

(def black-tile [:p.aspect-square {:style {:background-color "#000000"}} " "])
(def grid-range (range (- view-radius) (inc view-radius)))
(def reverse-grid-range (reverse grid-range))
;; (stylefy/class "background-transition"
;;                {:transition "background-color 1s"})
(defn spawn-strange-creature [db]
  (-> db
      (add-message "you spawned a strange creature")
      (create-entity {:random-movement true
                      :name            "strange creature"
                      :char            (rand-nth ["🦵" "🤖" "🦋" "👁" "🐦" "🦈"])}
                     (get-player-pos db))))
(defn set-current-view [db view]
  (assoc db :current-view view))
(def toggle-reverse-time #(update % :reverse-time? not))
(defn try-to-spawn-snowman [{:keys [c->e->v] :as db}]
  (let [player-id (get-player-id db)
        [inv pos] (entity-components c->e->v player-id [:container :pos])]
    (if (valid-transaction? inv {:snowman -1})
      (-> db
          (update-in [:c->e->v :container player-id] do-transaction {:snowman -1})
          (create-entity p/snowman pos))
      (add-message db "you don't have a snowman"))))
(defn mouse-over-relative-coord [db coord]
  (assoc db :mouse-over-relative-coord coord))
(defn world-click [{:keys [scroll-pos] :as db} relative-coord]
  (let [es (get-entities-on-relative-coord db relative-coord)
        i  (clamp 0 scroll-pos (dec (count es)))
        e  (nth es i)]
    (assoc db
           :selected-entity e
           :current-view :entity-view
           )))
(def initial-db (generate-level db/default-db))
(defn tick [{:keys [c->e->v reverse-time? history current-view] :as db}]
  (if (= current-view :world-view)
    (if reverse-time?
      (if (empty? history)
        (assoc db :reverse-time? nil)
        (-> db
            (assoc :c->e->v (first history))
            (update :time dec)
            (update :history rest)
            ))
      (if (<= (get-in c->e->v [:hp (get-player-id db)]) 0)
        (-> db
            (add-message (str "You were killed. Score: " (or (get-in c->e->v [:container (get-player-id db) :loot]) 0)))
            (assoc :reverse-time? true))
        (-> db
            (update :history conj c->e->v)
            (update :time inc)
            (update :c->e->v random-movement)
            player-movement
            enemy-movement
            attack-player
            )))
    db))

(defn try-to-craft [{:keys [c->e->v] :as db} id]
  (if-let [recipe (crafting-recipes id)]
    (let [t (update (zipmap (keys recipe)
                            (map - (vals recipe))) id inc)
          player-id (get-player-id db)
          player-inv (get-in c->e->v [:container player-id])
          ]
      (if (valid-transaction? player-inv t)
        (-> db
            (update-in [:c->e->v :container player-id] do-transaction t)
            (add-message (str "You crafted " (kw->str 1 id))))
        (add-message db "You don't have the items to craft that")))
    (add-message db "You can't craft that")))

(def init #(-> db/default-db
               generate-level))

(comment
  ;; (.fillText context "hello world 🤡" 50 90 140)
  dorun
  dorun
  for

  set!
  aget
  (def canvas (js/document.getElementById "canvas"))
  (def context (.getContext (js/document.getElementById "canvas") "2d"))


  )
