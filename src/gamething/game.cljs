(ns gamething.game
  (:require
   ;; [applied-science.js-interop :as j]
   [gamething.db :as db]
   [gamething.prototypes :as p]
   ;; [schema.core :as s :include-macros true]
   )
  ;; (:require-macros [gamething.macros :refer []]
  ;;                  [helix.core :refer [;; $
  ;;                                      ]])
  )


(defn valid-transaction? [inv t]
  (not-any? neg? (vals (merge-with + t (select-keys inv (keys t))))))
(defn do-transaction [inv t] (merge-with + inv t))

(defn vec-
  ([a b]
   (mapv - a b))
  ([a b c]
   (mapv - a b c))
  ;; ([& vs] (apply mapv - vs))
  )
(defn vec+
  ([a b]
   (mapv + a b))
  ([a b c]
   (mapv + a b c))
  ;; ([& vs] (apply mapv - vs))
  )


;; (defn vec- ([a b]
;;             (mapv - a b))
;;   ([& vs] (apply mapv - vs)))
;; (defn vec+ ([a b]
;;             (mapv + a b))
;;   ([& vs] (apply mapv - vs)))
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

(defn sqrdist [v1 v2]
  (reduce + (map #(* % %) (map - v1 v2))))
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
                      (prob 0.0001 ) (-> db
                                         (create-tile p/grass pos)
                                         (create-entity p/dragon pos))
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
      ;; (create-tile p/grass [0 0])
      (create-entity p/player [0 0])))
(defn key-up [db key]
  (if-let [kw (case key
                "a"          :left
                "ArrowLeft"  :left
                "s"          :down
                "ArrowDown"  :down
                "d"          :right
                "ArrowRight" :right
                "w"          :up
                "ArrowUp"    :up
                nil)]
    (update db :pressed-keys disj kw)
    db))
(defn key-down [db key]
  (if-let [kw (case key
                "a"          :left
                "ArrowLeft"  :left
                "s"          :down
                "ArrowDown"  :down
                "d"          :right
                "ArrowRight" :right
                "w"          :up
                "ArrowUp"    :up
                nil)]
    (-> db
        (update :pressed-keys conj kw)
        (update :new-pressed-keys conj kw)
        (assoc (if (contains? #{:up :down} kw)
                 :newest-pressed-y
                 :newest-pressed-x)
               kw))
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
(defn container-transfer-entity [c->e->v c1 c2 e]
  (-> c->e->v
      (assoc-in [:pos e] c2)
      (update-in [:container c1] dissoc e)
      (assoc-in [:container c2 e] 1)))
(defn component-values [c->e->v c es]
  (map (c->e->v c) es))
(defn entity-components [c->e->v e cs]
  (map #(% e) (map c->e->v cs)))
(defn pick-up-items-on-tile [c->e->v t player-id]
  (let [tile-contents  (keys (get-in c->e->v [:container t]))
        ;; takeable-items (->> tile-contents
        ;;                     (component-values c->e->v :takeable)
        ;;                     (map vector tile-contents)
        ;;                     (filter second)
        ;;                     (map first))
        takeable-items (map first (filter second (map vector tile-contents (component-values c->e->v :takeable tile-contents))))
        ]
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
    (container-transfer-entity c->e->v pos dest e)))
(defn random-movement [c->e->v]
  (let [affected (keys (c->e->v :random-movement))]
    (reduce (fn [c->e->v e]
              (if (prob 0.1)
                (try-to-move c->e->v e (rand-nth [[0 1] [0 -1] [-1 0] [1 0]]))
                c->e->v))
            c->e->v
            affected)))
;; (defn random-movement [c->e->v]
;;   (reduce (fn [c->e->v e]
;;             (if (prob 0.1)
;;               (try-to-move c->e->v e (rand-nth [[0 1] [0 -1] [-1 0] [1 0]]))
;;               c->e->v))
;;           c->e->v
;;           (let [affected (keys (c->e->v :random-movement))]
;;             affected)))

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
                                 (vec- player-pos pos))
                           (rand-nth [[0 1] [0 -1] [-1 0] [1 0]])))
                       c->e->v))
                   c->e->v
                   (map vector affected pos-es)))))
(defn remove-all-but [c->e->v e cs]
  (reduce (fn [c->e->v c]
            (update c->e->v c dissoc e))
          c->e->v
          (filter #(and (contains? (c->e->v %) e)
                        (not (contains? (set cs) %)))
                  (keys c->e->v))))
(defn kill [{:keys [c->e->v] :as db} e]
  (let [name (get-in c->e->v [:name e])]
    (assoc (if (get-in c->e->v [:player e])
             (add-message db (str "You were killed. Click ⌛→. Score: " (or (get-in c->e->v [:container (get-player-id db) :loot]) 0)))
             db)
           :c->e->v (-> c->e->v
                        (remove-all-but e #{:player :pos :container})
                        (assoc-in [:name e] (str "dead " name))
                        (assoc-in [:char e] "☠")))))
(defn harm [{:keys [c->e->v] :as db} e n]
  (let [new-hp (- (get-in c->e->v [:combat e :hp]) n)]
    (if (<= new-hp 0)
      (kill db e)
      (assoc-in db [:c->e->v :combat e :hp] new-hp))))
(defn dragon-attack [{:keys [c->e->v time] :as db}]
  (if (= 0 (mod time 30))
    (let [affected (keys (c->e->v :dragon-attack))
          pos-es   (component-values c->e->v :pos affected)
          player-pos (get-player-pos db)
          ]
      (reduce (fn [db pos]
                (create-entity db
                               (assoc-in p/fire [:fire :dir] (mapv #(clamp -1 % 1) (vec- player-pos pos)))
                               pos))
              db
              pos-es))
    db))
(defn fire-move [{:keys [c->e->v] :as db}]
  (let [affected (keys (c->e->v :fire))
        pos-es   (component-values c->e->v :pos affected)
        dirs     (map #(get % :dir) (component-values c->e->v :fire affected))
        dests    (map vec+ dirs pos-es)
        player-pos (get-player-pos db)
        ]
    (reduce (fn [db [e pos [destx desty]]]
              (if (and (< (abs destx) level-radius)
                       (< (abs desty) level-radius)
                       (< (sqrdist player-pos [destx desty]) 1600))
                (update db :c->e->v container-transfer-entity pos [destx desty] e)
                (-> db
                    (update :c->e->v remove-all-but e [])
                    (update-in [:c->e->v :container pos] dissoc e))))
            db
            (map vector affected pos-es dests))))
(defn fire-damage [{:keys [c->e->v] :as db}]
  (let [affected   (keys (c->e->v :fire))
        pos-es     (component-values c->e->v :pos affected)
        player-pos (get-player-pos db)
        player-id  (get-player-id db)]
    (if (some #(= % player-pos) pos-es)
      (harm db player-id 500)
      db)))
;; (defn adjacent? [pos1 pos2]
;;   (every? #(<= -1 % 1) (map - pos1 pos2)))
;; (concat [1 2] [3 4])
;; (flatten [[1 2] [3 4]])
;; max-key

(defn most [f coll]
  (first (reduce (fn [a b]
                   (max-key second a b))
                 (map #(vector % (f %)) coll))))
(defn least [f coll]
  (-> [a b]
      (fn (min-key second a b))
      (reduce (map #(vector % (f %)) coll))
      first))
;; (defn least [f coll]
;;   (most (comp - f) coll))

;; (defn maximize [f coll]
;;   (reduce (fn [v v2]
;;             (if (< (f v) (f v2))
;;               v2
;;               v))
;;           (first coll)
;;           coll))

(defn get-adjacent-entities-with-component [{:keys [c->e->v] :as db} c]
  (filter #(contains? (c->e->v c) %)
          (apply concat (for [x [-1 0 1]
                              y [-1 0 1]]
                          (get-entities-on-relative-coord db [x y])))))
(defn combat [{:keys [c->e->v] :as db}]
  (let [enemies          (get-adjacent-entities-with-component db :attack-player)
        [target-enemy _] (most (fn [[e {:keys [hp damage]}]]
                                 (/ damage hp))
                               (map vector enemies (component-values c->e->v :combat enemies)))
        player-id        (get-player-id db)
        player-damage    (get-in c->e->v [:combat player-id :damage])]
    (if target-enemy
      (-> db
          (harm target-enemy player-damage)
          (harm player-id (count enemies)))
      db)))
(def tonum {:left  -1
            :right 1
            :down  -1
            :up    1
            nil    0})
(defn player-movement [{:keys [c->e->v pressed-keys new-pressed-keys newest-pressed-y newest-pressed-x] :as db}]
  (let [player-id (get-player-id db)
        pos       (get-player-pos db)
        dx        (tonum
                    (cond
                      (or (contains? new-pressed-keys :left)
                          (contains? new-pressed-keys :right)
                          (and (contains? pressed-keys :left)
                               (contains? pressed-keys :right))) newest-pressed-x
                      (contains? pressed-keys :left)             :left
                      (contains? pressed-keys :right)            :right
                      true                                       nil))
        dy        (tonum
                    (cond
                      (or (contains? new-pressed-keys :down)
                          (contains? new-pressed-keys :up)
                          (and (contains? pressed-keys :down)
                               (contains? pressed-keys :up))) newest-pressed-y
                      (contains? pressed-keys :down)          :down
                      (contains? pressed-keys :up)            :up
                      true                                    nil))
        move-dir  [dx dy]
        ]
    (if (= move-dir [0 0])
      db
      (-> db
          (assoc :new-pressed-keys #{})
          (assoc :c->e->v (-> c->e->v
                              (pick-up-items-on-tile pos player-id)
                              (try-to-move player-id move-dir)))))))
(def grid-side-length (inc (* 2 view-radius)))

;; (println (str #(let [a (inc %)]
;;                  (* a %))))
;; (println (str #(if (> 5 %)
;;                   (let [a (dec %)]
;;                     (* a a))
;;                   (let [a (inc %)]
;;                     (* a a)))))
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
(defn world-click [{:keys [scroll-pos mouse-over-relative-coord] :as db}]
  (let [es (reverse (get-entities-on-relative-coord db mouse-over-relative-coord))
        i  (clamp 0 scroll-pos (dec (count es)))
        e  (nth es i)]
    (assoc db
           :selected-entity e
           :current-view :entity-view)))
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
      (if (> (get-in c->e->v [:combat (get-player-id db) :hp]) 0)
        (-> db
            (update :history conj c->e->v)
            (update :time inc)
            (update :c->e->v random-movement)
            player-movement
            fire-move
            fire-damage
            dragon-attack
            enemy-movement
            combat
            )
        db))
    db))

(defn interact [{:keys [c->e->v] :as db} e]
  (let [[type v] (get-in c->e->v [:interact e])]
    (case type
      :message   (add-message db v)
      :give-item (-> db
                     (add-message (str "You got " (kw->str 1 v)))
                     (create-item v (get-player-id db)))
      (add-message db (str type " " v)))))

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
  for
  set!
  aget
  (def canvas (js/document.getElementById "canvas"))
  (def context (.getContext (js/document.getElementById "canvas") "2d"))


  )
