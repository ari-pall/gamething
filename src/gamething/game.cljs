(ns gamething.game
  (:require
   [goog.events]
   ;; [spade.util]
   ;; [spade.core]
   ;; [spade.runtime]
   [gamething.db :as db]
   [gamething.prototypes :as p]
   ;; [schema.core :as s :include-macros true]
   ;; [stylo.core :refer [c]]
   ;; [dumdom.core :refer [defcomponent]]
   ;; [dumdom.dom :refer [div p button]]
   [promesa.core :as promesa]
   ;; [minicosm.core :refer [start!]]
   ;; [minicosm.ddn :refer [render-to-canvas]]
   [goog.async.nextTick]
   [cljs.core.async :refer [chan alts! put! go <! >! timeout close!]]
   [stylefy.core :as stylefy :refer [use-style]]
   [stylefy.generic-dom :as stylefy-generic-dom]
   [helix.core :refer [defnc <>]]
   [helix.hooks :as hooks]
   [helix.dom :as d]
   ["react-dom/client" :as rdom]
   )
  (:require-macros [gamething.macros :refer [defevent]]
                   [helix.core :refer [$]]))


;; (stylefy/keyframes "simple-animation"
;;                        [:from
;;                         {:opacity 0}]
;;                        [:to
;;                         {:opacity 1}])


chan
promesa/promise
<!
>!
put!
cljs.core.async/untap-all*
go
cljs.core.async/alts!
cljs.core.async/timeout
goog.async.nextTick

(def db (atom nil))

(def setter (atom nil))
(comment
  (render-to-canvas 4 4 (.getElementById js/document "app"))
  )

(defn valid-transaction? [inv t] (not-any? neg? (vals (merge-with + t (select-keys inv (keys t))))))
(defn do-transaction [inv t] (merge-with + inv t))

;; (defsub inventory :inventory)
;; (defsub sget [db path] (get-in db path))
;; (defsub none [] [[u] [(sget [:cake])]] #(do nil))
(defn vec- [& vs] (apply mapv - vs))
(defn vec+ [& vs] (apply mapv + vs))
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
(defevent add-message! [db message] (add-message db message))
(def crafting-recipes {:snowball       {}
                       :large-snowball {:snowball 3}
                       :snowman        {:snowball 1 :large-snowball 2 :stick 2}
                       :stick          {}
                       :thing-maker {:conveyor-belt 1 :metal-gear 1 :electric-motor 1}
                       })
(defevent try-to-craft [db id]
  (if-let [recipe (crafting-recipes id)]
    (let [t (update (zipmap (keys recipe)
                            (map - (vals recipe))) id inc)]
      (if (valid-transaction? (db :inventory) t)
        (-> db
            (update :inventory do-transaction t)
            (add-message (str "You crafted " (kw->str 1 id))))
        (add-message db "You don't have the items to craft that")))
    (add-message db "You can't craft that")))
;; (defsub crafting-menu [] [[inv] [(inventory)]]
;;   [:div.p-7.flex.flex-col.w-80.space-y-2
;;    [:p.text-yellow-400.p-1 "You have:"]
;;    (doall (for [[id recipe] crafting-recipes]
;;             (let [num (or (get inv id) 0)]
;;               ^{:key id} [:button.rounded-full.bg-gray-300.hover:bg-green-300.py-1
;;                           {:on-click #(try-to-craft id)}
;;                           (kw->str num id)])) )])
;; (defsub inventory-menu [] [[inv] [(inventory)]]
;;   [:div.p-7.flex-col.flex.w-80.space-y-2
;;    [:p.text-yellow-400.p-1 "You have:"]
;;    (for [[id num] inv]
;;      ^{:key id} [:p (kw->str num id)])])
(def store-prices {:conveyor-belt 3 :metal-gear 2 :electric-motor 6 :factory 30 :crate 2 :flower-seed 2})
(defn add-place [db place] (update db :places conj place))
(defevent try-to-buy [db id]
  (let [t {:money (- (store-prices id)) id 1}]
    (if (valid-transaction? (db :inventory) t)
      (let [db (-> db
                   (update :inventory do-transaction t)
                   (add-message (str "You bought " (kw->str 1 id))))]
        (if (= id :factory)
          (add-place db :factory)
          db))
      (add-message db "You don't have enough money"))))
;; (defsub store-menu [] [_ [(none)]]
;;   [:div.p-7.flex-col.flex.w-80.space-y-2
;;    [:p.text-yellow-400.p-1 "You can buy:"]
;;    (for [[id price] store-prices]
;;      ^{:key id} [:button.rounded-full.bg-gray-300.hover:bg-green-300.py-1
;;                  {:on-click  #(try-to-buy id)}
;;                  (str (kw->str 1 id) " for " price " moneys")])])
;; (defsub all str)
(defevent mouse-down-on-card [db k arg]
  (-> db
      (assoc-in [:drag :held-card] k)
      (assoc-in [:drag :held-card-relative-pos] (vec- (get-in db [:cards k :pos])
                                                      [(.-clientX arg) (.-clientY arg)]))))
(defevent mouse-up [db] (assoc-in db [:drag :held-card] nil))
;; (reg-event-db :mouse-up (fn [db _] (assoc-in db [:drag :held-card] nil)))
(defevent mouse-move [db arg]
  (if-let [k (get-in db [:drag :held-card])]
    (assoc-in db [:cards k :pos] (vec+ (get-in db [:drag :held-card-relative-pos])
                                       [(.-clientX arg) (.-clientY arg)]))
    db))
;; (defsub cards :cards)
;; (defsub card [k] [[cards] [(cards)]] (cards k))
;; (defsub cards-view [] [[cards] [(cards)]]
;;   [:div.select-none.overflow-hidden
;;    (doall (for [[k card] cards]
;;             (let [{[x y] :pos text :text} card]
;;               ^{:key k} [:div.absolute.px-5.py-8.rounded-lg.bg-gray-300  ;; .hover:border-solid.hover:border-4.hover:border-black
;;                          {:style         {:left x :top y}
;;                           :onMouseDown   #(mouse-down-on-card k %1)}
;;                          text])))])
;; (defsub message-log :message-log)
;; (defsub places :places)
;; (defsub item-count [item] [[inventory] [(inventory)]] (inventory item))
;; (defsub factory-objects [db] (get-in db [:factory :objects]))
(def factory-placeables [:thing-maker :conveyor-belt :crate])
;; (defn factory []
;;   [:div.flex.flew-col.select-none.overflow-hidden
;;    [:div.flex.flex-row.m-8.space-x-6.h-16
;;     (for [[id text] (cons [:remove "remove"]
;;                           (for [[id num] (select-keys @(subscribe [:inventory]) factory-placeables)]
;;                             [id (str (kw->str num id))]))]
;;       ^{:key id} [:button.rounded-full.bg-gray-300.hover:bg-green-300.p-3.h-12
;;                   {:onMouseDown #(dispatch [:do (fn [db] (assoc-in % [:factory :held-object] id))])}
;;                   text])]
;;    ])
(defevent set-emoji [db e]
  (-> db
      (assoc :player-emoji e)
      ;; (assoc-in [:cave :player-emoji] e)
      (add-message (str "You picked " e))))
;; (defsub home [] [_ [(none)]]
;;   [:div.m-6.space-x-2.text-3xl
;;    [:p.text-yellow-400.m-3 "Choose your emoji"]
;;    (for [e ["🤓" "🐸" "😠" "🤡" "😳" "😔" "😐" "🤤" "🙃" "🙂" ]]
;;      ^{:key e} [:button.link {:on-click #(set-emoji e)}
;;                 e])])
;; (defsub garden [] [_ [(none)]]
;;   [:p.text-3xl "garden"])
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
(def level-radius 60)
(defn prob [p] (> p (rand)))

(defn generate-level [db]
  (-> (reduce (fn [db pos]
                (cond (prob 0.1)  (create-tile db p/tree pos)
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
      ;; (assoc :entity-count 0)
      (create-tile p/grass [0 0])
      (create-entity p/player [0 0])))
(defevent key-up [db key]
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
(defevent key-down [db key]
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
(defn entities-on-tile [{:keys [c->e->v] :as db} t]
  (keys (get-in c->e->v [:container t])))
(defn get-player-id [{:keys [c->e->v] :as db}] (first (first (get-in c->e->v [:player]))))
(defn get-player-pos [{:keys [c->e->v] :as db}] (get-in c->e->v [:pos (get-player-id db)]))
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
  (let [tile-contents (keys (get-in c->e->v [:container t]))
        takeable-items (map first (filter second (map vector tile-contents (component-values c->e->v :takeable tile-contents))))]
    (container-transfer c->e->v t player-id (zipmap takeable-items (map #(get-in c->e->v [:container t %]) takeable-items)))))
(defn random-movement [c->e->v]
  (let [affected (keys (c->e->v :random-movement))
        pos-es (component-values c->e->v :pos affected)]
    (reduce (fn [c->e->v [e pos]]
              (let [destination (vec+ pos (rand-nth [[1 0] [-1 0] [0 1] [0 -1]]))]
                (if (and (prob 0.3) (= :floor (get-in c->e->v [:tile destination :type])))
                  (container-transfer c->e->v pos destination {e 1})
                  c->e->v)))
            c->e->v
            (map vector affected pos-es))))

(def diag-dirs {[1 1]   [[0 1] [1 0]]
                [1 -1]  [[0 -1] [1 0]]
                [-1 -1] [[0 -1] [-1 0]]
                [-1 1]  [[0 1] [-1 0]]})
(defn world-movement [{:keys [c->e->v current-dir move-dir] :as db}]
  (let [player-id       (get-player-id db)
        pos             (get-player-pos db)
        dest (if (diag-dirs move-dir)
               (let [l  (vec+ ((diag-dirs move-dir) 0) pos)
                     m  (vec+ move-dir pos)
                     r  (vec+ ((diag-dirs move-dir) 1) pos)
                     ;; [lt mt rt] (map #(get-in c->e->v [:tile % :type]) [l m r])
                     lt (get-in c->e->v [:tile l :type])
                     mt (get-in c->e->v [:tile m :type])
                     rt (get-in c->e->v [:tile r :type])]
                 (cond (= :wall lt rt) pos
                       (= :wall mt)    (first (rand-nth (filter #(not= :wall (% 1)) [[l lt] [r rt]])))
                       true            m))
               (let [dest (vec+ move-dir pos)]
                 (if (= :wall (get-in c->e->v [:tile dest :type]))
                   pos
                   dest)))
        ]
    (-> db
        (assoc :c->e->v (-> c->e->v
                            (container-transfer pos dest {player-id 1})
                            (pick-up-items-on-tile dest player-id)))
        (assoc :move-dir current-dir))))
(defevent mouse-over-tile [{:keys [c->e->v] :as db} t]
  (assoc db :mouse-on-tile t))
(def grid-side-length (inc (* 2 view-radius)))

(def black-tile [:p.aspect-square {:style {:background-color "#000000"}} " "])
(def grid-range (range (- view-radius) (inc view-radius)))
(def reverse-grid-range (reverse grid-range))
(defn make-tiles [c->e->v [posx posy]]
  (for [y reverse-grid-range
        x grid-range
        ]
    ;; (if visible?
    ;;   ...
    ;;   black-tile
    ;;   )
    (let [t                  [(+ x posx) (+ y posy)]
          {:keys [bg-color]} (get-in c->e->v [:tile t])
          ;; es                 (conj (keys (get-in c->e->v [:container t])) t)
          ;; chars              (component-values c->e->v :char es)
          ]
      (d/p {& {:key   t
               :style {:background-color bg-color}}}
           ;; {:key t
           ;;  :style {:background-color bg-color}}
           (get-in c->e->v [:char (last (conj (keys (get-in c->e->v [:container t])) t))])
           ;; (last chars)
           ))))
;; (stylefy/class "background-transition"
;;                {:transition "background-color 1s"})
(stylefy.core/class "grid-style" {:grid-template-columns (str "repeat(" grid-side-length ", 1fr)")
                                  :grid-template-rows    (str "repeat(" grid-side-length ", 1fr)")
                                  })
(defevent toggle-reverse-time #(update % :reverse-time? not))
(defevent tick [{:keys [c->e->v reverse-time? time history] :as db}]
  (do
  ;; (js/console.log (get-player-pos db))
  (if reverse-time?
      (if (empty? history)
        (assoc db :reverse-time? nil)
        (-> db
            (assoc :c->e->v (first history))
            (update :history rest)
            (assoc :tiles (make-tiles c->e->v (get-player-pos db)))))
      (-> db
          (update :history conj c->e->v)
          (update :c->e->v random-movement)
          (world-movement)
          (assoc :tiles (make-tiles c->e->v (get-player-pos db))))))
    )
;; (defsub current-place [] [[place] [(sget [:current-place])]]
;;   (case place
;;     ;; :factory   factory
;;     :cards     cards-view
;;     :crafting  crafting-menu
;;     :inventory inventory-menu
;;     :store     store-menu
;;     :home      home
;;     :garden    garden
;;     ;; :cave      cave-view
;;     ))
;; (tick)
(defevent go-to-place [db place]
  (assoc db :current-place place))
(defevent spawn-strange-creature [db] (-> db
                                          (add-message "you spawned a strange creature")
                                          (create-entity {:random-movement true
                                                             :name            "strange creature"
                                                             :char            (rand-nth ["🦵" "🤖" "🦋" "👁" "👿" "🐦" "🦈"])}
                                                         (get-player-pos db))))

"🧰
👜
🪄
📓
🔍"
(defnc message-log-view [{:keys [message-log]}]
  (d/div {:class "flex flex-col-reverse bg-stone-700 text-yellow-200 text-sm overflow-auto h-full"}
         (map-indexed (fn [i message]
                        (d/p {:key i} message)
                        )
                message-log))
  )
(defnc sidebar [{:keys [reverse-time? message-log] :as props}]
  ;; (js/console.log props)
  (d/div {:class "flex flex-col"}
         (d/button {:class    "bg-gray-300 hover:bg-green-300 py-1 transition ease-in-out text-red-800 h-20 w-20 text-3xl"
                    :on-click toggle-reverse-time}
                   (if reverse-time?
                     "←⌛"
                     "⌛→"))
         (d/button {:class    "bg-gray-300 hover:bg-green-300 py-1 transition ease-in-out text-green-900"
                    :on-click spawn-strange-creature}
                   "spawn creature")
         ($ message-log-view {& {:message-log message-log}}))
  )
(defnc desc-popup [{:keys [mouse-over-relative-coord c->e->v] :as db}]
  (d/div {:class "flex flex-col bg-gray-600 font-mono text-red-200 text-lg"}
         (let [t (vec+ mouse-over-relative-coord (get-player-pos db))]
           (for [e (conj (keys (get-in c->e->v [:container t])) t)]
             (let [[char name] (entity-components c->e->v e [:char :name])]
               (d/p {& {:key e}} ;; char " " name
                    char " " name))))))
(defevent mouse-over-relative-coord [db coord]
  (assoc db :mouse-over-relative-coord coord))
(defnc overlay-grid []
  (d/div {:class "grid grid-style select-none"
          :style {:position "fixed"
                  :left     "30vh"
                  :width    "100vh"
                  :height   "100vh"
                  }}
         (for [y reverse-grid-range
               x grid-range]
           (d/div {:class         "bg-white opacity-0 hover:opacity-20 hover:animate-pulse"
                   :key           [x y]
                   :on-mouse-over #(mouse-over-relative-coord [x y])
                   }))))
(def overlay-grid-element ($ overlay-grid))
(def initial-db (generate-level db/default-db))
(defnc view []
  (let [[{:keys [tiles c->e->v reverse-time?] :as db} set-state!] (hooks/use-state initial-db)]
    (defonce aa (reset! setter set-state!))
    (d/div {:onMouseUp     #(mouse-up)
            :on-mouse-move #(mouse-move %1)
            :class         "h-screen w-screen bg-gray-600 font-mono text-red-200 text-lg overflow-hidden"
            }
           (d/div {:class "flex-none"
                   :style {:position "fixed"
                           :height   "100vh"
                           :width    "30vh"}

                   }
                  ($ sidebar {& (select-keys db [:reverse-time? :message-log])}))
           ;; (js/console.log (first tiles))
           (d/div {:class "grid grid-style text-3xl select-none"
                   :style {:position "fixed"
                           :left     "30vh"
                           :width    "100vh"
                           :height   "100vh"
                           }
                   }
                  tiles)
           overlay-grid-element
           (d/div {:style {:position "fixed"
                           :left      "130vh"
                           :height    "100vh"
                           }}
                  ($ desc-popup {& (select-keys db [:mouse-over-relative-coord :c->e->v])})
                  )
           )))

(defevent init #(-> db/default-db
                    generate-level))

(comment
  (.fillText context "hello world 🤡" 50 90 140)
  dorun
  dorun
  for

  set!
  aget
  (def canvas (js/document.getElementById "canvas"))
  (def context (.getContext (js/document.getElementById "canvas") "2d"))
  (render (<> "You have"
              (for [item all-items]
                [(str (inventory item) " " item) (actions inventory item)])
              ))

  )
