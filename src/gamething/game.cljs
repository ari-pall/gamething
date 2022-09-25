(ns gamething.game
  (:require
   ;; [re-com.core :as re :refer [at]]
   [re-frame.core :as rf :refer [reg-sub reg-event-db reg-event-fx reg-fx dispatch subscribe]]
   [goog.events]
   ;; [spade.util]
   ;; [spade.core]
   ;; [spade.runtime]
   [reagent.dom :as rdom]
   [gamething.db :as db]
   [gamething.prototypes :as p]
   [day8.re-frame.tracing :refer-macros [fn-traced]]
   ;; [schema.core :as s :include-macros true]
   ;; [uix.core :refer [defui $]]
   ;; [uix.dom]
   [stylo.core :refer [c]]
   ;; [minicosm.core :refer [start!]]
   ;; [minicosm.ddn :refer [render-to-canvas]]
   ;; [rum.core :as rum :refer [defc reactive react]]
   )
  (:require-macros [gamething.macros :refer [defsub defevent]]))

(comment
  (render-to-canvas 4 4 (.getElementById js/document "app"))
  )

(defn valid-transaction? [inv t] (not-any? neg? (vals (merge-with + t (select-keys inv (keys t))))))
(defn do-transaction [inv t] (merge-with + inv t))

(def db re-frame.db/app-db)
;; (reg-event-db :init (fn-traced [_ _]
;;                                db/default-db))

(defsub inventory :inventory)
;; (reg-sub :inventory :-> :inventory)
;; (reg-event-db :do (fn [db [_ f]] (f db)))
(defsub sget [db path] (get-in db path))
(defsub none [] [[u] [(sget [:cake])]] #(do nil))
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
;; try-to-craft has to operate on db because it modifies the inventory and also the message-log
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
(defsub crafting-menu [] [[inv] [(inventory)]]
  [:div.p-7.flex.flex-col.w-80.space-y-2
   [:p.text-yellow-400.p-1 "You have:"]
   (doall (for [[id recipe] crafting-recipes]
            (let [num (or (get inv id) 0)]
              ^{:key id} [:button.rounded-full.bg-gray-300.hover:bg-green-300.py-1
                          {:on-click #(try-to-craft id)}
                          (kw->str num id)])) )])
(defsub inventory-menu [] [[inv] [(inventory)]]
  [:div.p-7.flex-col.flex.w-80.space-y-2
   [:p.text-yellow-400.p-1 "You have:"]
   (for [[id num] inv]
     ^{:key id} [:p (kw->str num id)])])
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
(defsub store-menu [] [_ [(none)]]
  [:div.p-7.flex-col.flex.w-80.space-y-2
   [:p.text-yellow-400.p-1 "You can buy:"]
   (for [[id price] store-prices]
     ^{:key id} [:button.rounded-full.bg-gray-300.hover:bg-green-300.py-1
                 {:on-click  #(try-to-buy id)}
                 (str (kw->str 1 id) " for " price " moneys")])])
(defsub all str)
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
(defsub cards :cards)
(defsub card [k] [[cards] [(cards)]] (cards k))
(defsub cards-view [] [[cards] [(cards)]]
  [:div.select-none.overflow-hidden
   (doall (for [[k card] cards]
            (let [{[x y] :pos text :text} card]
              ^{:key k} [:div.absolute.px-5.py-8.rounded-lg.bg-gray-300  ;; .hover:border-solid.hover:border-4.hover:border-black
                         {:style         {:left x :top y}
                          :onMouseDown   #(mouse-down-on-card k %1)}
                         text])))])
(defsub message-log :message-log)
(defsub message-log-view [] [[log] [(message-log)]]
  [:div.flex.flex-col-reverse.bg-stone-700.text-yellow-200.text-sm.overflow-auto.h-full
   (map-indexed (fn [i message]
                  ^{:key i} [:p message])
                log)])
(defsub places :places)
(defsub item-count [item] [[inventory] [(inventory)]] (inventory item))
(defsub factory-objects [db] (get-in db [:factory :objects]))
(def factory-placeables [:thing-maker :conveyor-belt :crate])
(defn factory []
  [:div.flex.flew-col.select-none.overflow-hidden
   [:div.flex.flex-row.m-8.space-x-6.h-16
    (for [[id text] (cons [:remove "remove"]
                          (for [[id num] (select-keys @(subscribe [:inventory]) factory-placeables)]
                            [id (str (kw->str num id))]))]
      ^{:key id} [:button.rounded-full.bg-gray-300.hover:bg-green-300.p-3.h-12
                  {:onMouseDown #(dispatch [:do (fn [db] (assoc-in % [:factory :held-object] id))])}
                  text])]
   ])
(defevent set-emoji [db e]
  (-> db
      (assoc :player-emoji e)
      ;; (assoc-in [:cave :player-emoji] e)
      (add-message (str "You picked " e))))
(defsub home [] [_ [(none)]]
  [:div.m-6.space-x-2.text-3xl
   [:p.text-yellow-400.m-3 "Choose your emoji"]
   (for [e ["🤓" "🐸" "😠" "🤡" "😳" "😔" "😐" "🤤" "🙃" "🙂" ]]
     ^{:key e} [:button.link {:on-click #(set-emoji e)}
                e])])
(defsub garden [] [_ [(none)]]
  [:p.text-3xl "garden"])
(defn hash-set-conj [set new]
  (conj (or set #{}) new))
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
                                 (assoc tile-prototype :container {})
                                 ))))
(def view-radius 12)
(def level-radius 60)
(defn prob [p] (> p (rand)))

(defn generate-level [db]
  (-> (reduce (fn [db pos]
                (cond (prob 0.1) (create-tile db p/tree pos)
                      (prob 0.03) (create-tile db p/rock pos)
                      true (create-tile db p/grass pos)))
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
      (assoc :entity-count 0)
      (create-tile p/grass [0 0])
      (create-entity p/player [0 0])))
(defsub player-pos [db] (let [[player-id _] (first (get-in db [:c->e->v :player]))]
                          (get-in db [:c->e->v :pos player-id])))
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
(def diag-dirs {[1 1]   [[0 1] [1 0]]
                [1 -1]  [[0 -1] [1 0]]
                [-1 -1] [[0 -1] [-1 0]]
                [-1 1]  [[0 1] [-1 0]]})
(defn entities-on-tile [{:keys [c->e->v] :as db} t]
  (keys (get-in c->e->v [:container t])))
(defn get-player-id [{:keys [c->e->v] :as db}] (first (first (get-in c->e->v [:player]))))
(defn get-player-pos [{:keys [c->e->v] :as db}] (get-in c->e->v [:pos (get-player-id db)]))
(defsub player-pos-sub [db] (get-player-pos db))
;; (get-player-pos @db)
;; ( @db :move-dir)
(defn container-add [c->e->v container e]
  (update-in (if (keyword? e)
               c->e->v
               (assoc-in c->e->v [:pos e] container)
               ) [:container container e] inc))
(defn container-remove [c->e->v container e]
  (update-in (if (keyword? e)
               c->e->v
               (assoc-in c->e->v [:pos e] container)
               ) [:container container e] inc))
;; (defn pick-up-item [{:keys [c->e->v] :as db} item]
;;   )
(defn world-movement [{:keys [c->e->v current-dir move-dir] :as db}]
  (let [player-id       (get-player-id db)
        pos             (get-player-pos db)
        previous-coords pos
        [destination destination-type]
        (let [c (vec+ move-dir pos)
              t (get-in c->e->v [:tile c :type])]
          (if (and (= :wall t) (diag-dirs move-dir))
            (let [c1 (vec+ ((diag-dirs move-dir) 0) pos)
                  c2 (vec+ ((diag-dirs move-dir) 1) pos)
                  t1 (get-in c->e->v [:tile c1 :type])
                  t2 (get-in c->e->v [:tile c2 :type])]
              (if (= t1 t2 :wall)
                [c t]
                (rand-nth (filter #(not= :wall (% 1)) [[c1 t1] [c2 t2]]))))
            [c t]))
        ]
    (-> (case destination-type
          :wall  db
          nil    db
          :floor (assoc db :c->e->v (-> c->e->v
                                        (update-in [:container pos] dissoc player-id)
                                        (update-in [:container destination player-id] inc)
                                        (assoc-in [:pos player-id] destination))))
        (assoc :move-dir current-dir))))
(defn component-values [c->e->v c es]
  (map (c->e->v c) es))
(defn entity-components [c->e->v e cs]
  (map #(% e) (map c->e->v cs) ))
(defevent mouse-over-tile [{:keys [c->e->v] :as db} t]
  (let [es    (conj (keys (get-in c->e->v [:container t])) t)
        chars (component-values c->e->v :char es)
        names (component-values c->e->v :name es)]
    (-> db
        (add-message (apply str " that's "(map #(str " " %1 " " %2 " " %3) es chars names)))
        (assoc :popup-text (get-in c->e->v [:name t ])))))
(def grid-side-length (inc (* 2 view-radius)))
(defn make-tiles [c->e->v [posx posy]]
  (for [y (reverse (range (- posy view-radius) (+ 1 posy view-radius)))
        x (range (- posx view-radius) (+ 1 posx view-radius))]
    (let [t                  [x y]
          {:keys [bg-color]} (get-in c->e->v [:tile t])
          es                 (conj (keys (get-in c->e->v [:container t])) t)
          chars              (component-values c->e->v :char es)]
      ^{:key t} [:p {:style         {:background-color bg-color}
                     :on-mouse-over #(mouse-over-tile t)}
                 (rand-nth chars)
                 ;; (rand-nth (filter identity (seq chars)))
                 ])))
(defevent tick [{:keys [c->e->v] :as db}]
  (let [n (get-in db [:inventory :thing-maker])]
    (cond-> db
      (pos? n) (update :inventory do-transaction {:thing n})
      true     (world-movement)
      true (assoc :tiles (make-tiles c->e->v (get-player-pos db)))
      )))
(defsub current-place [] [[place] [(sget [:current-place])]]
  (case place
    :factory   factory
    :cards     cards-view
    :crafting  crafting-menu
    :inventory inventory-menu
    :store     store-menu
    :home      home
    :garden    garden
    ;; :cave      cave-view
    ))
(defevent go-to-place [db place]
  (assoc db ;; (if (= place :cave)
         ;;   (update db :cave generate-cave-level)
         ;;   db)
         :current-place place))
(defsub description-popup [] [[text] [(sget [:popup-text])]]
  [:p text])
;; (def sidebar-style (c :flex))
(defsub sidebar [] [[places message-log-view] [(places) (message-log-view)]]
  [:div.flex.flex-col
   (for [place places]
     ^{:key place} [:button.bg-gray-300.hover:bg-green-300.py-1.transition.ease-in-out
                    {:on-click #(go-to-place place)}
                    (kw->str place)])
   message-log-view]
  )

(defn view []
  (let [;; [posx posy] @(player-pos-sub)
        ]
    [:div.h-screen.w-screen.flex.flex-row.bg-gray-600.font-mono.text-red-200.text-lg.overflow-hidden
     {:onMouseUp     #(mouse-up)
      :on-mouse-move #(mouse-move %1)
      }
     ;; [:p @(s :all)]
     [:div.w-72.flex-none ;; .overflow-hidden
      @(sidebar)]
     [:div.grid.text-3xl.select-none.m-4 ;; .aspect-square
      {:style {:grid-template-columns (str "repeat(" grid-side-length ", 1fr)")}}
      @(sget [:tiles])
      ;; (doall (for [y (reverse (range (- posy view-radius) (+ 1 posy view-radius)))
      ;;              x (range (- posx view-radius) (+ 1 posx view-radius))]
      ;;          ;; "a"
      ;;          @(tile-visual [x y])
      ;;          ))
      ]
     ]
    )
  )

(defevent init [_] (-> db/default-db
                       generate-level) ;; (fn-traced [_ _]
                          ;; db/default-db)
  )

(comment
  (render (<> "You have"
              (for [item all-items]
                [(str (inventory item) " " item) (actions inventory item)])
              ))

  )
