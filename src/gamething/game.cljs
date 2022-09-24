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
   [schema.core :as s :include-macros true]
   [uix.core :refer [defui $]]
   [uix.dom]
   [minicosm.core :refer [start!]]
   [minicosm.ddn :refer [render-to-canvas]]
   [rum.core :as rum :refer [defc reactive react]]
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
      (assoc-in [:cave :player-emoji] e)
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
@db
(defn create-entity [{:keys [entity-count] :as db} entity entity-pos]
  (-> db
      (update :c->e->v #(reduce (fn [c->e->v [c v]]
                                  (assoc-in c->e->v [c entity-count] v))
                                %
                                (assoc entity :pos entity-pos)))
      (update-in [:c->e->v :tile entity-pos :contents] hash-set-conj entity-count)
      (update :entity-count inc)))
(defn create-tile [{:keys [c->e->v] :as db} {:keys [tile]} tile-pos]
  (let [{:keys [type color name char]} tile]
    (assert color (str "tile " tile " must have a color"))
    (assoc-in db [:c->e->v :tile tile-pos] (assoc tile :contents #{}))))
@(sget [:pos])
(def view-radius 11)
(def level-radius 40)
(defn prob [p] (> p (rand)))
(defn generate-level [db]
  (-> (reduce (fn [db pos]
                (if (prob 0.1)
                  (create-tile db p/tree pos)
                  (create-tile db p/grass pos)))
              db
              (for [x (range (- level-radius) (inc level-radius))
                    y (range (- level-radius) (inc level-radius))]
                [x y]))
      (assoc :entity-count 0)
      (create-tile p/grass [0 0])
      (create-entity p/player [0 0])))
;; ( (-> @db
;;       (generate-level)
;;       (create-tile p/grass [0 0])
;;       (create-entity p/player [0 0])) :c->e->v)
(defsub player-pos [db] (let [[player-id _] (first (get-in db [:c->e->v :player]))]
                          (get-in db [:c->e->v :pos player-id])))
(defn generate-cave-level [{:keys [level current-dir move-dir pos] :as cave}]
  (-> cave
      (assoc :pos [0 0])
      (assoc :level (assoc (zipmap (for [x (range (- level-radius) (inc level-radius))
                                         y (range (- level-radius) (inc level-radius))]
                                     [x y])
                                   (map #(rand-nth [:wall :wall :wall :wall :wall :wall :floor :floor :floor :floor :floor :loot])
                                        (range)))
                           [0 0] :player))))
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
    (assoc-in db [:cave :current-dir i] 0)
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
        (assoc-in [:cave :move-dir i] val)
        (assoc-in [:cave :current-dir i] val))
    db))
(def diag-dirs {[1 1]   [[0 1] [1 0]]
                [1 -1]  [[0 -1] [1 0]]
                [-1 -1] [[0 -1] [-1 0]]
                [-1 1]  [[0 1] [-1 0]]})
(defn cave-movement [{:keys [cave] :as db}]
  (let [{:keys [level current-dir move-dir pos]} cave
        previous-coords                          pos
        [destination-coords destination-tile]    (let [c (vec+ move-dir pos)
                                                       t (level c)]
                                                   (if (and (= :wall t) (diag-dirs move-dir))
                                                     (let [c1 (vec+ ((diag-dirs move-dir) 0) pos)
                                                           c2 (vec+ ((diag-dirs move-dir) 1) pos)
                                                           t1 (level c1)
                                                           t2 (level c2)]
                                                       (if (= t1 t2 :wall)
                                                         [c t]
                                                         (rand-nth (filter #(not= :wall (% 1)) [[c1 t1] [c2 t2]]))))
                                                     [c t]))
        [db cave]
        (case destination-tile
          :wall   [db cave]
          :player [db cave]
          nil     [db cave]
          :floor  [db (-> cave
                          (assoc-in [:level previous-coords] :floor)
                          (assoc-in [:level destination-coords] :player)
                          (assoc-in [:pos] destination-coords))]
          :loot   [(-> db
                       (add-message "You found some loot")
                       (update :inventory do-transaction {:loot 1}))
                   (-> cave
                       (assoc-in [:level previous-coords] :floor)
                       (assoc-in [:level destination-coords] :player)
                       (assoc-in [:pos] destination-coords))])
        cave                                     (assoc cave :move-dir current-dir)
        db                                       (assoc db :cave cave)
        ]
    db))
(defn get-player-id [{:keys [c->e->v] :as db}] (first (first (get-in c->e->v [:player]))))
(defn get-player-pos [{:keys [c->e->v] :as db}] (get-in c->e->v [:pos (get-player-id db)]))
;; (get-player-pos @db)
(defn world-movement [{:keys [c->e->v current-dir move-dir] :as db}]
  (let [pos (get-player-pos db)
        previous-coords pos
        [destination-coords destination-type] (let [c (vec+ move-dir pos)
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
        db (case destination-type
             :wall  db
             nil    db
             :floor  [db (-> cave
                             (assoc-in [:level previous-coords] :floor)
                             (assoc-in [:level destination-coords] :player)
                             (assoc-in [:pos] destination-coords))]
             :loot  [(-> db
                          (add-message "You found some loot")
                          (update :inventory do-transaction {:loot 1}))
                      (-> cave
                          (assoc-in [:level previous-coords] :floor)
                          (assoc-in [:level destination-coords] :player)
                          (assoc-in [:pos] destination-coords))])
        cave                                  (assoc cave :move-dir current-dir)
        db                                    (assoc db :cave cave)
        ]
    db))

(defevent mouse-over-tile [db t]
  (assoc db :popup-text (get-in db [:c->e->v :tile t :name])))
(defn component-values [c->e->v c es]
  (map (c->e->v c) es))
(defn entity-components [c->e->v e cs]
  (map #(% e) (map c->e->v cs) ))
(defsub tile-visual-data [{:keys [c->e->v]} t]
  (let [{:keys [color char contents]} (get-in c->e->v [:tile t])
        entity-chars                  (component-values c->e->v :char contents)]
    [color char contents entity-chars]))
(defsub tile-visual [t] [[[color char contents entity-chars]] [(tile-visual-data t)]]
  ^{:key t} [:p {:style {:background-color color}
                 :on-mouse-over #(mouse-over-tile t)}
             (or char " ")
             ])




(def pos [0 0])
(def posx 0)
(def posy 0)
(def grid-side-length (inc (* 2 view-radius)))
(defsub world-view [[posx posy]] [tile-visuals (for [y (reverse (range (- posy view-radius) (+ 1 posy view-radius)))
                                                     x (range (- posx view-radius) (+ 1 posx view-radius))]
                                                 (tile-visual [x y]))]
  [:div.grid.aspect-square.text-3xl.select-none.m-4
   {:style {:grid-template-columns (str "repeat(" grid-side-length ", 1fr)")}}
   (doall (seq tile-visuals))]
  )

;; (defn world-view []
;;   (let [[posx posy] [(sget [:pos 0]) (sget [:pos 1])]]
;;     (fn []
;;       [:div.grid.aspect-square.text-3xl.select-none.m-4
;;        {:style {:grid-template-columns (str "repeat(" grid-side-length ", 1fr)")}}
;;        "a"
;;        posx
;;        posy
;;        (doall (for [y (reverse (range (- posy view-radius) (+ 1 posy view-radius)))
;;                     x (range (- posx view-radius) (+ 1 posx view-radius))]
;;                 (tile-visual [x y])))
;;        ])
;;     ))
(defsub cave-view [] [[{:keys [level pos player-emoji]}] [(sget [:cave])]]
  [:div.grid.aspect-square.text-3xl.select-none.m-4
   {:style {:grid-template-columns (str "repeat(" grid-side-length ", 1fr)")}}
   (doall (for [y (map - (range (- view-radius) (inc view-radius)))
                x (range (- view-radius) (inc view-radius))]
            ^{:key [x y]} [:p.bg-orange-500  ;; {:class (str "bg-orange-" (rand-nth ["200" "400" "600" ]))}
                           (case (level (vec+ pos [x y]))
                             :player player-emoji
                             :wall   "#"
                             :loot   "💰"
                             " ")]))])

(defevent tick [db]
  (let [n (get-in db [:inventory :thing-maker])]
    (cond-> db
      (pos? n)                      (update :inventory do-transaction {:thing n})
      (= :cave (db :current-place)) (cave-movement)
      ;; (->
      ;; (cave-movement)
      ;; (update :cave make-tiles))
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
    :cave      cave-view
    ))
(defevent go-to-place [db place]
  (assoc (if (= place :cave)
           (update db :cave generate-cave-level)
           db)
         :current-place place))
(defsub description-popup [db] [[text] [(sget [:popup-text])]]
  [:p text])
(defsub sidebar [] [[places message-log-view] [(places) (message-log-view)]]
  [:div.flex.flex-col
   (for [place places]
     ^{:key place} [:button.bg-gray-300.hover:bg-green-300.py-1.transition.ease-in-out
                    {:on-click #(go-to-place place)}
                    (kw->str place)])
   message-log-view]
  )
(defn view []
  ;; [:title "aaaaaaaaaaaaa"]
  [:div.h-screen.w-screen.flex.flex-row.bg-gray-600.font-mono.text-blue-400.text-lg.overflow-hidden
   {:onMouseUp     #(mouse-up)
    :on-mouse-move #(mouse-move %1)
    }
   ;; [:p @(s :all)]
   [:div.w-72.flex-none ;; .overflow-hidden
    @(sidebar)]
   [:div  @(world-view (get-player-pos @db)) ]
   ;; @(@(current-place) )
   ;; (fn [] @(home) )
   ;; @(crafting-menu)
   ;; [:div @(@(current-place) ) ]
   ])

;; [:div (@(current-place) ) ]
;; (type @(@(current-place) ))


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
