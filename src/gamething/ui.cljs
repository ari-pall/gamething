(ns gamething.ui
  (:require
   [goog.events]
   ;; [spade.util]
   ;; [spade.core]
   ;; [spade.runtime]
   ;; [applied-science.js-interop :as j]
   [gamething.db :as db]
   [gamething.game :as game :refer [view-radius kw->str initial-db get-player-pos crafting-recipes get-player-id clamp entity-components component-values get-player-component]]
   [gamething.prototypes :as p]
   ;; [schema.core :as s :include-macros true]
   ;; [stylo.core :refer [c]]
   [stylefy.core :as stylefy :refer [use-style]]
   [stylefy.generic-dom :as stylefy-generic-dom]
   [helix.core :refer [defnc <> $]]
   [helix.hooks :as hooks]
   [helix.dom :as d]
   )
  ;; (:require-macros [gamething.macros :refer []]
  ;;                  [helix.core :refer [;; $
  ;;                                      ]])
  )

;; (def x (+ '() '()))
;; (def x (+ 3 '()))
;; (def a baaaaaaaaaa)
(def setter (atom nil))
(defn ! [f & args]
  (@setter (fn [db] (apply f db args))))
(def gotten (atom nil))
(defn path [& args]
  (! #(do (reset! gotten (get-in % args))
          %))
  @gotten)

;; (stylefy/keyframes "simple-animation"
;;                        [:from
;;                         {:opacity 0}]
;;                        [:to
;;                         {:opacity 1}])
(defnc message-log-view [{:keys [message-log]}]
  (d/div {:class "flex flex-col-reverse bg-stone-700 text-yellow-200 text-sm overflow-auto h-full"}
         (map-indexed (fn [i message]
                        (d/p {& {:key i}} message))
                      message-log)))
"active:bg-green-600"
;; (defnc grid-button [{:keys [on-click children]}]
;;   (d/button {:class "hover:text-4xl h-14 focus:outline-none hover:bg-green-300 active:bg-green-400"
;;              &      {:on-click on-click}}
;;             children))

(defnc button [:keys [on-click children]]
  (d/button {:class "focus:outline-none text-black bg-gray-300 hover:bg-green-300 active:bg-green-400"
             &      {:on-click on-click}}
            children))
(defnc sidebar [{:keys [reverse-time? message-log]}]
  (d/div
    {:class "flex flex-col"}
    ;; (d/p {:class "border-2 border-solid"} "aaaaa")
    (d/div
      {:class "grid grid-cols-4 auto-rows-fr select-none bg-gray-300 text-3xl text-black"}
      ($ button {:on-click #(! game/set-current-view :world-view)} "👀")
      ;; ($ button {:on-click #(set-current-view :abilities-view)} "🪄")
      ($ button {:on-click #(! game/try-to-spawn-snowman)} "⛄")
      ;; ($ button {:on-click spawn-strange-creature} "🔎")
      ($ button {:on-click #(! game/set-current-view :inventory-view)} "👜")
      ;; ($ button {:on-click #(set-current-view :stats-view)} "📜")
      ($ button {:on-click #(! game/set-current-view :crafting-view)} "🛠")
      ($ button {:on-click #(! game/spawn-strange-creature)} "👿")
      ($ button {:on-click #(! game/toggle-reverse-time)} (if reverse-time?
                                                            "←⌛"
                                                            "⌛→"))
      )
    ($ message-log-view {& {:message-log message-log}})))
;; ↙
;; (defnc rect-button [{}])
;; (d/button {:class "rounded-full bg-gray-300 hover:bg-green-300 py-1"
;;                           & {:key id
;;                              :on-click #(! game/try-to-craft id)}}
;;                          (game/kw->str num id))
(defnc desc-popup [{:keys [c->e->v mouse-over-relative-coord scroll-pos] :as db}]
  (d/div
    {:class "flex flex-col h-full bg-gray-600 font-mono text-red-200 text-lg"}
    (d/p {:class "text-yellow-200"}
         (str "score: " (or (get-in c->e->v [:container (get-player-id db) :loot]) 0)))
    (d/p {:class "text-green-500"}
         (str "hp: " (or (get-in c->e->v [:combat (get-player-id db) :hp]) 0)))
    (for [e (game/get-adjacent-entities-with-component db :interact)]
      (let [[char name] (game/entity-components c->e->v e [:char :name])]
        (d/button {:class "rounded-full  focus:outline-none text-black bg-gray-300 hover:bg-green-300 active:bg-green-400 py-1"
                   &      {:key      e
                           :on-click #(! game/interact e)}}
                  (str char " " name))))
    (if mouse-over-relative-coord
      (let [list (reverse (game/get-entities-on-relative-coord db mouse-over-relative-coord))]
        (map-indexed (fn [i e]
                       (let [[char name] (entity-components c->e->v e [:char :name])]
                         (d/p {& {:key   i
                                  :class (if (->> list
                                                  count
                                                  dec
                                                  (clamp 0 scroll-pos)
                                                  (= i))
                                           ;; (= i (clamp 0 scroll-pos (dec (count list))))
                                           "border-solid border-2"
                                           "")}}
                              (str char " " name))))
                     list)))))
(stylefy/class "right-of-sidebar" {:position "fixed"
                                   :height   "100vh"
                                   :left     "30vh"})
(defnc entity-view [{:keys [c->e->v selected-entity]}]
  (let [{:keys [name char] :as aaa} (update-vals c->e->v #(get % selected-entity))]
    (d/div {:class "right-of-sidebar p-7 flex-col flex w-100 space-y-2"}
           (d/p {:class "text-white p-1 text-5xl"}
                (str char " " name))
           (for [[k v] (filter second (dissoc aaa :name :char))]
             (d/p {:key k}
                  (str k " " v))))))
;; (path :c->e->v :player)
(defnc inventory-view [{:keys [c->e->v] :as db}]
  (let [player-id  (get-player-id db)
        player-inv (get-in c->e->v [:container player-id])]
    ;; (js/console.log (str player-inv))
    (d/div {:class "right-of-sidebar p-7 flex-col flex w-80 space-y-2"}
           (d/p {:class "text-yellow-400 p-1"}
                "You have:")
           (for [[id num] player-inv]
             (d/p {:key id}
                  (str (game/kw->str num id) " " (get-in c->e->v [:char id])))))))
(defnc crafting-view [{:keys [c->e->v] :as db}]
  (let [player-inv (get-player-component db :container)]
    ;; (js/console.log (str player-inv))
    (d/div {:class "right-of-sidebar p-7 text-black flex-col flex w-80 space-y-2"}
           (d/p {:class "p-1"}
                "You have:")
           (for [[id recipe] crafting-recipes]
             (let [num (or (get player-inv id) 0)]
               (d/button {:class "rounded-full bg-gray-300 hover:bg-green-300 active:bg-green-400 py-1"
                          & {:key id
                             :on-click #(! game/try-to-craft id)}}
                         (game/kw->str num id)))))))
;; helix.core/memo
;; .........
(def grid-side-length (inc (* 2 game/view-radius)))
(stylefy.core/class "grid-style" {:grid-template-columns (str "repeat(" grid-side-length ", 1fr)")
                                  :grid-template-rows    (str "repeat(" grid-side-length ", 1fr)")
                                  :position              "fixed"
                                  :left                  "30vh"
                                  :width                 "100vh"
                                  :height                "100vh"
                                  })
(def grid-range (range (- view-radius) (inc view-radius)))
(def reverse-grid-range (reverse grid-range))
(defnc overlay-grid []
  (d/div
    {:class          "grid grid-style select-none"
     :on-mouse-leave #(! game/mouse-over-relative-coord nil)
     }
    (for [y reverse-grid-range
          x grid-range]
      (d/div
        {:class "bg-white opacity-0 hover:opacity-20 hover:animate-pulse"
         &      {:key           [x y]
                 :on-mouse-over #(! game/mouse-over-relative-coord [x y])
                 :on-click      #(! game/world-click)}}))))
(def overlay-grid-element ($ overlay-grid))

(defnc world-grid-cell [{:keys [bg-color char]}]
  (d/div {:class "overflow-hidden"
          &      {:style {:background-color bg-color}}}
         char))
(defnc world-grid [{:keys [c->e->v] :as db}]
  (d/div
    {:class "grid grid-style text-3xl select-none"
     }
    (let [[posx posy] (get-player-pos db)]
      (for [y reverse-grid-range
            x grid-range]
        (let [t                  [(+ x posx) (+ y posy)]
              {:keys [bg-color]} (get-in c->e->v [:tile t])
              es                 (conj (keys (get-in c->e->v [:container t])) t)
              char               (get-in c->e->v [:char (last es)])
              ]
          ($ world-grid-cell {& {:key t
                                 :bg-color bg-color
                                 :char     char}}))))))
;; use-memo
;; use-callback
;; hooks/use-callback
(defnc world-view [{:keys [time] :as db}]
  (<>
    (hooks/use-memo [time] ($ world-grid {& (select-keys db [:c->e->v])}))
    overlay-grid-element
    (d/div {:style {:position "fixed"
                    :left     "130vh"
                    :height   "100vh"
                    }}
           ($ desc-popup {& (select-keys db [:c->e->v :mouse-over-relative-coord :scroll-pos])}))))
;; context...
(defnc main-view []
  (let [[{:keys [current-view] :as db} set-state!] (hooks/use-state initial-db)]
    (defonce aa (reset! setter set-state!))
    (d/div {
            ;; :onMouseUp     #(! mouse-up)
            ;; :on-mouse-move #(mouse-move %1)
            :class         "h-screen w-screen bg-gray-600 font-mono text-red-200 text-lg overflow-hidden"
            :id "main-view"
            }
           (d/div {:class "flex-none"
                   :style {:position "fixed"
                           :height   "100vh"
                           :width    "30vh"}}
                  ($ sidebar {& (select-keys db [:reverse-time? :message-log])}))
           (case current-view
             :entity-view    ($ entity-view {& (select-keys db [:c->e->v :selected-entity])})
             :world-view     ($ world-view {& (select-keys db [:scroll-pos :mouse-over-relative-coord :c->e->v :time])})
             :inventory-view ($ inventory-view {& (select-keys db [:c->e->v])})
             :crafting-view  ($ crafting-view {& (select-keys db [:c->e->v])})
             ;; :stats-view     ($ stats-view (select-keys db [:c->e->v]))
             ;; :abilities-view ($ abilities-view (select-keys db [:c->e->v]))
             (d/p {:class "right-of-sidebar"} (kw->str current-view))))))
