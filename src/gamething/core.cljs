(ns gamething.core
  (:require
   [reagent.dom :as rdom]
   [re-frame.core :as rf]
   [re-frame.db]
   [gamething.events :as events]
   [gamething.game :as game])
  (:require-macros [gamething.macros :refer [defsub defevent]]))

(defn ^:dev/after-load mount-root []
  (rf/clear-subscription-cache!)
  (let [root-el (.getElementById js/document "app")]
    (rdom/unmount-component-at-node root-el)
    (rdom/render [game/view] root-el)))


(def db gamething.game/db)
;; (def db re-frame.db/db)
(defn init []
  (rf/dispatch-sync [:init
                     ;; ::events/initialize-db
                     ])

  (mount-root)
  (defonce first-messages (do (js/setTimeout #(rf/dispatch [:add-message! "hi"]) 4000)
                              (js/setTimeout #(rf/dispatch [:add-message! "this is the message log"]) 7000)
                              (js/setTimeout #(rf/dispatch [:add-message! "it tells you what's happening"]) 10000)))
  (defonce tick (js/setInterval ;; #(swap! db gamething.game/tick)
                  #(rf/dispatch-sync [:tick])
                  50))
  (.addEventListener js/window "keyup" #(rf/dispatch-sync [:key-up (.-key %)]))
  (.addEventListener js/window "keydown" #(rf/dispatch-sync [:key-down (.-key %)]))
  )

;; (shadow.cljs.devtools.client.browser/ws-status)
;; (shadow.cljs.devtools.client.)
