(ns gamething.core
  (:require
   [stylefy.core :as stylefy]
   [stylefy.generic-dom :as stylefy-generic-dom]
   [dumdom.core]
   ;; [reagent.dom :as rdom]
   ;; [re-frame.core :as rf]
   ;; [re-frame.db]
   [gamething.game :as game]
   )
  (:require-macros [gamething.macros :refer [defsub defevent]]))

(defn ^:dev/after-load mount-root []
  (let [root-el (.getElementById js/document "app")]
    (dumdom.core/unmount root-el)
    ;; (dumdom.core/render (game/view) root-el)
    ))

(def inited (atom nil))
(defn init []
  (when-not @inited
    (reset! inited true)
    (stylefy/init {:dom (stylefy-generic-dom/init)})
    (game/init)
    (js/setTimeout #(game/add-message! "hi")  4000)
    (js/setTimeout #(game/add-message! "this is the message log") 7000)
    (js/setTimeout #(game/add-message! "it tells you what's happening") 10000)
    (js/setInterval game/tick 1000)
    (.addEventListener js/window "keyup" #(when-not (.-repeat %)
                                            (game/key-up (.-key %))
                                            ;; (js/console.log (str (.-key %) " up"))
                                            ))
    (.addEventListener js/window "keydown" #(when-not (.-repeat %)
                                              (game/key-down (.-key %))
                                              ;; (js/console.log (str (.-key %) " down"))
                                              ))
    ))

;; (shadow.cljs.devtools.client.browser/ws-status)
;; (shadow.cljs.devtools.client.)
