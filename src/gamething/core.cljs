(ns gamething.core
  (:require
   [stylefy.core :as stylefy]
   [stylefy.generic-dom :as stylefy-generic-dom]
   [gamething.game :as game]
   ["react-dom/client" :as rdom]
   [helix.core :refer [defnc $]]
   )
  (:require-macros [gamething.macros :refer [defevent]]))

;; (defn ^:dev/after-load mount-root []
;;   (let [root-el (.getElementById js/document "app")]
;;     (dumdom.core/unmount root-el)
;;     ;; (dumdom.core/render (game/view) root-el)
;;     ))

(def inited (atom nil))
(defn init []
  (when-not @inited
    (reset! inited true)
    (stylefy/init {:dom (stylefy-generic-dom/init)})
    (.render (rdom/createRoot (js/document.getElementById "app"))
             ($ game/view))
    (js/setTimeout #(game/add-message! "hi")  4000)
    (js/setTimeout #(game/add-message! "this is the message log") 7000)
    (js/setTimeout #(game/add-message! "it tells you what's happening") 10000)
    (js/setInterval game/tick 100)
    (.addEventListener js/window "keyup" #(when-not (.-repeat %)
                                            (game/key-up (.-key %))
                                            ;; (js/console.log (str (.-key %) " up"))
                                            ))
    (.addEventListener js/window "keydown" #(when-not (.-repeat %)
                                              (game/key-down (.-key %))
                                              ;; (js/console.log (str (.-key %) " down"))
                                              ))
    ;; (game/init)

    ))

;; (shadow.cljs.devtools.client.browser/ws-status)
;; (shadow.cljs.devtools.client.)
