(ns gamething.core
  (:require
   [stylefy.core :as stylefy]
   [stylefy.generic-dom :as stylefy-generic-dom]
   [gamething.game :as game]
   ["react-dom/client" :as rdom]
   [helix.core :refer [defnc]]
   )
  (:require-macros [gamething.macros :refer [defevent]]
                   [helix.core :refer [$]]))

(defonce root (rdom/createRoot (js/document.getElementById "app")))

;; (.valueOf root)
;; root/valueOf
;; rdom
;; (.valueOf rdom)
;; rdom/valueOf

(defn ^:dev/after-load mount-root []
  (.render root
           ($ game/main-view))
  )

(def inited (atom nil))
(defn init []
  (when-not @inited
    (reset! inited true)
    (stylefy/init {:dom (stylefy-generic-dom/init)})

    (.render root
             ($ game/main-view))
    ;; (js/setTimeout #(game/add-message! "hi")  4000)
    ;; (js/setTimeout #(game/add-message! "this is the message log") 7000)
    ;; (js/setTimeout #(game/add-message! "it tells you what's happening") 10000)
    (js/setInterval game/tick 75)
    (.addEventListener js/window "wheel" #(game/scroll! (if (< (.-wheelDeltaY %) 0)
                                                          1
                                                          -1)))
    (.addEventListener js/window "keyup" #(when-not (.-repeat %)
                                            (game/key-up (.-key %))))
    (.addEventListener js/window "keydown" #(when-not (.-repeat %)
                                              (game/key-down (.-key %))))
    ;; (game/init)

    ))

;; (shadow.cljs.devtools.client.browser/ws-status)
;; (shadow.cljs.devtools.client.)
