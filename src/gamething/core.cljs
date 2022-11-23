(ns gamething.core
  (:require
   [stylefy.core :as stylefy]
   [stylefy.generic-dom :as stylefy-generic-dom]
   [gamething.game :as game]
   [gamething.ui :as ui]
   ["react-dom/client" :as rdom]
   [helix.core :refer [defnc $]]
   [goog.dom :as gdom]
   )
  (:require-macros [gamething.macros :refer [defevent]]
                   [helix.core :refer [;; $
                                       ]]))

(defonce root (rdom/createRoot (js/document.getElementById "app")))

(defn ^:dev/after-load mount-root []
  (.render root
           ($ ui/main-view))
  )

;; (.unmount root)
;; (. root)
(def inited (atom nil))
(defn init []
  (when-not @inited
    (reset! inited true)
    (stylefy/init {:dom (stylefy-generic-dom/init)})
    (.render root
             ($ ui/main-view))
    (js/setInterval #(ui/! game/tick) 50 ;; 75
                    )
    (.addEventListener js/window "wheel"
                       #(ui/! game/scroll (if (< (.-wheelDeltaY %) 0)
                                            1
                                            -1)))
    (.addEventListener js/window "keyup"
                       #(when-not (.-repeat %)
                          (ui/! game/key-up (.-key %))))
    (.addEventListener js/window "keydown"
                       #(when-not (.-repeat %)
                          (ui/! game/key-down (.-key %))))

    ))

;; (shadow.cljs.devtools.client.browser/ws-status)
;; (shadow.cljs.devtools.client.)
