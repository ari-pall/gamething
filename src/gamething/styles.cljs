(ns gamething.styles
  (:require-macros
   ;; [garden.def :refer [defcssfn]]
   )
  (:require
   ;; [spade.core   :refer [defglobal defclass]]
   ;; [garden.core :refer [css]]
   ;; [garden.selectors :refer [defselector defclass defid defpseudoclass defpseudoelement]]
   ;; [garden.units :refer [deg px]]
   ))

;; (defcssfn linear-gradient
;;  ([c1 p1 c2 p2]
;;   [[c1 p1] [c2 p2]])
;;  ([dir c1 p1 c2 p2]
;;   [dir [c1 p1] [c2 p2]]))

;; (defglobal defaults
;;   [:body
;;    {:color               :red
;;     :background-color    :#ddd
;;     :background-image    [(linear-gradient :white (px 2) :transparent (px 2))
;;                           (linear-gradient (deg 90) :white (px 2) :transparent (px 2))
;;                           (linear-gradient (rgba 255 255 255 0.3) (px 1) :transparent (px 1))
;;                           (linear-gradient (deg 90) (rgba 255 255 255 0.3) (px 1) :transparent (px 1))]
;;     :background-size     [[(px 100) (px 100)] [(px 100) (px 100)] [(px 20) (px 20)] [(px 20) (px 20)]]
;;     :background-position [[(px -2) (px -2)] [(px -2) (px -2)] [(px -1) (px -1)] [(px -1) (px -1)]]}])

;; (defclass level1
;;   []
;;   {:color :green})
