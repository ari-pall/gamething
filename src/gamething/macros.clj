(ns gamething.macros)

(defmacro event [name]
  `(defn ~(symbol (str name "!")) [& ~'args]
     ((deref ~'gamething.game/setter) (fn [~'db] (apply ~name ~'db ~'args)))))
(defmacro defevent
  ([name f]
   `(defn ~name []
      ((deref ~'gamething.game/setter) ~f)))
  ([name [db & args] body]
   `(defn ~name [~@args]
      ((deref ~'gamething.game/setter) (fn [~db] ~body)))))


;; (defmacro defevent [name [db & args] body]
;;   `(do
;;      (defn ~name [~db ~@args]
;;        ~body)
;;      (defn ~(symbol (str name "!")) [~@args]
;;        ((deref ~'gamething.game/setter) (fn [~db] (~name ~db ~@args))))
;;      ))
