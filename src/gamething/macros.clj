(ns gamething.macros)

(defmacro defevent
  ([name f]
   `(defn ~name []
      ((deref ~'gamething.game/setter) ~f)))
  ([name [db & args] body]
   `(defn ~name [~@args]
      ((deref ~'gamething.game/setter) (fn [~db] ~body)))))
