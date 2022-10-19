(ns gamething.macros)

(defmacro defevent
  ([name f]
   `(defn ~name []
      ;; (swap! ~'gamething.game/db ~f)
      ((deref ~'setter) ~f)
      ;; ((deref ~'setter) (deref ~'db))
      ;; (js/console.log ~(str name "ed"))
      ))
  ([name [db & args] body]
   `(defn ~name [~@args]
      ;; (swap! ~'gamething.game/db (fn [~db] ~body))
      ((deref ~'setter) (fn [~db] ~body))
      ;; ((deref ~'setter) (deref ~'db))
      ;; (js/console.log ~(str name "ed"))
      )))
