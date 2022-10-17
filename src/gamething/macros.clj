(ns gamething.macros)

(defmacro -->
  ([thing] thing)
  ([thing arg & args]
   (if (seq? arg)
     (let [[fst & rst] arg]
       `(--> (~fst ~thing ~@rst) ~@args))
     `(--> (~arg ~thing) ~@args))))

(defmacro defsub
  ([name f]
   `(do
      (defn ~name [] (re-frame.core/subscribe [~(keyword name)]))
      (re-frame.core/reg-sub ~(keyword name)
                             (fn [~'db ~'_]
                               (~f ~'db)))))
  ([name [db & args] body]
   `(do
      (defn ~name [~@args] (re-frame.core/subscribe [~(keyword name) ~@args]))
      (re-frame.core/reg-sub ~(keyword name)
                             (fn [~db [~'_ ~@args]]
                               ~body))))
  ([name args [subs-names subs-expr] body]
   `(do
      (defn ~name [~@args] (re-frame.core/subscribe [~(keyword name) ~@args]))
      (re-frame.core/reg-sub ~(keyword name)
                             (fn [[~'_ ~@args]] (vec ~subs-expr))
                             (fn [~subs-names [~'_ ~@args]]
                               ~body)))))

(defmacro defevent
  ([name f]
   `(defn ~name []
      (swap! ~'gamething.game/db ~f)
      (js/console.log ~(str name "ed"))
      ))
  ([name [db & args] body]
   `(defn ~name [~@args]
      (swap! ~'gamething.game/db (fn [~db] ~body))
      (js/console.log ~(str name "ed"))
      )))
