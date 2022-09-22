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
   `(do
      (defn ~name [] (re-frame.core/dispatch [~(keyword name)]))
      (re-frame.core/reg-event-db ~(keyword name)
                                  (fn [~'db ~'_]
                                    (~f ~'db)))))
  ([name [db & args] body]
   `(do
      (defn ~name [~@args] (re-frame.core/dispatch [~(keyword name) ~@args]))
      (re-frame.core/reg-event-db ~(keyword name)
                                  (fn [~db [~'_ ~@args]]
                                    ~body)))))

;; (str (macroexpand-1 '(defevent try-to-craft [db id]
;;   (if-let [recipe (crafting-recipes id)]
;;     (let [t (update (zipmap (keys recipe)
;;                             (map - (vals recipe))) id inc)]
;;       (if (valid-transaction? (db :inventory) t)
;;         (-> db
;;             (update :inventory do-transaction t)
;;             (add-message (str "You crafted " (kw->str 1 id))))
;;         (add-message db "You don't have the items to craft that")))
;;     (add-message db "You can't craft that")))))
