(ns gamething.db)

(def default-db
  {
   ;; :keys {:pressed {:left nil
   ;;                  :right nil
   ;;                  :up nil
   ;;                  :down nil
   ;;                  }
   ;;        :last-x :left
   ;;        :last-y :up
   ;;        }
   :mouse-over-relative-coord nil
   :scroll-pos 0
   :entity-count 0
   :c->e->v      {}
   :selected-entity nil
   :history '()
   :reverse-time? nil
   :time 0
   :tiles nil
   :message-log   []
   :pressed-keys #{}
   :new-pressed-keys #{}
   :newest-pressed-y nil
   :newest-pressed-x nil
   :current-view :world-view
   })
