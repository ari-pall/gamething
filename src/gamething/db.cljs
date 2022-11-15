(ns gamething.db)

(def default-db
  {
   ;; :inventory {:snowball       1
   ;;             :large-snowball 0
   ;;             :snowman        0
   ;;             :stick          1
   ;;             :money          50
   ;;             }
   :cards     {0 {:pos  [700 300]
                  :text "spider"}
               1 {:pos  [900 340]
                  :text "snowball"}
               2 {:pos  [600 600]
                  :text "book"}
               }

   :drag          {:held-card              nil
                   :held-card-relative-pos [nil nil]
                   }
   :factory       {:view-offest              [0 0]
                   :held-object              nil
                   :held-object-relative-pos [nil nil]
                   :objects                  {0 {:pos [700 300]
                                                 :id  :crate}
                                              1 {:pos [900 340]
                                                 :id  :thing-maker}
                                              2 {:id       :conveyor-belt
                                                 :connects [1 0]}
                                              }}
   :keys {:pressed {:left nil
                    :right nil
                    :up nil
                    :down nil
                    }
          :last-x :left
          :last-y :up
          }
   :mouse-over-relative-coord nil
   :scroll-pos 0
   :entity-count 0
   :c->e->v      {}
   :selected-entity nil
   :history '()
   :reverse-time? nil
   :tiles nil
   :message-log   []
   :current-dir  [0 0]
   :move-dir     [0 0]
   :current-view :world-view
   })
