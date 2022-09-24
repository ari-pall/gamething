(ns gamething.prototypes)


(def wall {:tile {:type     :wall
                  :bg-color "#717171"
                  }
           :name "wall"
           :char "#"
           })
(def tree {:tile {:type     :wall
                  :bg-color "#626B00"
                  }
           :name "tree"
           :char "🌲"
           })
(def grass {:tile {:type     :floor
                   :bg-color "#22B800"
                   }
            :name "grass"
            })
(def loot {:char     "💰"
           :name     "loot"
           :takeable true
           })
(def player {:char   "😀"
             :name   "player"
             :player true
             })
