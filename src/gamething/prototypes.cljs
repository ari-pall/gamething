(ns gamething.prototypes)


(def wall {:tile {:type      :wall
                  :color     "#717171"
                  :char      "#"
                  :name      "wall"
                  :contentes #{}
                  }})
(def loot {:char     "💰"
           :name     "loot"
           :takeable true
           })
(def tree {:tile {:type      :wall
                  :color     "#626B00"
                  :char      "🌲"
                  :name      "tree"
                  :contentes #{}
                  }})
(def grass {:tile {:type      :floor
                   :color     "#22B800"
                   :name      "grass"
                   :contentes #{}
                   }})
(def player {:char   "😀"
             :name   "player"
             :player true
             })
