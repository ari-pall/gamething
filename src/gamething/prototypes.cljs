(ns gamething.prototypes)


(def wall {:tile {:type     :wall
                  :bg-color "#717171"
                  }
           :name "wall"
           :char "#"
           })
(def tree {:tile {:type     :wall
                  :bg-color "#27AD00"
                  }
           :name "tree"
           :char "🌲"
           })
(def rock {:tile {:type     :wall
                  :bg-color "#71A269"
                  }
           :name "rock"
           :char "🪨"
           })
(def grass {:tile {:type     :floor
                   :bg-color "#22B800"
                   }
            :name "grass"
            })
(def player {:char   "😀"
             :name   "player"
             :player true
             :container {}
             })
(def sheep {:char            "🐑"
            :name            "sheep"
            :random-movement true
            })
(def duck {:char            "🦆"
           :name            "duck"
           :random-movement true
           })
(def chest {:container {}})
(def items {:chest {:container {}
                    :name "chest"
                    :char "c"}
            :loot  {:char     "💰"
                    :name     "loot"
                    :takeable true
                    }

            })
