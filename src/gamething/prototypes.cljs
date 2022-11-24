(ns gamething.prototypes)


(def wall {:tile {:type     :wall
                  :bg-color "#717171"
                  }
           :name "wall"
           :char "#"
           })
(def water {:tile {:type     :wall
                   :bg-color "#5961FF"
                   }
            :name "water"
            })
(def sand {:tile {:type     :floor
                  :bg-color "#D9DC60"
                  }
           :name "sand"
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
(def ladder {:tile {:type :floor
                    :bg-color "#4A4A4A"}
             :name "ladder"
             :char "🪜"
             :portal :cave})
(def player {:char   (rand-nth ["🤓" "🐸" "😠" "🤡" "😳" "😔" "😐" "🤤" "🙃" "🙂" ])
             :name   "player"
             :player true
             :container {}
             :hp 300
             })
(def enemy {:char "👿"
            :name "enemy"
            :enemy-movement true
            :attack-player true
            :hp 30
            })
(def snowman {:name            "snowman"
              :char            "⛄"
              :random-movement true})

(def spider {:char          "🕷"
             :name          "spider"
             :attack-player true
             })
(def sheep {:char            "🐑"
            :name            "sheep"
            :random-movement true
            })
(def duck {:char            "🦆"
           :name            "duck"
           :random-movement true
           })
(def rabbit {:char            "🐇"
             :name            "rabbit"
             :random-movement true
             })
(def chest {:container {}})
(def items {:loot  {:char     "💰"
                    :name     "loot"
                    :takeable true
                    }

            })
