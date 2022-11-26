(ns gamething.prototypes)


(def wall {:tile {:type     :wall
                  :bg-color "#717171"
                  }
           :name "wall"
           :char "#"
           })
(def fish {:char     "🐟"
           :name     "fish"
           :takeable true
           })
(def water {:tile     {:type     :wall
                       :bg-color "#5961FF"
                       }
            :name     "water"
            :interact [:give-item :fish]
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
           :interact  [:give-item :wood];; [:message "you punch the tree"]
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
             :name "ladder down"
             :char "🪜"
             :on-player-step :go-to-cave
             })
(def player {:char      (rand-nth ["🤓" "🐸" "😠" "🤡" "😳" "😔" "😐" "🤤" "🙃" "🙂" ])
             :name      "player"
             :player    true
             :container {}
             :combat    {:hp     300
                         :damage 2}
             })
(def enemy {:char           "👿"
            :name           "enemy"
            :enemy-movement true
            :attack-player  true
            :combat         {:hp     30
                             :damage 1}
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
(def items {:loot {:char     "💰"
                   :name     "loot"
                   :takeable true
                   }
            :wood {:char     "🪵"
                   :name     "wood"
                   :takeable true
                   }
            :fish {:char     "🐟"
                   :name     "fish"
                   :takeable true
                   }
            })
;; (def tiles {:wall  {:tile {:type     :wall
;;                            :bg-color "#717171"
;;                            }
;;                     :name "wall"
;;                     :char "#"
;;                     }
;;             :water {:tile     {:type     :wall
;;                                :bg-color "#5961FF"
;;                                }
;;                     :name     "water"
;;                     :interact [:add-item]
;;                     }
;;             :sand  {:tile {:type     :floor
;;                            :bg-color "#D9DC60"
;;                            }
;;                     :name "sand"
;;                     }
;;             :tree  {:tile     {:type     :wall
;;                                :bg-color "#27AD00"
;;                                }
;;                     :name     "tree"
;;                     :char     "🌲"
;;                     :interact [:message "you punch the tree"]
;;                     }
;;             :rock  {:tile {:type     :wall
;;                            :bg-color "#71A269"
;;                            }
;;                     :name "rock"
;;                     :char "🪨"
;;                     }
;;             :grass {:tile {:type     :floor
;;                            :bg-color "#22B800"
;;                            }
;;                     :name "grass"
;;                     }
;;             })
