#lang racket

(require Q/Common/game-state)
(require Q/Common/data/turn-action)
(require Q/Player/strategy)
(require Q/Player/dag)
(require Q/Player/ldasg)
(require Q/Common/interfaces/serializable)

(require json)


(define jstrat (read-json))
(define jpub (read-json))

(define pub-state (hash->pub-state jpub))
(define strat
  (match jstrat
    ["dag"   (new dag%)]
    ["ldasg" (new ldasg%)]))

(define action (send strat pub-state))
(define serialized-action
  (match action
    [(place pments) (->jsexpr (first pments))]
    [(exchange)     "exchange"]
    [(pass)         "pass"]))

(write-json serialized-action)
(displayln "")
(flush-output)
