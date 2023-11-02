#lang racket

(require Q/Common/game-state)
(require Q/Common/data/turn-action)
(require Q/Player/strategy)
(require Q/Player/dag)
(require Q/Player/ldasg)
(require Q/Common/interfaces/serializable)

(require json)


(define j1 (read-json))
(define j2 (read-json))

(define jstrat (if (string? j1)
                   j1
                   j2))
(define jpub (if (string? j1)
                 j2
                 j1))

(define pub-state (hash->pub-state jpub))
(define strat
  (match jstrat
    ["dag"   (new dag%)]
    ["ldasg" (new ldasg%)]))

(define action (send strat choose-action pub-state))
(define serialized-action
  (match action
    [(place pments) (->jsexpr (first pments))]
    [(exchange)     "replace"]
    [(pass)         "pass"]))

(write-json serialized-action)
(displayln "")
(flush-output)
