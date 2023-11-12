#lang racket

(require Q/Common/game-state)
(require Q/Common/turn-action)
(require Q/Player/strategy)
(require Q/Player/dag)
(require Q/Player/ldasg)
(require Q/Player/strategy-deserialize)
(require Q/Common/interfaces/serializable)

(require json)

(provide main)


(define (main)
  (define j1 (read-json))
  (define j2 (read-json))

  (define jstrat (if (string? j1)
                     j1
                     j2))
  (define jpub (if (string? j1)
                   j2
                   j1))

  (define pub-state (hash->pub-state jpub))
  (define strat     (hash->strategy++ jstrat))

  (define action (send strat choose-action pub-state))
  (define serialized-action
    (match action
      [(place pments) (->jsexpr (first pments))]
      [(exchange)     "replace"]
      [(pass)         "pass"]))

  (write-json serialized-action)
  (displayln "")
  (flush-output))

(module+ main
  (main))
