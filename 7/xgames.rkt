#lang racket

(require json)

(require Q/Common/game-state)
(require Q/Player/player)
(require Q/Referee/referee)

(provide main)

(define (main)
  (define jstate (read-json))
  (define jactors (read-json))

  (define start-state (hash->priv-state jstate))
  (define players (map hash->player++ jactors))

  (define result (play-game players #:game-state start-state))
  (define winners (first result))
  (define sinners (second result))

  (define sorted-winners (sort winners string<=?))

  (write-json (list sorted-winners sinners))
  (displayln "")
  (void))

(module+ main
  (main))
