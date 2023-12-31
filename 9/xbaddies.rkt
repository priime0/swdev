#lang racket

(require json)

(require Q/Common/game-state)
(require Q/Common/config)
(require Q/Player/player)
(require Q/Referee/referee)
(require Q/Lib/json)

(provide main)

(define (main)
  (define jstate (read-json))
  (define jactors (read-json))

  (define start-state (hash->priv-state jstate))
  (define players (map hash->player++ jactors))

  (define result
    (parameterize ([*bonus*        4]
                   [*points-per-q* 8]
                   [*timeout*      6])
      (play-game players #:game-state start-state)))
  (define winners (first result))
  (define sinners (second result))

  (define sorted-winners (sort winners string<=?))

  (json-write+flush (list sorted-winners sinners)))

(module+ main
  (main))
