#lang racket

(require json)

(require Q/Common/game-state)
(require Q/Player/player)
(require Q/Referee/referee)


(module+ main
  (define jstate (read-json))
  (define jactors (read-json))

  (define start-state (hash->priv-state jstate))
  (define players (map hash->player++ jactors))

  (write-json (run-game players #:start-state start-state))
	(displayln ""))
