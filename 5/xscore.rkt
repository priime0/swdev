#lang racket

(require json)

(require Q/Common/game-state)
(require Q/Common/map)
(require Q/Common/turn-action)
(require Q/Common/player-state)
(require Q/Common/config)

(define jmap (read-json))
(define jplacements (read-json))

(define b (hash->board++ jmap))

(define placements (map hash->placement++ jplacements))

(define pub (game-state b 0 (list (player-state 0 '() #f))))

(define score
  (parameterize ([*bonus* 0])
    (do-turn/score pub (place placements))))
(write-json score)
(displayln "")
(flush-output)
