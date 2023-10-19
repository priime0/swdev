#lang racket

(require json)

(require Q/Common/turn-info)
(require Q/Common/map)
(require Q/Common/data/turn-action)
(require Q/Common/player)
(require Q/Common/config)

(define jmap (read-json))
(define jplacements (read-json))

(define b (hash->board++ jmap))

(define placements (map hash->placement++ jplacements))

(define ti (turn-info (player-state '|0| 0 '()) '() '() b 0))
(parameterize ([*hand-size* -1])
  (write-json (score-turn ti (place placements)))
  (displayln "")
  (flush-output))
