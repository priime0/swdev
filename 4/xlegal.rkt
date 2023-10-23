#lang racket

(require json)
(require threading)
(require struct-plus-plus)
(require (rename-in (only-in lazy define)
                    [define define/lazy]))

(require Q/Common/game-state)
(require Q/Common/map)
(require Q/Common/data/turn-action)
(require Q/Common/turn-info)
(require Q/Common/data/posn)
(require Q/Common/player-state)
(require Q/Common/data/tile)
(require Q/Common/util/list)
(require Q/Common/interfaces/serializable)

(define jpub        (read-json (current-input-port)))
(define jplacements (read-json (current-input-port)))

(define info (hash->turn-info++ jpub))
(define placements (map hash->placement++ jplacements))

(unless (valid-action? info (place placements))
  (write-json #f)
  (displayln "")
  (flush-output)
  (exit))

(define b+ (add-tiles (turn-info-board info) placements))


(define out-json (serialize b+))

(write-json out-json (current-output-port))
(displayln "")
(flush-output)
