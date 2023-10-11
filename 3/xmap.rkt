#lang racket

(require json)
(require threading)
(require struct-plus-plus)

(require Q/Common/map)
(require Q/Common/data/tile)
(require Q/Common/data/posn)

(define jboard (read-json (current-input-port)))
(define jtile  (read-json (current-input-port)))

(define board            (hash->board++ jboard))
(define tile             (hash->tile++ jtile))
(define valid-placements (valid-tile-placements tile board))

(define (sort^ lst extract-key)
  (sort lst < #:key extract-key))

(define sorted-placements
  (~> valid-placements
      (sort^ posn-column)
      (sort^ posn-row)))

(define (posn->hash p)
  (struct->hash posn p))

(define out-json (map posn->hash sorted-placements))

(write-json out-json (current-output-port))
(displayln "")
(flush-output)