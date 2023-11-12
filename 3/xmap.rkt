#lang racket

(require json)
(require threading)
(require struct-plus-plus)

(require Q/Common/map)
(require Q/Common/tile)
(require Q/Common/posn)

(provide main)

(define (main)
  (define jboard (read-json (current-input-port)))
  (define jtile  (read-json (current-input-port)))

  (define board            (hash->board++ jboard))
  (define tile             (hash->tile++ jtile))
  (define valid-placements (posns-for-tile tile board))

  (define sorted-placements
    (~> valid-placements
        (sort _ posn<?)))

  (define (posn->hash p)
    (struct->hash posn p))

  (define out-json (map posn->hash sorted-placements))

  (write-json out-json (current-output-port))
  (displayln "")
  (flush-output))

(module+ main
  (main))
