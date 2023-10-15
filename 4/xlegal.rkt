#lang racket

(require json)
(require threading)
(require struct-plus-plus)
(require (rename-in (only-in lazy define)
                    [define define/lazy]))

(require Q/Common/game-state)
(require Q/Common/map)
(require Q/Common/data/turn-action)
(require Q/Common/data/posn)
(require Q/Common/player)
(require Q/Common/data/tile)
(require Q/Common/util/list)

(define jpub        (read-json (current-input-port)))
(define jplacements (read-json (current-input-port)))

(define info (hash->turn-info++ jpub))
(define placements (map hash->placement++ jplacements))

(define (valid-placements? info placements)
  (match-define [turn-info state _scores _history board _tiles-left] info)
  (define hand         (player-state-hand state))
  (define posns        (map placement-posn placements))
  (define placed-tiles (map placement-tile placements))

  (define all-valid-placements?
   (for/fold ([valid? #t]
              [b      board]
              #:result b)
             ([pment placements]
              #:break (not valid?))
     (match-define [placement p t] pment)
     (if (valid-placement? b t p)
         (values #t (add-tile b p t))
         (values #f #f))))

  (define/lazy all-tiles-in-hand?
    (contains-all? hand placed-tiles))

  (and (same-axis? posns)
       all-valid-placements?
       all-tiles-in-hand?))

(unless (valid-placements? info placements)
  (write-json #f)
  (displayln "")
  (flush-output)
  (exit))

(define b+ (place-tiles (turn-info-board info) placements))


(define (map-list b)
  (define-values (top-bound bot-bound left-bound right-bound)
    (bounds b))

  (for/fold ([rows '()])
            ([r (in-inclusive-range top-bound bot-bound)])
    (append rows
            (list (cons r
                        (for/fold ([cells '()])
                                  ([c (in-inclusive-range left-bound right-bound)])
                          (define t (tile-at b (posn r c)))
                          (if t
                              (append cells
                                      (list (list c (hash 'color (symbol->string (tile-color t))
                                                          'shape (symbol->string (tile-shape t))))))
                              cells)))))))

(define out-json (map-list b+))

(write-json out-json (current-output-port))
(displayln "")
(flush-output)
