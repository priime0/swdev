#lang racket

(require threading)
(require predicates)

(require Q/Common/turn-info)
(require Q/Common/player)
(require Q/Common/map)
(require Q/Common/data/tile)
(require Q/Common/data/posn)
(require Q/Common/config)
(require Q/Common/data/turn-action)
(require Q/Common/util/list)

;; TODO: THIS IS AWFUL CODE. THIS WAS WRITTEN TO TEST STRATEGY LOGIC

;; A Player Strategy is an interface that represents the functionality that
;; any player strategy will have to support, namely, choosing an action to perform.
(define player-strategy (interface () choose-action))

(define (choose-tile hand board)
  (~>> hand
       sort-tiles
       (map (lambda (t) (cons t (valid-tile-placements t board))))
       (findf (lambda (p) (nonempty-list? (cdr p))))))

(define (choose-placement t posns sort-func)
  (place (list (placement (first (sort-func posns))
                          t))))

(define (remove-placed state t)
  (remove-from-hand state (list t)))

(define (update-turn-info info pment)
  (match-define [turn-info state _scores _history board _tiles*] info)
  (define b+ (add-tile board pment))
  (define s+ (remove-placed state (placement-tile pment)))
  (turn-info s+ _scores _history b+ _tiles*))

(define (combine-1act a1 a2)
  (match (cons a1 a2)
    [(cons (place pments1) (place pments2)) (place (append pments1 pments2))]
    [(cons (place (list pment pments ...)) _) (place (cons pment pments))]
    [(cons (place '()) a)  a]))


(define dag%
  (class* object% (player-strategy)
    (super-new)

    #; {TurnInfo -> TurnAction}
    ;; Produces a turn action, which is pass, exchange, or a placement of 1 tile
    ;; choosing the lexicographically smallest tile, if possible to place any.
    ;; Prefers place > exchange > pass. Breaks ties with row-column order.
    (define/public (choose-action info)
      (match-define [turn-info state scores history board tiles*] info)

      (define first-maybe-tile-pair
        (choose-tile (player-state-hand state) board))

      (match first-maybe-tile-pair
        [(cons t posns)
         (choose-placement t posns (sort-by (list (cons < posn-column)
                                                  (cons < posn-row))))]
        [_ (if (>= tiles* (*hand-size*))
               (exchange)
               (pass))]))))

(define ldasg%
  (class* object% (player-strategy)
    (super-new)

    #; {TurnInfo -> TurnAction}
    ;; Produces a turn action, which is pass, exchange, or a placement of 1 tile
    ;; choosing the lexicographically smallest tile, if possible to place any.
    ;; Prefers place > exchange > pass. Breaks ties with most neighbors, then with
    ;; row-column order.
    (define/public (choose-action info)
      (match-define [turn-info state scores history board tiles*] info)

      (define first-maybe-tile-pair
        (choose-tile (player-state-hand state) board))

      (match first-maybe-tile-pair
        [(cons t posns)
         (choose-placement t posns (sort-by (list (cons < posn-column)
                                                  (cons < posn-row)
                                                  (cons > (lambda~> adjacent-tiles length)))))]
        [_ (if (>= tiles* (*hand-size*))
               (exchange)
               (pass))]))))

;; iterative strategy
;; TODO: doesnt always produce valid moves -- it has to 
(define itstrat%
  (class* object% (player-strategy)
    (init s)

    (define strat s)

    (super-new)

    (define/public (choose-action info)
      (let/ec return
        (let loop ([action (place '())] [info^ info])
          (define 1act (send strat choose-action info^))
          (define new-total-act (combine-1act action 1act))
          (unless (place? 1act)
            (return new-total-act))

          (define pment (first (place-placements 1act)))
          (define info+ (update-turn-info info^ pment))
          (loop new-total-act info+))))))

(module+ test
  (require rackunit)

  (define turn-info-1
    (turn-info
     (player-state
      'lucas
      0
      (list
       (tile 'yellow 'clover)
       (tile 'green 'diamond)
       (tile 'yellow 'square)
       (tile 'red 'clover)
       (tile 'blue 'diamond)
       (tile 'purple 'circle)))
     '((andrey . 0) (luke . 0))
     '()
     (make-board (tile 'orange 'diamond))
     17))

  (define dag1 (new dag%))
  (define istrat1 (new itstrat% [s dag1])))

(module+ test
  )
