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


(provide
 player-strategy<%>
 (contract-out
  [choose-tile
   (-> (listof tile?)
       board?
       (or/c (cons/c tile? (listof placement?))
             #f))]
  [choose-placement
   (-> tile?
       (listof posn?)
       procedure?
       turn-action?)]
  [remove-placed
   (-> player-state?
       tile?
       player-state?)]
  [update-turn-info
   (-> turn-info?
       placement?
       turn-info?)]
  [combine-actions
   (-> turn-action?
       turn-action?
       turn-action?)]))


;; A PlayerStrategy is an interface that represents the functionality that any player strategy will
;; have to support, namely, choosing an action to perform given some turn information.
(define player-strategy<%>
  (interface ()
    #; {PlayerStrategy TurnInfo -> TurnAction}
    ;; Given some turn information for the player, produce a valid action to influence the game
    ;; state.
    [choose-action (->m turn-info? turn-action?)]))


#; {[Listof Tile] Board -> [Maybe [Pairof Tile [Listof TilePlacement]]]}
;; Choose the first tile lexicographically that can be placed on the board.
(define (choose-tile hand board)
  (define (tile-and-placements t)
    (cons t (valid-tile-placements t board)))
  (define (has-possible-placement? tp)
    (nonempty-list? (cdr tp)))
  (~>> hand
       sort-tiles
       (map tile-and-placements)
       (findf has-possible-placement?)))


#; {Tile [Listof Posn] ([Listof Posn] -> [Listof Posn]) -> TurnAction}
;; Choose the first placement by the given `posns-sort` function.
;; ASSUME: `posns` is a non-empty list.
(define (choose-placement tile posns posns-sort)
  (define first-posn (first (posns-sort posns)))
  (define pment (placement first-posn tile))
  (place (list pment)))


#; {PlayerState Tile -> PlayerState}
;; Remove a single tile from the player's hand.
(define (remove-placed state t)
  (remove-from-hand state (list t)))


#; {TurnInfo Placement -> TurnInfo}
;; Update the turn information with the given placement, removing the tile from the player's hand.
(define (update-turn-info info pment)
  (match-define [turn-info state _scores _history board _tiles*] info)
  (define b+ (add-tile board pment))
  (define s+ (remove-placed state (placement-tile pment)))
  (turn-info s+ _scores _history b+ _tiles*))


#; {TurnAction TurnAction -> TurnAction}
;; Combine two turn actions into one, where the first action is performed first.
(define (combine-actions a1 a2)
  (match (cons a1 a2)
    [(cons (pass) action)
     action]
    [(cons (exchange) act)
     (if (place? act)
         act
         (exchange))]
    [(cons (place pments) (pass))
     (place pments)]
    [(cons (place pments) (exchange))
     (place pments)]
    [(cons (place p1) (place p2))
     (place (append p1 p2))]))


#;
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

#;
(module+ test
  )
