#lang racket

(require Q/Common/config)
(require Q/Common/game-state)
(require Q/Common/player-state)
(require Q/Common/turn-action)
(require Q/Common/tile)
(require Q/Common/posn)
(require Q/Common/map)
(require Q/Lib/list)

(require Q/Player/strategy)

(provide greedy-select-strategy%)

(define greedy-select-strategy%
  (class* object% (player-strategy<%>)
    (super-new)

    #; {PublicState -> TurnAction}
    ;; Decides whether to pass or exchange. 
    (define/public (pass-or-exchange pub-state)
      (if (turn-valid? pub-state (exchange))
          (exchange)
          (pass)))

    #; {[Listof Tile] Board -> [Maybe TilePlacement]}
    ;; Choose this strategy's preferred, smallest placement possible
    ;; in the given board with the given hand.
    (define/public (smallest-placement hand board)
      (define sorted-tiles (send this rank-tiles hand board))
      (for/first ([t (in-list sorted-tiles)]
                  #:do [(define posns (valid-tile-placements t board))
                        (define sorted-posns (send this rank-posns posns board))]
                  #:when (pair? sorted-posns))
        (placement (first sorted-posns) t)))

    #; {[Listof Tile] Board -> [Listof Tile]}
    ;; Rank the tiles in hand according to this strategy's tile preference.
    (define/public (rank-tiles hand board)
      (sort hand tile<))

    #; {[Listof Posn] Board -> [Listof Posn]}
    ;; Rank this strategy's preferred positions in lexicographic order.
    (define/public (rank-posns posns board)
      (sort posns posn<?))

    #; {TurnInfo -> TurnAction}
    (define/public (choose-action pub-state)
      (match-define [game-state board _ [cons state _]] pub-state)
      (define hand (player-state-hand state))
      (define maybe-pment (send this smallest-placement hand board))
      (if maybe-pment
          (place (list maybe-pment))
          (send this pass-or-exchange pub-state)))))
