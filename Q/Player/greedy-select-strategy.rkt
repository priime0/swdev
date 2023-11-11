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

    #; {PublicState -> TilePlacement}
    ;; Choose this strategy's preferred, smallest placement possible
    ;; in the given public state
    (define/public (smallest-placement pub-state)
      (match-define [game-state board tiles* [cons state others]] pub-state)
      (define hand (player-state-hand state))
      (define sorted-tiles (sort hand tile<))
      (for/first ([t (in-list sorted-tiles)]
                  #:do [(define posns (valid-tile-placements t board))]
                  #:when (pair? posns))
        (placement (send this smallest-posn posns board) t)))

    #; {[Listof Posn] Board -> Posn}
    ;; Chooses this strategy's preferred posn on the board out of the
    ;; given list of posns.
    (define/public (smallest-posn posns board)
      (define sorted-posns (sort posns posn<?))
      (first sorted-posns))

    #; {TurnInfo -> TurnAction}
    (define/public (choose-action pub-state)
      (define maybe-pment (send this smallest-placement pub-state))
      (if maybe-pment
          (place (list maybe-pment))
          (send this pass-or-exchange pub-state)))))
