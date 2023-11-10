#lang racket

(require Q/Common/game-state)
(require Q/Common/map)
(require Q/Common/turn-action)
(require Q/Common/player-state)
(require Q/Common/posn)
(require Q/Common/tile)
(require Q/Lib/list)

(require threading)

(require Q/Player/strategy)
(require Q/Player/dag)

(provide (all-defined-out))

;; A "non-adjacent-coord" strategy is a strategy that willfuly
;; produces an invalid turn, by choosing one of its tiles to place in
;; a spot that has no neighbors.
(define non-adj-coord%
  (class* object% (player-strategy<%>)
    (super-new)
    #; {Posn Board -> Posn}
    ;; Starting from the given position on this board, translate in
    ;; one direction to find the first spot that doesn't have any
    ;; neighbors.
    (define/public (first-non-adj-posn start-posn board)
      (define direction (first directions))
      (let loop ([p start-posn])
        (if (has-adjacent-tiles? board posn)
            (loop (posn-translate posn direction))
            p)))

    (define/public (choose-action pub-state)
      (match-define [game-state board tiles* [cons state others]] pub-state)
      (define hand  (player-state-hand state))

      (define open-positions (open-posns board))
      (cond
        [(null? open-positions) (pass)]
        [(null? hand)           (pass)] ;; should never happen
        [else
         (define start-pos   (first open-positions))
         (define pos         (send this first-non-adj-posn start-pos board))
         (place (list (placement pos (first hand))))]))))


;; A "tile-not-owned" strategy will willfuly produce an invalid
;; placement. Specifically, it will choose a tile not in its hand and
;; place it in a valid spot on the board.
(define tile-not-owned%
  (class* object% (player-strategy<%>)
    (super-new)
    (define/public (choose-action pub-state)
      (match-define [game-state board tiles* [cons state others]] pub-state)
      (define hand  (player-state-hand state))

      (define not-in-hand-tiles (filter-not (curry member? tile-set) hand))
      (cond
        [(null? not-in-hand-tiles) (pass)]
        [(pair? not-in-hand-tiles)
         (define maybe-pment (choose-tile not-in-hand-tiles board))
         (match maybe-pment
           [#f
            (pass)]
           [(cons t pments)
            (place (list (first pments)))])]))))


;; A "not a line" strategy willfuly places a series of placements
;; (which are independently valid) but aren't in a line.
(define not-a-line%
  (class* object% (player-strategy<%>)
    (super-new)

    #; {PublicState [Listof TilePlacement] Continuation}
    ;; Using backtracking, computes a placement that is not along the
    ;; same axis.
    (define/public (select-invalid pub-state pments k)
      (when (and (not (same-axis? (map placement-posn pments)))
                 (pair? pments)
                 (pair? (cdr pments)))
        (k (place pments)))

      (match-define [game-state board tiles* [cons state others]] pub-state)
      (define hand (player-state-hand state))
      (cond
        [(null? hand) (void)]
        [(pair? hand)
         (define tile0 (first hand))
         (define possible-posns (valid-tile-placements tile0 board))
         (define possible-placements (map (curryr placement tile0) possible-posns))

         (for ([pment possible-placements])
           (define pub-state+ (do-turn/action pub-state (place (list pment))))
           (select-invalid pub-state+ (cons pment pments) k))

         (define pub-state+ (game-state board tiles* (cons (remove-from-hand state (list tile0))
                                                           others)))
         (select-invalid pub-state+ pments k)]))

    (define/public (choose-action pub-state)
      (let/ec return
        (send this select-first-invalid pub-state '() return)))))


;; A "bad ask for tiles" strategy is a strategy that willfuly makes an invalid exchange.
(define bad-ask-for-tiles%
  (class* object% (player-strategy<%>)
    (super-new)
    (define/public (choose-action pub-state)
      (define xchange (exchange))
      (if (turn-valid? pub-state xchange)
          xchange
          (pass)))))

;; A "no fit" strategy is a strategy that willfuly place a tile in
;; hand into a spot where the neighbors dont match on the board.
(define no-fit%
  (class* object% (player-strategy<%>)
    (super-new)
    (define/public (choose-action pub-state)
      (match-define [game-state board tiles* [cons state others]] pub-state)
      (define hand (player-state-hand state))

      (define open-positions (open-posns board))
      (define maybe-action
        (for*/first ([p open-positions]
                     [t hand]
                     #:when (not (valid-placement? board (placement p t))))
          (placement p t)))

      (or maybe-action (pass)))))
