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
(require Q/Player/greedy-select-strategy)

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
    ;; generative: generates a posn that has no adjacent tiles on the given board.
    ;; terminates: the map is finite and posns are infinite, so there will always
    ;;             be such a position that can be produced by translating in one direction.
    (define/public (first-non-adj-posn p board)
      (define direction (first direction-names))
      (cond
        [(not (has-adjacent-tiles? board p)) p]
        [else
         (define next-posn (posn-translate p direction))
         (send this first-non-adj-posn next-posn board)]))
    
    (define/public (choose-action pub-state)
      (match-define [game-state board tiles* [cons state others]] pub-state)
      (define hand           (player-state-hand state))
      (define open-positions (open-posns board))
      (define start-pos      (first open-positions))
      (define pos            (send this first-non-adj-posn start-pos board))
      (place (list (placement pos (first hand)))))))


;; A "tile-not-owned" strategy will willfuly produce an invalid
;; placement. Specifically, it will choose a tile not in its hand and
;; place it in a valid spot on the board.
(define tile-not-owned%
  (class* greedy-select-strategy% (player-strategy<%>)
    (super-new)
    (inherit/super smallest-placement)

    (define/override (choose-action pub-state)
      (match-define [game-state board _ [cons state _]] pub-state)
      (define hand  (player-state-hand state))
      (define not-in-hand-tiles (filter-not (curryr member? hand) tile-set))

      (define maybe-pment (super smallest-placement not-in-hand-tiles board))
      (if maybe-pment
          (place (list maybe-pment))
          (pass)))))


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
      (define result
        (let/ec return
          (send this select-invalid pub-state '() return)))
      (if (void? result)
          (pass)
          result))))


;; A "bad ask for tiles" strategy is a strategy that willfuly makes an invalid exchange.
(define bad-ask-for-tiles%
  (class* object% (player-strategy<%>)
    (super-new)
    (define/public (choose-action pub-state)
      (define xchange (exchange))
      (if (turn-valid? pub-state xchange)
          (pass)
          xchange))))

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
      
      (or (and maybe-action (place (list maybe-action)))
          (pass)))))

(module+ test
  (require rackunit)
  
  (define no-fit1 (new no-fit%))
  (define bad-xchange1 (new bad-ask-for-tiles%))
  (define not-line1    (new not-a-line%))
  (define non-adj-coord1 (new non-adj-coord%))
  (define tile-not-owned1 (new tile-not-owned%))
  
  (define gs1
    (game-state (make-board (tile 'red 'square))
                (list (tile 'blue 'square))
                (list (player-state++ #:hand (list (tile 'purple 'clover)
                                                   (tile 'green 'square))))))
  
  (define gs2
    (game-state (make-board (tile 'red 'square))
                (list (tile 'blue 'square)
                      (tile 'red 'clover))
                (list (player-state++ #:hand (list (tile 'purple 'clover)
                                                   (tile 'green 'square))))))
  (define gs3
    (game-state (make-board (tile 'red 'square))
                (list (tile 'blue 'square)
                      (tile 'red 'clover))
                (list (player-state++ #:hand tile-set))))

  (define gs4
    (game-state (make-board (tile 'red 'square))
                (list (tile 'blue 'square)
                      (tile 'red 'clover))
                (list (player-state++ #:hand (list (tile 'green 'square))))))

  (define gs5
    (game-state (make-board (tile 'red 'square))
                (list (tile 'blue 'square)
                      (tile 'red 'clover))
                (list (player-state++ #:hand (list (tile 'green 'square)
                                                   (tile 'blue 'square)))))))

(module+ test
  
  (test-equal?
   "bad ask for tiles correctly produces an exchange"
   (send bad-xchange1 choose-action (priv-state->pub-state gs1))
   (exchange))
  
  (test-false
   "bad ask for tiles exchange is invalid"
   (turn-valid? gs1 (exchange)))
  
  (test-equal?
   "if bad ask for tiles cant cheat, it produces a pass"
   (send bad-xchange1 choose-action (priv-state->pub-state gs2))
   (pass))
  
  (test-equal?
   "non adjacent coord picks a placement that is up"
   (send non-adj-coord1 choose-action (priv-state->pub-state gs1))
   (place (list (placement (posn -2 0) (tile 'purple 'clover)))))
  
  (test-false
   "non adjacent coord picks a placement which is invalid"
   (turn-valid? gs1
                (send non-adj-coord1 choose-action (priv-state->pub-state gs1))))
  
  
  (test-equal?
   "not-owned tile strategy tries to place a tile that it doesnt own"
   (send tile-not-owned1 choose-action (priv-state->pub-state gs1))
   (place (list (placement (posn -1 0) (tile 'red 'star)))))
  
  (test-false
   "not owned tile strategy produces an invalid move if it can"
   (turn-valid? gs1 (send tile-not-owned1 choose-action (priv-state->pub-state gs1))))
  
  (test-equal?
   "not owned tile passes if it cant find an invalid move"
   (send tile-not-owned1 choose-action (priv-state->pub-state gs3))
   (pass))

  (test-equal?
   "no fit strategy places a tile so the neighbors dont match"
   (send no-fit1 choose-action (priv-state->pub-state gs1))
   (place (list (placement (posn -1 0) (tile 'purple 'clover)))))

  (test-false
   "no fit strategy produces an invalid move"
   (turn-valid? gs1 (send no-fit1 choose-action (priv-state->pub-state gs1))))

  (test-equal?
   "if there are only valid moves, no fit will pass"
   (send no-fit1 choose-action (priv-state->pub-state gs4))
   (pass))

  (test-equal?
   "not a line will produce pass if it cannot place an otherwise valid non line"
   (send not-line1 choose-action (priv-state->pub-state gs5))
   (place (list (placement (posn 0 -1) (tile 'blue 'square)) (placement (posn -1 0) (tile 'green 'square)))))

  (test-false
   "placements not in a line are invalid moves"
   (turn-valid? gs5 (send not-line1 choose-action (priv-state->pub-state gs5))))

  (test-equal?
   "not a line will produce pass if it cannot place an otherwise valid non line"
   (send not-line1 choose-action (priv-state->pub-state gs1))
   (pass)))






