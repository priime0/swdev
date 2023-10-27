#lang racket

(require racket/sandbox)
(require threading)

(require Q/Common/game-state)
(require Q/Common/interfaces/playable)
(require Q/Common/data/turn-action)
(require Q/Common/config)
(require Q/Common/player-state)
(require Q/Common/util/list)

;; A Referee is a function from a list of players to the list of
;; winners and the list of rulebreakers.

;; FAULT PROTOCOL:
;; - The Ref  will respond to player errors (contract and other exceptions)
;;      by removing them
;; - The Ref will respond to invalid turn requests by removing that player
;; - The Ref will respond to a timeout by removing that player
(define (run-game players [tile-set '()]
                  #:start-state [gs #f])
  (define start-gs (make-game-state tile-set players))
  (define gs* (or gs start-gs))
  (define round-num 0)
  (let loop ([priv-state^ gs*])
    (display "round: ")
    (displayln round-num)
    (set! round-num (add1 round-num))
    (define-values (priv-state+ game-ended? any-placements?) (run-round priv-state^))

    (unless (or game-ended? (not any-placements?))
      (loop priv-state+))

    (define players (~>> (game-state-players priv-state+)
                         (map player-state-player)
                         (map (lambda (play) (send play name)))))
    (define initial-players (~>> (game-state-players gs*)
                                 (map player-state-player)
                                 (map (lambda (play) (send play name)))))

    (define max-score (apply max (map player-state-score (game-state-players priv-state+))))
    (define winners   (~>> (game-state-players priv-state+)
                           (filter (lambda (p) (= (player-state-score p) max-score)))
                           (map player-state-player)
                           (map (lambda (play) (send play name)))))
    (define sinners   (remove-from players initial-players))
    `(,winners . ,sinners)))

#; {PrivState -> PrivState}
;; Run a single round to completion, or ending early if the game ends
;; before the round is over.
(define (run-round ps)
  (define num-players (length (game-state-players ps)))
  (for/fold ([ps^ ps]
             [game-ended?     #f]
             [any-placements? #f])
            ([_ (in-range num-players)]
             #:break game-ended?)
    (define-values (ps+ ge? ap?) (run-turn ps^))
    (values ps+ ge? (or ap? any-placements?))))

#; {PrivateState -> (values PrivateState Boolean Boolean)}
;; Run a single turn for the current player, obeying the protocol with
;; PrivateState, assuming that the game is not over.  Returns the
;; updated state and whether this action ended the game, and whether
;; this was a placement action.
(define (run-turn priv-state)
  (let/ec return
    (define pub-state (priv-state->pub-state priv-state))
    (define curr-player (first (game-state-players pub-state)))
    (define playable    (player-state-player curr-player))

    (define action
      (with-timeout (thunk (send playable take-turn pub-state))))
    (unless (valid-turn? priv-state action)
      (return (values (remove-player priv-state)
                      (not (any-players? (remove-player priv-state)))
                      #f)))
    (define priv-state+ (apply-turn priv-state action))
    (define score       (score-turn priv-state action))
    (define p-tiles     (new-tiles priv-state action))
    (with-timeout (thunk (send playable new-tiles p-tiles)))
    (define num-placements
      (match action
        [(place pments) (length pments)]
        [_              0]))
    (define game-ended? (= (length (player-state-hand curr-player)) num-placements))
    (values (end-turn priv-state+ action #:new-points score #:tiles-given (length p-tiles))
            game-ended?
            (place? action))

#;
    (with-handlers ([exn:fail?
                     (lambda (e)
                       (println e)
                       (values (remove-player priv-state)
                               (not (any-players? (remove-player priv-state)))
                               #f))])
      (define action
        (with-timeout (thunk (send playable take-turn pub-state))))
      (unless (valid-turn? priv-state action)
        (return (values (remove-player priv-state)
                        (not (any-players? (remove-player priv-state)))
                        #f)))
      (define priv-state+ (apply-turn priv-state action))
      (define score       (score-turn priv-state action))
      (define p-tiles     (new-tiles priv-state action))
      (with-timeout (thunk (send playable new-tiles p-tiles)))
      (define num-placements
        (match action
          [(place pments) (length pments)]
          [_              0]))
      (define game-ended? (= (length (player-state-hand curr-player)) num-placements))
      (values (end-turn priv-state+ action #:new-points score #:tiles-given (length p-tiles))
              game-ended?
              (place? action)))))

#; {(-> Any) -> Any}
;; Call the given think with this game's timeout, specified in config.
(define (with-timeout proc)
  (call-with-limits (*timeout*)
                    #f
                    proc))


(module+ test
  (require rackunit)

  (require racket/class)
  (require threading)

  (require Q/Common/util/test)
  (require Q/Common/data/posn)
  (require Q/Common/data/tile)
  (require Q/Common/data/turn-action)
  (require Q/Player/strategy)
  (require Q/Player/player)
  (require Q/Player/dag)
  (require Q/Player/ldasg)

  (define tile-set
    (~>> (cartesian-product tile-colors tile-shapes)
         (map (curry apply tile))))

  (define dumb%
    (class* object% (player-strategy<%>)
      (super-new)

      (define/public (choose-action pub-state)
        (place (list (placement (posn 0 0)
                                (tile 'red 'square)))))))

  (define dag (new dag%))
  (define ldasg (new ldasg%))
  (define dumb (new dumb%))
  

  (test-equal?
   ""
   (apply/seed 0
               run-game
               (list (new player% [id 'andrey] [strategy dag])
                     (new player% [id 'lucas]  [strategy ldasg])
                     (new player% [id 'luke]   [strategy dag]))
               tile-set)
   '((lucas) . ()))

#;
  (test-equal?
   ""
   (apply/seed 0
               run-game
               (list (new player% [id 'luke]   [strategy dumb])
                     (new player% [id 'andrey] [strategy dag])
                     (new player% [id 'lucas]  [strategy ldasg]))
               tile-set)
   '((lucas) . (luke))))
