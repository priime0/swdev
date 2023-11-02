#lang racket

(require racket/sandbox)
(require threading)

(require Q/Common/game-state)
(require Q/Common/interfaces/playable)
(require Q/Common/turn-action)
(require Q/Common/config)
(require Q/Common/player-state)
(require Q/Common/util/list)


;; A Referee is a function from a list of players to the list of
;; winners and the list of rulebreakers.

;; FAULT PROTOCOL:
;; - The Ref will respond to player errors (contract and other exceptions)
;;      by removing that player
;; - The Ref will respond to invalid turn requests by removing that player
;; - The Ref will respond to a timeout by removing that player
;; If no fault occurs, then a referee will enact the turn and commit all changes
;; to the game state.
(define (run-game players [tile-set '()]
                  #:start-state [gs #f])
  (define start-gs (make-game-state tile-set players))
  (define gs* (or gs start-gs))
  (let loop ([priv-state^ gs*])
    (define-values (priv-state+ game-ended? any-placements?) (run-round priv-state^))

    (cond
      [(not (or game-ended? (not any-placements?)))
       (loop priv-state+)]
      [else
       (define final-players   (game-state-players priv-state+))
       (define initial-players (game-state-players gs*))
       (define final-names     (player-names final-players))
       (define initial-names   (player-names initial-players))

       (define winners   (player-names (get-winners final-players)))
       (define sinners   (remove-from final-names initial-names))
       (list winners sinners)])))


#; {[Listof PlayerState] -> [Listof String]}
;; Gets the name of each player in the given list.
(define (player-names player-states)
  (~>> player-states
       (map player-state-player)
       (map (lambda (play) (send play name)))))

#; {[Listof PlayerState] -> [Listof PlayerState]}
;; Gets the list of winners from the given list of player states.
(define (get-winners player-states)
  (define max-score (apply max (map player-state-score player-states)))
  (filter (lambda (p) (= (player-state-score p) max-score)) player-states))

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
  (define pub-state (priv-state->pub-state priv-state))
  (define curr-player (first (game-state-players pub-state)))
  (define playable    (player-state-player curr-player))

  (with-handlers ([exn:fail?
                   (lambda (e)
                     (println e)
                     (values (remove-player priv-state)
                             (not (any-players? (remove-player priv-state)))
                             #f))])
    (define action
      (with-timeout (thunk (send playable take-turn pub-state))))
    (cond
      [(not (valid-turn? priv-state action))
       (values (remove-player priv-state)
               (not (any-players? (remove-player priv-state)))
               #f)]
      [else
       (define priv-state+ (apply-turn priv-state action))
       (define score       (score-turn priv-state+ action))
       (define p-tiles     (new-tiles priv-state+ action))
       (with-timeout (thunk (send playable new-tiles p-tiles)))
       (define num-placements
         (match action
           [(place pments) (length pments)]
           [_              0]))
       (define game-ended? (= (length (player-state-hand curr-player)) num-placements))
       (values (end-turn priv-state+ action #:new-points score #:tiles-given (length p-tiles))
               game-ended?
               (place? action))])))

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
  (require Q/Common/posn)
  (require Q/Common/tile)
  (require Q/Common/turn-action)
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
   '(("andrey") . (())))


  (test-equal?
   ""
   (apply/seed 0
               run-game
               (list (new player% [id 'luke]   [strategy dumb])
                     (new player% [id 'andrey] [strategy dag])
                     (new player% [id 'lucas]  [strategy ldasg]))
               tile-set)
   '(("andrey") . (("luke")))))
