#lang racket

(require racket/sandbox)
(require threading)

(require Q/Common/game-state)
(require Q/Common/interfaces/playable)
(require Q/Common/turn-action)
(require Q/Common/config)
(require Q/Common/player-state)
(require Q/Common/tile)
(require Q/Lib/list)

(provide run-game)


;; A Referee is a function from a list of players to the list of
;; winners and the list of rulebreakers.

;; FAULT PROTOCOL:
;; - The Ref will respond to player errors (contract and other exceptions)
;;      by removing that player
;; - The Ref will respond to invalid turn requests by removing that player
;; - The Ref will respond to a timeout by removing that player
;; If no fault occurs, then a referee will enact the turn and commit all changes
;; to the game state.
(define (run-game players
                  #:start-state [gs #f]
                  #:tile-set [tile-set start-tiles])
  (define gs*
    (if gs
        (apply-players (lambda (lops) (map set-player-state-player lops players))
                       gs)
        (make-game-state tile-set players)))

  (println gs*)

  (define gs+
   (for/fold ([gs^ gs*])
             ([ps (game-state-players gs*)])
     (match-define [player-state _ hand playable] ps)
     (send/checked
      (thunk
       (send playable setup (game-state-board gs*) hand)
       gs^)
      (thunk
       (remove-player gs^)))))

  (let loop ([priv-state^ gs+])
    (define round-result (run-round priv-state^))
    (cond
      [(game-state? round-result)
       (loop round-result)]
      [(list? round-result)
       (define-values (winners losers)
         (end-game round-result))
       (list (player-names winners) (remove-from (player-names (append winners losers))
                                                 (player-names players)))])))


#; {type TurnResult = (U [Pair 'place PrivateState]
                         [Pair 'no-place PrivateState]
                         [Pair 'game-end PrivateState])}
;; Represents the result of a single turn, which can either be a new
;; state after a placement, a new state after no placement, or an end
;; of game.

#; {type RoundResult = (U PrivateState
                       [Listof PlayerState])}


#; {[Listof Playable] -> [Listof String]}
;; Gets the name of each player in the given list.
(define (player-names players)
  (~>> players
       (map (lambda (play) (send play name)))))


#; {PrivState -> RoundResult}
;; Run a single round to completion, or ending early if the game ends
;; before the round is over.
(define (run-round ps)
  (define num-players (length (game-state-players ps)))
  (define-values (next-priv-state any-places? end?)
    (for/fold ([ps^ ps]
               [any-placements? #f]
               [end? #f])
              ([_ (in-range num-players)]
               #:break end?)
      (define turn-result (run-turn ps^))
      (match turn-result
        [(cons 'place ps+)    (values ps+ #t #f)]
        [(cons 'no-place ps+) (values ps+ any-placements? #f)]
        [(cons 'game-end ps+) (values ps+ any-placements? #t)])))
  (cond
    [(or end? (not any-places?))
     (game-state-players next-priv-state)]
    [else next-priv-state]))

#; {PrivateState -> TurnResult}
;; Run a single turn for the current player, obeying the protocol with
;; PrivateState, assuming that the game is not over.  Returns the
;; updated state and whether this action ended the game, and whether
;; this was a placement action.
(define (run-turn priv-state)
  (define pub-state (priv-state->pub-state priv-state))
  (define curr-player (first (game-state-players pub-state)))
  (define playable    (player-state-player curr-player))

  (let/ec return
    (define misbehave-proc (lambda (act) (thunk (return (deal-with-misbehavior priv-state act)))))
    (define action
      (send/checked (thunk (send playable take-turn pub-state))
                    (misbehave-proc (pass))))
    (println action)
    (cond
      [(valid-turn? priv-state action)
       (define priv-state+ (apply-turn priv-state action))
       (define score       (score-turn priv-state+ action))
       (unless (pass? action)
         (define p-tiles     (new-tiles priv-state+ action))
         (send/checked (thunk (send playable new-tiles p-tiles))
                       (misbehave-proc action))
         (define priv-state++ (end-turn priv-state+ action #:new-points score #:tiles-given (length p-tiles)))
         (return
          (cond
            [(turn-ends-game? priv-state+ action) (cons 'game-end priv-state++)]
            [(place? action)                      (cons 'place    priv-state++)]
            [else                                 (cons 'no-place priv-state++)])))

       (cons 'no-place (end-turn priv-state+ action #:new-points score))]
      [else (deal-with-misbehavior priv-state action)])))

#; {(-> Any) (-> Any) -> Any}
;; IDK
(define (send/checked send-thunk callback)
  (with-handlers ([exn:fail? (lambda (e) (println e) (callback))])
    (with-timeout send-thunk)))

#; {PrivateState TurnAction -> TurnResult}
;; Removes a misbehaving player, returning either the next state or an end-of-game signal.
(define (deal-with-misbehavior priv-state [action (pass)])
  (define priv-state+ (remove-player priv-state))
  (define turn-type
    (match action
      [(place _) 'place]
      [_         'no-place]))
  (cons (if (any-players? priv-state+)
            turn-type
            'game-end)
        priv-state+))

#; {[Listof PlayerState] -> (values [Listof Playable]
                                    [Listof Playable])}
;; Computes the list of winners and the list of losers.
(define (end-game players)
  (define winners  (get-winners players))
  (define losers   (filter (negate (curryr member? winners)) players))
  (define win-players (map player-state-player winners))
  (define lose-players (map player-state-player losers))
  (values win-players lose-players))

#; {[Listof PlayerState] -> [Listof PlayerState]}
;; Gets the list of winners from the given list of player states.
(define (get-winners player-states)
  (define max-score (apply max (map player-state-score player-states)))
  (filter (lambda (p) (= (player-state-score p) max-score)) player-states))

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

  (require Q/Lib/test)
  (require Q/Common/posn)
  (require Q/Common/tile)
  (require Q/Common/turn-action)
  (require Q/Player/strategy)
  (require Q/Player/player)
  (require Q/Player/dag)
  (require Q/Player/ldasg)
  (require Q/Common/map)

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
               (thunk (run-game
                       (list (new player% [id 'andrey] [strategy dag])
                             (new player% [id 'lucas]  [strategy ldasg])
                             (new player% [id 'luke]   [strategy dag]))
                       #:tile-set tile-set)))
   '(("andrey") . (())))

  (test-equal?
   ""
   (apply/seed 0
               (thunk (run-game
                       (list (new player% [id 'luke]   [strategy dumb])
                             (new player% [id 'andrey] [strategy dag])
                             (new player% [id 'lucas]  [strategy ldasg]))
                       #:tile-set tile-set)))
   '(("andrey") . (("luke")))))
