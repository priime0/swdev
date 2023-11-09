#lang racket

(require Q/Common/tile)
(require Q/Common/game-state)
(require Q/Common/player-state)
(require Q/Common/turn-action)
(require Q/Common/interfaces/playable)
(require Q/Referee/observer)
(require Q/Lib/macros)
(require Q/Lib/result)

(require threading)
(require 2htdp/image)

(provide play-game)

(define obs (make-parameter (new default-observer%)))

;; ========================================================================================
;; DATA DEFINITIONS
;; ========================================================================================


;; A Referee is a function from a list of players to the list of winners and the list of
;; rulebreakers. The entry-point to the Referee is `play-game`.
;; FAULT PROTOCOL:
;; - The Ref will respond to player errors (contract and other exceptions)
;;      by removing that player
;; - The Ref will respond to invalid turn requests by removing that player
;; - The Ref will respond to a timeout by removing that player
;; If no fault occurs, then a referee will enact the turn and commit all changes
;; to the game state.
;; FAULT ISSUE: If the player becomes unresponsive, then calling
;; `(send player name)` won't retrieve the name. Perhaps it should be
;; cached.

;; ----------------------------------------------------------------------------------------

#; {type GameInfo = (game-info PrivateState [Listof String])}
;; A GameInfo represents the referee's current information about the
;; progression of a game. It contains the current state, as well as
;; the list of players that broke the rules, in the increasing
;; time-order of elimination.
(struct/contract game-info
                 ([state priv-state/c]
                  [sinners (listof string?)])
                 #:transparent)

#; {GameInfo PrivateState -> GameInfo}
;; Creates a copy of the given game info with the given new private state.
(define/contract (set-game-info-state g-info new-state)
  (-> game-info? priv-state/c game-info?)
  (match-define [game-info priv-state sinners] g-info)
  (game-info new-state sinners))

;; ----------------------------------------------------------------------------------------

#; {type GameEndContinuation = Continuation}
;; A GameEndContinuation is a continuation that, when called, will
;; trigger the end of the game. It should be called with one argument,
;; the final GameInfo.


;; ========================================================================================
;; FUNCTIONALITY
;; ========================================================================================

#; {[Listof Playable] -> (list [Listof String] [Listof String])}
;; Play a game of Q with the given list of players to completion, producing the list of players with
;; the highest scores sorted in lexicographical order and a list of rulebreakers sorted by temporal
;; order of rule-breaking.

;; CONSTRAINT: If starting from a start game state, the list of players must be
;; equal length to the list of player states in the game state.
(define (play-game playables
                   #:tiles [tiles start-tiles]
                   #:game-state [gs* (make-game-state playables tiles)])
  (define gs (bind-playables gs* playables))

  (define game-info0 (setup gs))
  (send (obs) observe (game-info-state game-info0))
  (define game-info1 (run-game game-info0))
  (send (obs) observe (game-info-state game-info1))
  (match-define [game-info priv-state sinners0] game-info1)

  (define-values (winners losers) (winners+losers priv-state))
  (define winners0 (map player-state-payload winners))
  (define losers0  (map player-state-payload losers))

  (define-values (winners1 sinners1) (notify-players winners0 #t))
  (define-values (losers1  sinners2) (notify-players losers0 #f))

  (list winners1 (append sinners0 sinners1 sinners2)))


#; {PrivateState [Listof Playable] -> PrivateState}
;; Populates the game state's player states with the given playables.
(define/contract (bind-playables gs playables)
  (->i ([gs priv-state/c] [playables (listof (is-a?/c playable<%>))])
       #:pre/name (gs playables)
       "# of player states in the game state != # players"
       (= (length (game-state-players gs))
          (length playables))
       [result priv-state/c])

  (match-define [game-state board tiles states] gs)
  (define states+ (map set-player-state-payload states playables))
  (game-state board tiles states+))


#; {PrivateState -> GameInfo}
;; Communicate to all the players the initial setup of the private state, kicking out all the
;; players that misbehaved during this stage.
;; EFFECT: calls the setup method of playables.
(define (setup gs)
  (match-define [game-state board _tiles states] gs)
  (for/fold ([gs^       gs] [sinners   '()]
                            #:result (game-info gs^ (reverse sinners)))
            ([state states])
    (match-define [player-state _score hand _ playable] state)
    (define name           (unwrap-or (send/checked playable name #f) ""))
    (define setup-result   (send/checked playable setup name board hand))
    (define setup-success? (success? setup-result))
    (if setup-success?
        (values (do-turn/rotate gs^) sinners)
        (values (remove-player gs^)  (cons name sinners)))))


#; {GameInfo -> GameInfo}
;; Run the rounds of the game to completion, collecting the private
;; state and sinners, starting with the given game info.
;; ASSUME: the game has been set up.
(define (run-game g-info)
  (let/ec end-game
    ;; generative: produces the final game state.
    ;; terminates:
    ;; 1. all the players drop out if they misbehave, ending the game.
    ;; 2. there are a finite number of tiles, so eventually all are exhausted, and so players will
    ;; be unable to place any tiles, ending the game.
    (let loop ([g-info^ g-info])
      (loop (run-round g-info^ end-game)))))


#; {GameInfo GameEndContinuation -> GameInfo}
;; Run a full round, accumulating the game information.
;; EFFECT: if a turn ends with no placements, this triggers the end of the game.
(define (run-round g-info k)
  (define priv-state (game-info-state g-info))
  (define-values (next-g-info placement-made?)
    (for/fold ([g-info^ g-info]
               [any-placed? #f]
               #:result (values g-info^ any-placed?))
              ([state (game-state-players priv-state)])
      (define playable (player-state-payload state))
      (match-define [cons g-info+ placed?] (run-turn g-info^ playable k))
      (send (obs) observe (game-info-state g-info+))
      (values g-info+ (or placed? any-placed?))))

  (if (not placement-made?)
      (k next-g-info)
      next-g-info))


#; {GameInfo String GameEndContinuation -> GameInfo}
;; Kicks the currently active player from the game, updating the game info.
;; EFFECT: if the last player is removed, this triggers the end of the game.
(define (kick-player g-info name k)
  (match-define [game-info priv-state sinners] g-info)
  (define priv-state+ (remove-player priv-state))
  (define g-info+     (game-info priv-state+ (append sinners (list name))))
  
  (if (players-left? priv-state+)
      g-info+
      (k g-info+)))

#; {type TurnResult = [Pairof GameInfo Boolean]}
;; A TurnResult represents a pair of the game info after the turn, and
;; whether or not a placement was requested during the turn,
;; preventing the round from ending.


#; {GameInfo Playable GameEndContinuation -> TurnResult}
;; Performs a single turn, returning the updated game information,
;; and  whether a placement was requested.
;; EFFECT: If the turn ends the game, this triggers the end of the game.
;; EFFECT: sends a player their new tiles.
(define (run-turn g-info player k)
  (match-define [game-info priv-state sinners] g-info)
  (let/ec stop-turn
    (define name (unwrap-or (send/checked player name #f) ""))

    (define pub-state (priv-state->pub-state priv-state))
    (define turn-result (send/checked player take-turn #f pub-state))
    (unless (success? turn-result)
      (stop-turn (cons (kick-player g-info name k) #f)))
    
    (define action  (success-val turn-result))
    (define placed? (place? action))
  
    (unless (turn-valid? priv-state action)
      (stop-turn (cons (kick-player g-info name k) placed?)))

    (define-values (priv-state+ ended?) (do-turn-without-rotate priv-state action))
    (when ended?
      (k (game-info priv-state+ sinners)))

    (unless (pass? action)
      (define new-hand            (new-tiles priv-state+))
      (define new-tiles-result    (send/checked player new-tiles #f new-hand))

      (unless (success? new-tiles-result)
        (stop-turn (cons (kick-player (game-info priv-state+ sinners) name k) placed?))))
    

    (define priv-state++ (do-turn/rotate priv-state+))
    (define g-info+      (game-info priv-state++ sinners))
    (cons g-info+ placed?)))


#; {[Listof Playables] Boolean -> (values [Listof String] [Listof String])}
;; Notify the players of whether they won, collecting the remaining list of valid players, and the
;; list of misbehavers.
(define (notify-players players won?)
  (for/fold ([valid '()] [sinners '()] #:result (values (reverse valid) (reverse sinners)))
            ([player players])
    (define name (unwrap-or (send/checked player name "") ""))
    (define won-result (send/checked player win name won?))
    (match won-result
      [(success _)
       (values (cons name valid) sinners)]
      [(failure name)
       (values valid (cons name sinners))])))


(module+ test
  (require Q/Player/player)
  (require Q/Player/dag)

  (define gs (make-game-state start-tiles
                              (list (new player% [id 'lucas] [strategy (new dag%)])
                                    (new player% [id 'andrey] [strategy (new dag%)])
                                    (new player% [id 'luke] [strategy (new dag%)])))))

