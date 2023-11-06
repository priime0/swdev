#lang racket

(require Q/Common/tile)
(require Q/Common/game-state)
(require Q/Common/player-state)
(require Q/Lib/macros)
(require Q/Lib/result)

(require threading)


;; A Referee is a function from a list of players to the list of winners and the list of
;; rulebreakers. The entry-point to the Referee is `play-game`.

#; {[Listof Playable] -> (list [Listof String] [Listof String])}
;; Play a game of Q with the given list of players to completion, producing the list of players with
;; the highest scores sorted in lexicographical order and a list of rulebreakers sorted by temporal
;; order of rule-breaking.
;; FAULT ISSUE: If the player becomes unresponsive, then calling `(send player name)` won't retrieve
;; the name. Perhaps it should be cached.
;; FAULT PROTOCOL:
;; - The Ref will respond to player errors (contract and other exceptions)
;;      by removing that player
;; - The Ref will respond to invalid turn requests by removing that player
;; - The Ref will respond to a timeout by removing that player
;; If no fault occurs, then a referee will enact the turn and commit all changes
;; to the game state.
;; CONSTRAINT: If starting from a start game state, the list of players must be
;; equal length to the list of player states in the game state.
(define (play-game playables #:tiles [tiles start-tiles] #:game-state [gs* (make-game-state playables tiles)])
  (define gs (bind-playables gs* playables))

  (let/ec return
    (define-values (gs1 sinners1) (setup gs))
    ;; gs2, sinners2      <- Run the game to completion
    ;; winners1, losers   <- game-state - Compute winners and losers
    ;; winners2, sinners3 <- Notify winning and losing players
    ;; (list winners (append sinners1 sinners2 sinners3))
    (void)))


#; {PrivateState [Listof Playable] -> PrivateState}
;; Populates the game state's player states with the given playables.
;; CONSTRAINT: the number of players in the game state is equivalent to the number of playables
;;             given, and the order of player states in the game state is the same as the order of
;;             the given playables.
(define (bind-playables gs playables)
  (match-define [game-state board tiles states] gs)
  (define states+ (map set-player-state-player states playables))
  (game-state board tiles states+))


#; {PrivateState -> (values PrivateState [Listof String])}
;; Communicate to all the players the initial setup of the private state, kicking out all the players
;; that misbehaved during this stage.
;; EFFECT: calls the setup method of playables.
(define (setup gs)
  (match-define [game-state board _tiles states] gs)
  (for/fold ([gs^       gs] [sinners   '()]
             #:result (values gs^ (reverse sinners)))
            ([state (game-state-players gs)])
    (match-define [player-state _score hand playable] state)
    (define name           (unwrap-or (send/checked playable name #f) ""))
    (define setup-result   (send/checked state setup name board hand))
    (define setup-success? (success? setup-result))
    (if setup-success?
        (values (do-turn/rotate gs^) sinners)
        (values (remove-player gs^)  (cons name sinners)))))


#; {PrivateState Continuation -> (values PrivateState [Listof String])}
;; Run the rounds of the game to completion, collecting the private state and sinners.
;; ASSUME: the game has been set up.
(define (run-game gs k)
  ;; generative: produces the final game state.
  ;; terminates:
  ;; 1. all the players drop out if they misbehave, ending the game.
  ;; 2. there are a finite number of tiles, so eventually all are exhausted, and so players will be
  ;; unable to place any tiles, ending the game.
  (let loop ()
    ;; run a round, collecting the game state and the list of new cheaters
    (void))
  (void))


#; {PrivateState -> (values PrivateState [Listof String])}
;; Run a single round
;; EFFECT: sends a player's new tiles after a turn.
(define (run-round gs k)
  (for/fold ([gs^ gs]
             [sinners '()]
             #:result (values gs^ (reverse sinners)))
            ([state (game-state-players gs)])
    (match-define [player-state score hand playable] state)
    (let/ec return
      (define name (unwrap-or (send/checked playable name #f) ""))
      (define (kick-player)
        (return (values (remove-player gs^)
                        (cons name sinners))))
      (define turn-result
        (send/checked playable take-turn kick-player gs^))
      (unless (success? turn-result)
        (kick-player))
      (define turn-action (success-val turn-result))
      (unless (turn-valid? gs^ turn-action)
        (kick-player))

      (void)))
  ;; for p in gs.players, fold gs^, sinners:
  ;;   match p.take-turn
  ;;   | success action ->
  ;;     match action.validate()
  ;;     | true ->
  ;;       gs+ <- action.apply(gs^)
  ;;       gs++ <- gs+.score()
  ;;       p.new-tiles(gs++.new-tiles())
  ;;       gs++.deal-tiles( <gs++.new-tiles()> )
  ;;       gs++, sinners
  ;;     | false ->
  ;;       gs+ <- gs.kick-player()
  ;;       gs+, p::sinners
  ;;   | failure _ ->
  ;;     gs+ <- gs.kick-player()
  ;;     gs+, p::sinners

  (void))

#; {[Listof Playables] Boolean -> (values [Listof Playables] [Listof String])}
;; Notify the players of whether they won, collecting the remaining list of valid players, and the
;; list of misbehavers.
(define (notify-players players won?)
  (for/fold ([valid '()] [sinners '()] #:result (values (reverse valid) (reverse sinners)))
            ([player players])
    (define name (send/checked player name ""))
    (define won-result (send/checked player win name won?))
    (match won-result
      [(success _)
       (values (cons player valid) sinners)]
      [(failure name)
       (values valid (cons name sinners))])))


(module+ test
  (require Q/Player/player)
  (require Q/Player/dag)

  (define gs (make-game-state start-tiles
                              (list (new player% [id 'lucas] [strategy (new dag%)])
                                    (new player% [id 'andrey] [strategy (new dag%)])
                                    (new player% [id 'luke] [strategy (new dag%)])))))