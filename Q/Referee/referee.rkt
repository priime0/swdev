#lang racket

(require racket/sandbox)

(require Q/Common/game-state)
(require Q/Common/interfaces/playable)
(require Q/Common/data/turn-action)
(require Q/Common/config)
(require Q/Common/player-state)

;; A Referee is a function from a list of players to the list of
;; winners and the list of rulebreakers.

;; FAULT PROTOCOL:
;; - The Ref  will respond to player errors (contract and other exceptions)
;;      by removing them
;; - The Ref will respond to invalid turn requests by removing that player
;; - The Ref will respond to a timeout by removing that player
(define (run-game players)
  ;; TODO: pass in non empty tileset
  (let loop ([gs (make-game-state players '())]) (loop)))

#; {PrivState -> PrivState}
;; Run a single round to completion, or ending early if the game ends
;; before the round is over.
#;
(define (run-round ps)
  (define num-players (length (game-state-players ps)))
  (for/fold ([ps^ ps]
             [kicked '()]
             [placed-all-tiles? #f]
             []
             #:result ps)
            ([i num-players]
             #:break (or (not (any-players? (game-state-players ps)))
                         placed-all-tiles?))
    (define pub-state (priv-state->pub-state ps^))

    ))

#; {PrivateState -> PrivateState}
;; Run a single turn for the current player, obeying the protocol with
;; PrivateState, assuming that the game is not over.
(define (run-turn priv-state)
  (define pub-state (priv-state->pub-state priv-state))
  (define curr-player (first (game-state-players pub-state)))
  (with-handlers ([exn:fail?
                   (lambda (_) (remove-player priv-state))])
    (define action
      (call-with-limits (*timeout*)
                        #f
                        (thunk (send (player-state-player curr-player) takeTurn pub-state))))
    (cond
      [(valid-turn? pub-state action) ])
    
    )
  )

#; {(-> Any) -> 
(define (with-timeout proc))
