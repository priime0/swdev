#lang racket

(require Q/Lib/connection)
(require Q/Common/interfaces/playable)
(require Q/Common/interfaces/serializable)
(require Q/Common/game-state)
(require Q/Common/tile)

(provide
 (contract-out
  [listen (-> connection? (is-a?/c playable<%>) void?)]))

;; ========================================================================================
;; DATA DEFINITIONS
;; ========================================================================================

#; {type EndOfGameContinuation = Continuation}
;; An EndOfGameContinuation represents an escape continuation that
;; signals to the referee proxy that a game is over and listening
;; should stop.

#; {type RPC = (values (-> (U Void TurnAction)) Boolean)}
;; An RPC represents a deserialized remote method request, consisting
;; of a thunk which produces the result of the requested method call,
;; and whether this is the final action of the game.
;; Stands for "remote procedure call".

;; ========================================================================================
;; FUNCTIONALITY
;; ========================================================================================

#; {Connection Playable -> Void}
;; Listens on the provided connection for RPCs until after the call to
;; `win` completes, forwarding them to the provided player and putting
;; results back on the connection.
(define (listen conn playable)
  (let/ec end-listening
    (execute-rpc conn playable end-listening)
    (listen conn playable))
  (void))

#; {Connection Playable EndOfGameContinuation -> Void}
;; Reads and executes the next RPC to come over the connection.
;; Blocks until next command arrives.
(define (execute-rpc conn playable k)
  (define-values (method-call last-call?) (execute-rpc/read conn playable))
  (define method-result (method-call))
  (define serialized-result (->jsexpr method-result))
  (conn-write conn serialized-result)
  (when last-call?
    (k)))

#; {Connection Playable -> RPC}
;; Reads and deserialzies a JSON message into an RPC for the referee
;; proxy to execute.
(define (execute-rpc/read conn playable)
  (define command (conn-read conn))
  (match command
    [(list "setup" (list jpub jtiles))
     (define pub-state (hash->pub-state jpub))
     (define hand      (map hash->tile++ jtiles))
     (values (thunk (send playable setup pub-state hand)) #f)]
    [(list "take-turn" (list jpub))
     (define pub-state (hash->pub-state jpub))
     (values (thunk (send playable take-turn pub-state)) #f)]
    [(list "new-tiles" (list jtiles))
     (define hand (map hash->tile++ jtiles))
     (values (thunk (send playable new-tiles hand)) #f)]
    [(list "win" (list won?))
     (values (thunk (send playable win won?)) #t)]))

;; ========================================================================================
;; TESTS
;; ========================================================================================

(module+ test
  (require rackunit)
  (require Q/Player/mock-player)
  (require Q/Player/player)
  (require Q/Player/strategy)
  (require Q/Player/dag)
  (require Q/Common/game-state)
  (require Q/Common/map)
  (require Q/Common/player-state)
  (require Q/Common/turn-action)
  (require Q/Common/tile)
  (require Q/Common/posn)

  ;; player mock setup
  (define player0 (new player% [id 'andrey] [strategy (new dag%)]))
  (define mock-player0 (new mock-player% [player-delegate player0]))
  (define player1 (new player% [id 'lucas] [strategy (new dag%)]))
  (define mock-player1 (new mock-player% [player-delegate player1]))

  ;; sample data
  (define hand0 (list (tile 'red 'star)))
  (define pub-state0 (game-state (make-board (tile 'red 'square))
                                 1
                                 (list (make-player-state hand0)
                                       0)))
  (define call0 (serialize `(setup (,pub-state0 ,hand0))))
  (define call1 (serialize `(take-turn (,pub-state0))))
  (define call2 (serialize `(win (#t))))

  (define hand1 (list (tile 'red 'star) (tile 'blue 'circle)))
  (define pub-state1 (game-state (make-board (tile 'red 'square))
                                 1
                                 (list (make-player-state hand1)
                                       0)))
  (define call3 (serialize `(setup (,pub-state1 ,hand1))))
  (define call4 (serialize `(take-turn (,pub-state1))))
  (define call5 (serialize `(new-tiles (,(list (tile 'red 'clover))))))
  (define call6 (serialize `(win (#f))))

  ;; mock connection setup
  (define mock-input0 (open-input-string (string-append call0 call1 call2)))
  (define mock-output0 (open-output-string))
  (define mock-connection (connection mock-input0 mock-output0))

  (define mock-input1 (open-input-string (string-append call3 call4 call5 call6)))
  (define mock-output1 (open-output-string))
  (define mock-connection1 (connection mock-input1 mock-output1)))

(module+ test

  (test-case
   "referee proxy calls correct methods on mock player"
   (listen mock-connection mock-player0)
   (check-equal?
    (send mock-player0 get-log)
    (list
     (list 'setup pub-state0 hand0)
     (list 'take-turn pub-state0)
     (list 'win #t)))

   (check-equal?
    (get-output-string mock-output0)
    (string-append (serialize (void))
                   "\n"
                   (serialize (place (list (placement (posn -1 0) (tile 'red 'star)))))
                   "\n"
                   (serialize (void))
                   "\n")))

  (test-case
   "referee proxy calls new-tiles also and calls win with false"
   (listen mock-connection1 mock-player1)
   (check-equal?
    (send mock-player1 get-log)
    (list
     (list 'setup pub-state1 hand1)
     (list 'take-turn pub-state1)
     (list 'new-tiles (list (tile 'red 'clover)))
     (list 'win #f)))

   (check-equal?
    (get-output-string mock-output1)
    (string-append (serialize (void))
                   "\n"
                   (serialize (place (list (placement (posn -1 0) (tile 'red 'star)))))
                   "\n"
                   (serialize (void))
                   "\n"
                   (serialize (void))
                   "\n"))))
