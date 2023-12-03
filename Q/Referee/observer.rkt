#lang racket

(require Q/Common/game-state)
(require Q/Common/config)
(require Q/Common/interfaces/serializable)

(require racket/class)
(require racket/set)

(provide
 observer<%>
 observer-manager%)


;; ========================================================================================
;; DATA DEFINITIONS
;; ========================================================================================

#; {interface Observer}
;; An Observer represents a component that facilitates spectator interaction with a game as it
;; progresses.
(define observer<%>
  (interface ()
    #; {Observer PrivateState -> Void}
    ;; Receive the next private game state to observe.
    [observe (->m priv-state/c void?)]

    #; {Observer -> Void}
    ;; Signal that the game has ended.
    [terminate (->m void?)]))


;; ========================================================================================
;; FUNCTIONALITY
;; ========================================================================================

#; {class ObserverManager}
;; A ObserverManager is a concrete implementation the Observer interface that represents a
;; collection of Observers to dispatch received messages to.
(define observer-manager%
  (class* object% (observer<%>)
    (super-new)
    (field [observers (mutable-set)])

    (define/public (observe priv-state)
      (for ([observer observers])
        (send observer observe priv-state)))

    (define/public (terminate)
      (for ([observer observers])
        (send observer terminate)))

    (define/public (connect observer)
      (unless (set-member? observers observer)
        (set-add! observers observer)))

    (define/public (disconnect observer)
      (when (set-member? observers observer)
        (set-remove! observers observer)))))





(module+ test
  (require racket/class)

  (require Q/Common/tile)

  (require Q/Common/tile)
  (require Q/Player/player)
  (require Q/Player/ldasg)
  (require Q/Player/dag)
  (require Q/Player/iterative)

  (define dag (new dag%))
  (define ldasg (new ldasg%))
  (define itdag (new iterative% [strategy dag]))
  (define itldasg (new iterative% [strategy ldasg]))

  (define luke (new player% [id 'luke] [strategy itdag]))
  (define andrey (new player% [id 'andrey] [strategy itdag]))
  (define lucas (new player% [id 'lucas] [strategy itldasg]))

  (define players0 (list luke andrey lucas))

  (define gs1 (make-game-state (take start-tiles 30) players0)))
