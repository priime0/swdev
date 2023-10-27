**TO:** Matthias Felleisen <matthias@ccs.neu.edu>, Benjamin Lerner <blerner@ccs.neu.edu>

**FROM:** Andrey Piterkin <piterkin.a@northeastern.edu>, Lucas Sta
Maria <lucas@priime.dev>

**DATE:** 2023-10-26

**SUBJECT:** Design of the Game-Observer Mechanism

We propose the introduction of the `observer` component for
facilitating the observation of the game and how the game progresses.
The observer component will need to interact with several existing
components. In order for the observer to display the game, it will
need to receive the game state via some method.

## The Interface

Thus, we propose an `observable<%>` interface that promises a single
method: `observe`. Then, similar to the `playable<%>` Player
interface, we can have a concrete implementation and a implementation
that dispatches the game state to some remote observer on another
computer.

```racket
#; {interface Observable}
;; An Observable promises a single method that consumes a game state
;; for observation.
(define observable<%>
  (interface ()
    #; {Observable GameState -> Void}
    ;; Communicate the game state to the observer
    [observe (->m game-state? void?)]))
```

## Integration into the Existing System

To integrate with the existing system, we additionally propose an
"observer manager" that the referee accepts prior to the game
starting. An "observer manager" will be a concrete implementation of
the `observable<%>` interface that will contain a list of connected
observers. The referee will dispatch the current game state after
every move, and the "observer manager" will in turn dispatch that game
state to every observer it contains.

```racket
(define observer-manager%
  (class* object% (observable<%>)
    (init-field observers)
  
    (define/public (observe gs) ...)
    
    ;; Connect the given observer to the observer manager.
    (define/public (connect observer) ...)
    
    ;; Disconnect the given observer to the observer manager.
    (define/public (disconnect observer) ...)
    
    ...))
```

## User Interaction Experience

Suppose Alice wants to observe an on-going game. Alice would construct
a concrete implementation of the `observable<%>` interface that is
passed into the observer manager, and the observer manager
would add the observer to its list of observers. Then, after each
turn, the observer manager would dispatch the game state to Alice's
observer.

Alice's concrete implementation of the `observable<%>` interface can
render or observe the received game state in any way it see fit, using
functionality provided by Alice's `Q/Common/game-state`.

If Alice wanted to disconnect at any point, her observer would call
the `disconnect` method on the observer manager, and the observer
manager would remove Alice's observer from its list of observers.
