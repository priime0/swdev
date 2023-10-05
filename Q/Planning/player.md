**TO:** Matthias Felleisen <matthias@ccs.neu.edu>, Benjamin Lerner <blerner@ccs.neu.edu>

**FROM:** Andrey Piterkin <piterkin.a@northeastern.edu>, Lucas Sta
Maria <lucas@priime.dev>

**DATE:** 2023-10-05

**SUBJECT:** Design of the Player Mechanics

We define a _sequence_ to be a collection of distinct posn-tile pairs
such that for any two different posn-tile pairs α, β in the sequence, 
reachable?(α, β).

We define two distinct posn-tile pairs α, β to be _reachable_ IFF WLOG
they share one axis and for all posns φ between the posns of α and β,
φ is occupied by a tile.

```racket
;; A Player is a function interface (in Racket, a generic interface) 
;; that implements the single, following function:

#; {Player TurnInfo -> TurnAction}
;; Given the necessary information to take a turn for this player, 
;; produce the action this player takes.
(define (take-turn player turn-info) ...)

;; For this piece of functionality, the player will have to extract 
;; all possible positions to place any of its tiles in hand, along 
;; with using the function for collecting sequences (WIP) in order to 
;; measure the value of each possible move. We will also need to 
;; extract from the TurnInfo the tiles in the player's hand, along 
;; with the scores of other players. We additionally extract 
;; information about the actions performed during "rounds" so that
;; players can incorporate that into their strategies.

#; {Player -> PlayerId}
;; Retrieve the name of the player.
(define (player-name player) ...)

#; {Player -> Natural}
;; Retrieve the age in years of the player.
(define (player-age player) ...)

;; The context for the above functions are any concrete struct
;; implementations of the Player generic interface. That is the
;; source of knowledge.
```
