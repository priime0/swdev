**TO:** Matthias Felleisen <matthias@ccs.neu.edu>, Benjamin Lerner <blerner@ccs.neu.edu>

**FROM:** Andrey Piterkin <piterkin.a@northeastern.edu>, Lucas Sta Maria <lucas@priime.dev>

**DATE:** 2023-09-27

**SUBJECT:** Design of the Game State Component

### Functionality Wishlist in Game State

```racket
#; {type GameState = (game-state [Listof Player]
                                 [HashTable Player PlayerState])}
;; A GameState represents the state of a game of Q at any instant, implemented as a
;; list of Players in the game, and a hash table where the keys are players and the values are 
;; their states in this game state.

#; {type PlayerState = (player-state Natural [Listof Tile])}
;; A PlayerState represents a player's state at any instant during the game,
;; and contains a player's current score and the tiles in their hand at that moment.

#; {type TurnAction = (U [List 'place-tile [Listof [List Integer Integer Tile]] Natural]
                         [List 'exchange]
                         [List 'pass])}
;; An TurnAction represents a possible action during a player turn, and is one of:
;; placing tile(s), which places tiles on the board, and updates a player's score with the given natural
;; exchange, which switches all remaining tiles available for the current player (and gives no points)
;; or pass, which does nothing at all.

#; {type TurnRequest = (turn-request Player [Listof Tile] [HashTable String Natural] Natural Board)}
;; A TurnRequest represents the information referee requires to dispatch a turn to a player, and
;; communicate all relevant information to that player.
;; It contains the current player whose turn it is, their tiles, the scores in the game, the number of
;; tiles remaining, and the current board.

#; {Player ...+ -> GameState}
;; Creates a fresh game state with the given players, i.e. starts a new game.

#; {GameState Player -> GameState}
;; Kicks a player from the game, by creating a game state without that player. 

#; {GameState -> TurnRequest}
;; Produce the current turn request from the given game state.

#; {GameState TurnAction -> GameState}
;; Apply the given action to the current game state, creating a new game state.
;; That is, update the current player whose turn it is, points, tiles in hand,
;; tiles remaining, and board.
;; ASSUME: The referee, using information from the turn request and returned player action,
;; has already validated the request action, or kicked the player.
```
