**TO:** Matthias Felleisen <matthias@ccs.neu.edu>, Benjamin Lerner <blerner@ccs.neu.edu>

**FROM:** Andrey Piterkin <piterkin.a@northeastern.edu>, Lucas Sta
Maria <lucas@priime.dev>

**DATE:** 2023-10-19

**SUBJECT:** Full Game State Interface Design

### Complete Game State Interface

```
;; Creates a game state with an initial collection of tiles and a list
;; of player ids. The given list of tiles can be in any order, any there 
;; must be sufficient tiles to place one on the board and hand out to
;; all players. The list of players should be sorted in order of
;; descending age.
[make-game-state (-> (listof tile?) (listof player-id?) game-state?]

;; Produce the public knowledge necessary for the current player to take 
;; their turn from the given game state.
[game-state->turn-info (-> game-state? game-state?]

;; Performs the given turn action for the current player in this game
;; state. Either enacts the turn and updates the queue, returning the
;; updated board, or removes the player and reclaims their tiles.
[take-turn (-> game-state? turn-action? game-state?]

;; Gets the updated player state for the player who just took their
;; turn, to update them with their new complete hand and score.
[get-updated-state (-> game-state? player-state?)]

;; Removes the current player from the game state, reclaiming their
;; tiles.
[remove-player (-> game-state? game-state?]

;; Is the game over?
[game-over? (-> game-state? boolean?]

;; Produce the final ranking of players from this game state.
;; Orders the list of players in order of descending score.
;; Must be called on a game that has ended.
[final-ranking (-> game-state? (list/c player-id? natural?))]
```

### Referee-Game State Protocol

#### Setup Phase

During setup, the referee must create the game state. The referee will
collect enough players for the game to start, sort them by descending
age. The only interaction between the referee and game state during this
phase is the referee creating the game state.

Protocol:

1. Players sign up to referee.
2. Referee acquires all players and tile set, and creates game state
   with ```(make-game-state tiles players)```.

#### Play

During play, the referee asks the game state for the current public
knowledge, sends that to the current player (stored in the public
knowledge), waits for a response (or kicks the player if they exceed
time limit), then updates the game state and then the player with their
new state. Since the game-state is immutable, the referee can store any
game state, and suspend gameplay and resume from any game state.

Protocol:

1. Ref checks ```(game-over? curr-state)```. If the game is over,
   proceed to Game End.
2. Referee requests public knowledge for current player with
   ```(game-state->turn-info curr-state)``` from game state.
3. Referee sends this to player, waiting for response.
4. If player exceeds time limit, ref calls ```(remove-player
   curr-state)``` (which returns the next game state) and proceeds to Step 1.
5. Else, player returns an ```action``` to the ref, which calls
   ```(take-turn curr-state action```). This will either kick the player
   and reclaim their tiles, or apply the action to the game state.
   Returns the next game state ```next-state``` in both cases.
6. The ref now calls ```(get-updated-state next-state)``` which gets the
   player who just took their turn's new state, and sends it to that
   player.
7. Go to 1.

#### Game End

During the game end, the referee produces the final rankings, and sends
them to each player. At this point, the interaction between the game
state and the ref end.

Protocol:

1. Ref calls ```(final-rankings curr-state)```. It sends this to each
   player.
2. Interaction ends.
