### Game State Referee Protocol

The Game State and the Referee must obey the following protocol for valid communications for the game of Q:

#### Setting Up a Game

```
referee                      game-state
  |                             |
  |                             |
  |                             |
  |  make(tiles, players)       |
  |---------------------------> |
  |  omnisicient-state          |
  |<============================|

```

During setup, a referee must call `make-game-state` with a list of
tiles and a list of Players (with there being enough tiles to place
one onto the board and to hand out `(*hand-size*)` number of tiles to
each player, controlled via `config.rkt`

#### Playing a Game

```
referee                      game-state
   |                             |
   |  valid-turn?(omni, action)  |
   |---------------------------->|
   |      true?                  |
 --|<============================|
 |                               |
 ->|    (remove-player omni)     |

```
