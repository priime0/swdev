# Q

A game based on Qwirkle, written in Racket.

## Setup

### Pre-requisites

- Racket 8.10
- raco

### Installation

In the `Q` directory, if it hasn't been installed yet:

```bash
$ # Install the game and its packages
$ raco pkg install --auto --skip-installed
```

## Tests

To run the collection of unit tests and integration tests, run in the
`Q` directory:

```bash
$ # Make the test script executable
$ chmod +x xtest
$ # Run the test script
$ ./xtest
```

## Project Structure

The project is structured as follows:

- `Q/`: The root directory of the project
  - `README.md`: This file
  - `xtest`: The test script for running unit and integration tests
  - `Player/`: Contains player implementations and player strategies
    - `dag.rkt`: The "dumb and greedy" strategy implementation
    - `ldasg.rkt`: The "less dumb and still greedy" implementation
    - `iterative.rkt`: The implementation of an iterative strategy
    - `greedy-select-strategy.rkt`: The abstract base class for both
      greedy strategies.
    - `strategy.rkt`: The strategy interface, and some helpers for
      strategies.
    - `player.rkt`: The local house implementation of the `playable` interface 
  - `Referee/`: Contains referee functionality
    - `referee.rkt`: The referee functionality; running games, rounds,
      and turns
  - `Common/`: Contains components used by players and referees
    - `map.rkt`: The representation of the board and its functions.
    - `game-state.rkt`: The representation of the public and private
      game states and all functionality for running games, validating
      turns, and scoring.
    - `player-state.rkt`: The representation of the knowledge about a
      player during a game.
    - `config.rkt`: The configuration of the game, including globals
    - `interfaces/`: Contains common interfaces
      - `playable.rkt`: Contains the common interface for the public
        player API
      - `serializable.rkt`: Contains the interface for serialization to
        JSON.
    - `data/`:
      - `tile.rkt`: The representation of tiles and its functions
      - `posn.rkt`: The representation of an arbitrary positions and
        concrete directions
      - `turn-action.rkt`: The representation of actions that can be
        performed during a turn.
    - `util/`:
      - `list.rkt`: List function utilities
      - `hash.rkt`: Hashtable function utilities
      - `struct.rkt`: struct function utilities
      - `function.rkt`: Higher order function utilities
      - `image.rkt`: Image utilities
      - `test.rkt`: Utilities for testing
      - `misc.rkt`: Utilities for various miscellaneous categories

## Roadmap

- Support remote player implementations of the `playable` interface
- Support different rule sets other than just the game of `Q`

## Module Diagrams

![overview](https://files.priime.dev/swdev/overview.png)

![strategy](https://files.priime.dev/swdev/strategy.png)
