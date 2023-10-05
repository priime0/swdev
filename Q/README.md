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
  - `Common/`: Contains components used by players and referees
    - `map.rkt`: The representation of the board and its functions
    - `game-state.rkt`: The representation of the all of the public
      and private knowledge in a game of Q
    - `config.rkt`: The configuration of the game, including globals
    - `data/`:
      - `tile.rkt`: The representation of tiles and its functions
      - `posn.rkt`: The representation of an arbitrary positions and
        concrete directions
    - `util/`:
      - `list.rkt`: List function utilities
      - `hash.rkt`: Hashtable function utilities
      - `struct.rkt`: struct function utilities

## Roadmap

- `tile.rkt` contains the representation of tiles and exposes
  functionality to create, compare, and render tiles.
- `map.rkt` contains the representation of the board and exposes
  functionality to create the game board component, manipulate the
  board by placing tiles and retrieving possible placement positions
  for a new tile. Future iterations could include rendering the board.
- `config.rkt` contains global parameters for defining settings about
  the game, such as the size of the rendering of the tiles. This is
  the point of control for the settings of the game. In the future,
  different possible directions could be supported, etc.
    




