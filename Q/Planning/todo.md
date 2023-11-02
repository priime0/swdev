**TO:** Matthias Felleisen <matthias@ccs.neu.edu>, Benjamin Lerner <blerner@ccs.neu.edu>

**FROM:** Andrey Piterkin <piterkin.a@northeastern.edu>, Lucas Sta
Maria <lucas@priime.dev>

**DATE:** 2023-11-02

**SUBJECT:** TODO

Desired tasks to complete, grouped by subject, and ordered by priority
for the subject.

- [ ] Testing & Bug-fixing:
  - [ ] Fix `valid-board?` to remove the invariant that `(0, 0)` is
        contained in the board.
  - [ ] Make a test script that runs all previous integration tests.
  - [ ] Fix `xscore` to remove `turn-info` references
- [ ] README:
  - [ ] Add a module diagram (IN PROGRESS)
  - [ ] Add a Referee-Player protocol diagram.
  - [ ] Add a Referee-Game-state protocol diagram.
  - [ ] Add a Referee-Observer protocol diagram.
- [ ] Referee:
  - [ ] Call each player's `setup` method.
  - [ ] Call each player's `win` method at the end.
  - [ ] Write unit tests for referee functionality.
  - [ ] Refactor the `run-turn` function in the referee, since there
        is repeated code for error-handling, along with there being
        atomic tasks that could be further abstracted out of the
        function.
  - [ ] Move the computation of winners and rule breakers into its own
        function
  - [ ] Make `run-round` compute whether a game is ended itself, and
        return a value that conveys that result or the next game
        state.
  - [ ] Wrapped Playable for handling exceptions and returning union
        result type (Either), for cleaner error-handling in
        `run-turn`.
  - [ ] Add global constraints to purpose statement of testing version
        of `run-game`.
- [ ] Game State:
  - [ ] Add comprehensive game state tests for scoring and taking
        turns.
  - [ ] Refactor `player-state`: define a new `player-state` substruct
        that includes the `playable` field.
  - [ ] Refactor `game-state` to use the initial `player-state` type.
    - [ ] Change the private state contract to have the new substruct.
  - [ ] Add de/serialization for private states
  - [ ] Refactor `apply-turn/exchange` to remove reused code.
- [ ] Scoring:
  - [ ] Refactor `xscore` to utilize the optional end-of-game-bonus
        parameter.
- [ ] Strategy:
  - [ ] Add more strategy tests for `dag`, `ldasg`, and helpers.
- [ ] Misc:
  - [ ] Move all in `Common/data` up a directory.
  - [ ] Move all in `Common/util` to a new directory `Lib/`.
  - [ ] Move provided contracts into definitions with
        `define/contract`.
  - [ ] Move `posn` lexicographical-sorting functionality into `posn`
        as `posn<`
  - [ ] Change `sort-tiles` into `tile<`
  - [ ] Don't export the `posn` row/column accessors, since they
        shouldn't be needed anywhere.

#### Completed

- README:
  - [X] Update the file structure. **Commit:** `1af6dc5 docs: updated README file structure`

