**TO:** Matthias Felleisen <matthias@ccs.neu.edu>, 
        Benjamin Lerner <blerner@ccs.neu.edu>

**FROM:** Andrey Piterkin <piterkin.a@northeastern.edu>, 
          Lucas Sta Maria <lucas@priime.dev>

**DATE:** 2023-11-02

**SUBJECT:** TODO

Desired tasks to complete, grouped by subject, and ordered by priority
for the subject.

#### Completed

- [x] Testing & Bug-fixing:
  - [x] Fix `valid-board?` to remove the invariant that `(0, 0)` is
        contained in the board. **Commit:** `e56c45c fix: updated
        invariants and corrected board validation`
  - [x] Make a test script that runs all previous integration tests.
        **Commit:** `0bb8e21 test: finalized integration test script`
  - [x] Fix `xscore` to remove `turn-info` references. **Commit:**
        `5bd5869 fix: xscore`
- [ ] README:
  - [x] Add a module diagram **Commit:** `85571cf docs: added a v1
        module diagram`
  - [x] Update the file structure. **Commit:** `1af6dc5 docs: updated
        README file structure`
- [ ] Referee:
  - [x] Call each player's `setup` method. **Commit:** `f4037db fix:
        referee bug fixes`
  - [x] Call each player's `win` method at the end. **Commit:**
        `fac6267 fix: referee returns players in correct oder`
  - [x] Refactor the `run-turn` function in the referee, since there
        is repeated code for error-handling, along with there being
        atomic tasks that could be further abstracted out of the
        function. **Commit:**
        `fac6267 fix: referee returns players in correct oder`
  - [x] Move the computation of winners and rule breakers into its own
        function. **Commit:** `fac6267 fix: referee returns players in
        correct oder`
  - [x] Make `run-round` compute whether a game is ended itself, and
        return a value that conveys that result or the next game
        state. **Commit:** `fac6267 fix: referee returns players in
        correct oder`
  - [x] Add global constraints to purpose statement of testing version
        of `run-game`. **Commit:** `67ca5c5 origin/main fix: remoed
        print error from with-handlers`
- [ ] Game State:
  - [x] Add de/serialization for private states. **Commit:** `7fbf83c
        feat: add hash->priv-state`
  - [x] Refactor `apply-turn/exchange` to remove reused code.
        **Commit:** `d868403 fix: change semantics for
        apply-turn/exchange`
- [x] Scoring:
  - [x] Refactor `xscore` to utilize the optional end-of-game-bonus
		parameter. **Commit:** `5bd5869 fix: xscore`
- [ ] Misc:
  - [x] Move `posn` lexicographical-sorting functionality into `posn`
        as `posn<`. **Commit:** `8b59fe7 main feat: moved posn sorting
        and comparison into posn.rkt`
  - [x] Move all in `Common/data` up a directory. **Commit:** 4ff9da1
        `refactor: move all in Common/data to Common`
  - [x] Move all in `Common/util` to a new directory `Lib/`.
        **Commit:** `f9caa8a main refactor: move all in Common/util to
        Lib/`

  
#### Incomplete

We discovered more pressing bugs while writing code and integration
tests for this milestone, so we prioritized fixing those. Namely, we
found our rule-breaking handling and other aspects of the referee to
be especially buggy.

- [ ] README:
  - [ ] ~~Add a Referee-Player protocol diagram.~~
  - [ ] ~~Add a Referee-Game-state protocol diagram.~~
  - [ ] ~~Add a Referee-Observer protocol diagram.~~
- [ ] Referee:
  - [ ] ~~Write unit tests for referee functionality.~~
  - [ ] ~~Wrapped Playable for handling exceptions and returning union
        result type (Either), for cleaner error-handling in
        `run-turn`.~~ **NOTE**: We decided to go with a different
        approach.
- [ ] Game State:
  - [ ] ~~Add comprehensive game state tests for scoring and taking
        turns.~~
  - [ ] ~~Refactor `player-state`: define a new `player-state`
        substruct that includes the `playable` field.~~ **NOTE**: This
        ended up being infeasible, since issues would arise with type
        erasure.
  - [ ] ~~Refactor `game-state` to use the initial `player-state`
        type.~~
    - [ ] ~~Change the private state contract to have the new
          substruct.~~
- [ ] Strategy:
  - [ ] ~~Add more strategy tests for `dag`, `ldasg`, and helpers.~~
  - [ ] ~~Refactor strategy helpers in `greedy` abstract to use
        `valid-*?` predicates~~
- [ ] Misc:
  - [ ] ~~Move provided contracts into definitions with
        `define/contract`.~~
  - [ ] ~~Change `sort-tiles` into `tile<`~~
  - [ ] ~~Don't export the `posn` row/column accessors, since they
        shouldn't be needed anywhere.~~
