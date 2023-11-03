**TO:** Matthias Felleisen <matthias@ccs.neu.edu>, 
        Benjamin Lerner <blerner@ccs.neu.edu>

**FROM:** Andrey Piterkin <piterkin.a@northeastern.edu>, 
          Lucas Sta Maria <lucas@priime.dev>

**DATE:** 2023-10-26

**SUBJECT:** Predicted Difficulty of Prospective Changes

- Changing the bonus points again and allow more players to
  participate in the game
  
1/5

We already have existing code to easily change the bonus points. We
don't have contracts to enforce a maximum amount of players, it is
fairly trivial to introduce another parameter and contracts on the
game state.

- Adding wildcard tiles

3/5

Changes in the `tile` module would be straightforward: tile sorting
and tile sets would change. Placing and scoring would be considerably
more difficult, with changes being made to how tiles can be placed and
how Qs and scores are determined.

- Imposing restrictions that enforce the rules of Qwirkle instead of Q

5/5

"Rulebook" predicate functions, which are scattered in different
modules, would need to completely change. Scoring logic, contained in
the game state module, would need to completely change. We did not
make use of first-class modules to make rules and scoring flexible.
