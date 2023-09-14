**TO:** Matthias Felleisen <matthias@ccs.neu.edu>

**FROM:** Andrey Piterkin <piterkin.a@northeastern.edu>, Lucas Sta Maria
<lucas@priime.dev>

**CC:** Benjamin Lerner <blerner@ccs.neu.edu>

**DATE:** 2023-09-12

**SUBJECT:** Sprint Allocation

## Sprint 1:

**GOAL**: Define all the essential game types needed for describing a
game state, along with the rules that govern interactions with the game
state.

This includes basic data representations, such as tiles and the board,
along with the referee data representation and game state validation
functions.

## Sprint 2:

**GOAL**: Define player interactions with the game states and with the
referee.

It's necessary to have a representable game state to apply actions on,
plus a referee to enforce rules that govern the game states. It was also
reasonable to place the player data representation and implementation in
this sprint to adequately portion work into roughly equivalent chunks.

## Sprint 3:

**GOAL**: Implement client-server communication for players and
observers.

Players are required to exist prior to communicating with the server. 

