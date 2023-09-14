**TO:** Matthias Felleisen <matthias@ccs.neu.edu>

**FROM:** Andrey Piterkin <piterkin.a@northeastern.edu>, Lucas Sta Maria
<lucas@priime.dev>

**CC:** Benjamin Lerner <blerner@ccs.neu.edu>

**DATE:** 2023-09-12

**SUBJECT:** Questions Concerning the Game

For this document, we define an *intersection* to be the board square
adjacent to both a row sequence and column sequence. 

For instance, `X`
represents the intersection, where a tile has not yet been placed.

As an example, consider the following 3x3 board, where `.` represents an
empty square, `C` represents tiles of the column sequence, `R`
represents tiles of the row sequence, and `X` represents an empty tile
at *an* intersection (there exist 2 in this example).

```
..C
..C
RRX
```

1. Is the tile placed at the intersection constrained by both sequences?

There are several possible interpretations of the rules. One, is that
the placed tile at the intersection can satisfy either the adjacent row
tile or adjacent column tile's constraints. Second, the placed tile at
the intersection must satisfy both adjacent tiles. Making this explicit
would be useful in the implementation of the referee.

2. For a tile placed in an intersection, are points assigned for both
   sequences?

3. How are points assigned when an existing sequence is extended?

There are several possible interpretations of the rules. One, points are
assigned based on the length of the existing sequence. Two, points are
assigned based on the length of the added sequence. Lastly, points are
assigned based on the cumulative lengths of the existing and added
sequences. According to the rules, points are already added for each
tile placed, so we want to clarify whether points are doubly counted for
the added sequence.
