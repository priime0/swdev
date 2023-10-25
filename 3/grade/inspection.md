Pair: fearless-mice \
Commit: [e206fff462623a4c4de09a4568438f54d959b3e9](https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/tree/e206fff462623a4c4de09a4568438f54d959b3e9) \
Self-eval: https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/8b0d6e906a1428aefca3aead35d6e2b8a4dc656e/3/self-3.md \
Score: 65/80 \
Grader: Can Ivit

## Self Eval [20/20]
Thank you for honest, helpful, and clear self evaluation.

## Programming [30/40]
- [-10] No unit tests for 'checking legality of proposed sequence of placements'

## Design [15/20]
- [-5] Player does not have a function to accept tiles. According to the game description, the referee hands the player as many tiles as it placed.
- [+5] The developers provided a very good justification on why they don't have the function explained above and earned bonus points. 
- [-5] A player should know other players' order of play. `TurnInfo` data defnition does not include this information.
