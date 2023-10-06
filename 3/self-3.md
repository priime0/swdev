The commit we tagged for your submission is e206fff462623a4c4de09a4568438f54d959b3e9.
**If you use GitHub permalinks, they must refer to this commit or your self-eval will be rejected.**
Navigate to the URL below to create permalinks and check that the commit hash in the final permalink URL is correct:

https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/tree/e206fff462623a4c4de09a4568438f54d959b3e9

## Self-Evaluation Form for Milestone 3

Indicate below each bullet which method takes care of each task:

1. 'referee state to public knowledge'

https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/e206fff462623a4c4de09a4568438f54d959b3e9/Q/Common/game-state.rkt#L164-L175

2. 'completing a turn action' 

https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/e206fff462623a4c4de09a4568438f54d959b3e9/Q/Common/game-state.rkt#L267-L277
   
3. 'checking legality of proposed sequence of placements'
   plus its unit tests

We included this function, but did not write unit tests for it, so the following is a permalink just to the function.
https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/e206fff462623a4c4de09a4568438f54d959b3e9/Q/Common/game-state.rkt#L240-L258

4. The 'checking legality of proposed sequence of placements'
   functionality clearly performs three different checks: 
   - 'that the player owns the tiles it wishes to place'
      https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/e206fff462623a4c4de09a4568438f54d959b3e9/Q/Common/game-state.rkt#L253-L254
   - 'that all placements are either in the same row xor the same column'
      https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/e206fff462623a4c4de09a4568438f54d959b3e9/Q/Common/game-state.rkt#L248-L250
   - 'that all placed tiles fit into the board', sequentially 
      https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/e206fff462623a4c4de09a4568438f54d959b3e9/Q/Common/game-state.rkt#L251-L252
   - Indicate how these checks are factored out into separate methods/functions.
      They are factored out into separate functions, `posns-same-row?`, `posns-same-column?`, which are defined in `posn.rkt`, `contains-all?`, which is in `util/list.rkt`, 
      and we utilized `valid-placement?` for each individual placement on the board (defined in `map.rkt`), and use an andmap to check that all placements are valid.
      Then, we combine those results in this function using composition.

      `posns-same-row?`: https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/e206fff462623a4c4de09a4568438f54d959b3e9/Q/Common/data/posn.rkt#L95-L98

      `posns-same-column?`: https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/e206fff462623a4c4de09a4568438f54d959b3e9/Q/Common/data/posn.rkt#L100-L103

      `contains-all?`: https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/e206fff462623a4c4de09a4568438f54d959b3e9/Q/Common/util/list.rkt#L38-L46

      `valid-placement?`: https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/e206fff462623a4c4de09a4568438f54d959b3e9/Q/Common/map.rkt#L144-L165

   
The ideal feedback for each of these points is a GitHub
perma-link to the range of lines in a specific file or a collection of
files.

A lesser alternative is to specify paths to files and, if files are
longer than a laptop screen, positions within files are appropriate
responses.

You may wish to add a sentence that explains how you think the
specified code snippets answer the request.

If you did *not* realize these pieces of functionality, say so.


