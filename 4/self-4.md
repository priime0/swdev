The commit we tagged for your submission is ebbe9b8e9c46446d3f09d94d99433123b5e6bf5e.
**If you use GitHub permalinks, they must refer to this commit or your self-eval will be rejected.**
Navigate to the URL below to create permalinks and check that the commit hash in the final permalink URL is correct:

https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/tree/ebbe9b8e9c46446d3f09d94d99433123b5e6bf5e

## Self-Evaluation Form for Milestone 4

Indicate below each bullet which method takes care of each task:

1 'rendering the referee state' 

Yes, although we forgot to write an out contract for this piece of
functionality:
https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/ebbe9b8e9c46446d3f09d94d99433123b5e6bf5e/Q/Common/game-state.rkt#L337-L347

2. 'scoring a placement' 

The function that does this is the following:

https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/ebbe9b8e9c46446d3f09d94d99433123b5e6bf5e/Q/Common/game-state.rkt#L315-L320

It doesn't operate on game state, but is called here with the relevant
info (and is not external functionality): https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/ebbe9b8e9c46446d3f09d94d99433123b5e6bf5e/Q/Common/game-state.rkt#L215  

3. The 'scoring a placement' functionality clearly performs four different checks: 

We made this one long function, with the goal of eventually neatly
separating these concerns, but for now, we put them in one and clearly
labeled each check.

  - 'length of placement'

https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/ebbe9b8e9c46446d3f09d94d99433123b5e6bf5e/Q/Common/game-state.rkt#L281-L282

  - 'bonus for finishing'

https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/ebbe9b8e9c46446d3f09d94d99433123b5e6bf5e/Q/Common/game-state.rkt#L307-L310

  - 'segments extended along the line (row, column) of placements'

For this bullet, and for the next, we realized that it was unnecessary
to separate the sequences that a newly placed tile extends into along the
axis of the placements, and along the orthogonals. We can just collect
all sequences interacted with into a set, and given that each sequence
is always ordered the same, will give us all unique sequences.

https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/ebbe9b8e9c46446d3f09d94d99433123b5e6bf5e/Q/Common/game-state.rkt#L281-L291

  - 'segments extended orthogonal to the line (row, column) of placements'

https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/ebbe9b8e9c46446d3f09d94d99433123b5e6bf5e/Q/Common/game-state.rkt#L281-L291

  - indicate which of these are factored out into separate
    methods/functions and where.

  We factor out the sequence finding into a helper:

https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/ebbe9b8e9c46446d3f09d94d99433123b5e6bf5e/Q/Common/game-state.rkt#L270-L275

which relies on this helper in the board: https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/ebbe9b8e9c46446d3f09d94d99433123b5e6bf5e/Q/Common/map.rkt#L257-L270
   
The ideal feedback for each of these points is a GitHub perma-link to
the range of lines in a specific file or a collection of files.

A lesser alternative is to specify paths to files and, if files are
longer than a laptop screen, positions within files are appropriate
responses.

You may wish to add a sentence that explains how you think the
specified code snippets answer the request.

If you did *not* realize these pieces of functionality, say so.


