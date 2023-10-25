The commit we tagged for your submission is 043f8e1c3333ce5309d6372925c7dee39b1266fb.
**If you use GitHub permalinks, they must refer to this commit or your self-eval will be rejected.**
Navigate to the URL below to create permalinks and check that the commit hash in the final permalink URL is correct:

https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/tree/043f8e1c3333ce5309d6372925c7dee39b1266fb

## Self-Evaluation Form for Milestone 5

Indicate below each bullet which piece of your code takes care of each task:

1. a data definition (inc. interpretation) for the result of a strategy

https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/043f8e1c3333ce5309d6372925c7dee39b1266fb/Q/Common/data/turn-action.rkt#L41-L51

2. the `dag` strategy 

https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/043f8e1c3333ce5309d6372925c7dee39b1266fb/Q/Player/dag.rkt#L10-L19

3. the `ldasg` strategy 

https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/043f8e1c3333ce5309d6372925c7dee39b1266fb/Q/Player/ldasg.rkt#L11-L21

4. a data definition (inc. interpretation) for the result of a strategy iterator

https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/043f8e1c3333ce5309d6372925c7dee39b1266fb/Q/Player/iterative.rkt#L8-L37

5. unit tests for the `dag` strategy
   - one for a 'pass' decision
   
   We did not test this functionality.
   
   - one for a 'replace all tiles' decision

   We did not test this functionality.

   - one for a 'place this tile there' decision

https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/043f8e1c3333ce5309d6372925c7dee39b1266fb/Q/Player/iterative.rkt#L69-L75

6. unit tests for the `ldaag` strategy
   - one for a 'pass' decision

   We did not test this functionality

   - one for a 'replace all tiles' decision

   We did not test this functionality

   - one for a 'place this tile there' decision

   https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/043f8e1c3333ce5309d6372925c7dee39b1266fb/Q/Player/ldasg.rkt#L50-L54

7. unit tests for the strategy iteration functionality 
   - one for a 'pass' decision

   We did not test this.

   - one for a 'replace all tiles' decision

   We did not test this.

   - one for a _sequence of_ 'place this tile there' decision

   https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/043f8e1c3333ce5309d6372925c7dee39b1266fb/Q/Player/iterative.rkt#L69-L75

8. how does your design abstract the common strategy iteration functionality 

   Our iterative strategy is a composition strategy, i.e. it takes in a strategy and applies it until right before the total turn is no longer valid. https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/043f8e1c3333ce5309d6372925c7dee39b1266fb/Q/Player/iterative.rkt#L8-L37

9. does your design abstract the common search through the sorted tiles?
   (for a bonus)
   
   We defined a greedy strategy base class  https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/043f8e1c3333ce5309d6372925c7dee39b1266fb/Q/Player/greedy-select-strategy.rkt#L13-L41 
   that chooses the first tile with `choose-tile` (a helper defined here: https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/043f8e1c3333ce5309d6372925c7dee39b1266fb/Q/Player/strategy.rkt#L53-L63 ) which selects the smallest tile. Then, we utilize an abstract function called `get-compare-accessor-list` implemented in base classes to get the comparators we need to decide how to sort the positions a smallest tile could be placed in. For example, in `dag`, we define the `get-compare-accessor-list` funciton as follows:
https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/043f8e1c3333ce5309d6372925c7dee39b1266fb/Q/Player/dag.rkt#L17-L19
Here, we see that we sort the possible positions a tile could be placed at in row-column order. In `ldsag`, we do it by most contrained, and then row-column order: 
https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/043f8e1c3333ce5309d6372925c7dee39b1266fb/Q/Player/ldasg.rkt#L18-L21
   
The ideal feedback for each of these points is a GitHub perma-link to
the range of lines in a specific file or a collection of files.

A lesser alternative is to specify paths to files and, if files are
longer than a laptop screen, positions within files are appropriate
responses.

You may wish to add a sentence that explains how you think the
specified code snippets answer the request.

If you did *not* realize these pieces of functionality, say so.


