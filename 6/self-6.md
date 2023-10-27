The commit we tagged for your submission is 585015b5c8f9ba8ab7685f3d237136def2c044f2.
**If you use GitHub permalinks, they must refer to this commit or your self-eval will be rejected.**
Navigate to the URL below to create permalinks and check that the commit hash in the final permalink URL is correct:

https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/tree/585015b5c8f9ba8ab7685f3d237136def2c044f2

## Self-Evaluation Form for Milestone 6

Indicate below each bullet which piece of your code takes care of each task:

1. the five pieces of player functionality

   https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/585015b5c8f9ba8ab7685f3d237136def2c044f2/Q/Player/player.rkt#L13-L38
   
   Because our player is stateless, a lot of the functionality does nothing. 

2. `setting up players` functionality in the referee component 

    We forgot to include this functionality. Our referee only creates the game state, which stores the player states and the players themselves:
    https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/585015b5c8f9ba8ab7685f3d237136def2c044f2/Q/Referee/referee.rkt#L26, https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/585015b5c8f9ba8ab7685f3d237136def2c044f2/Q/Common/player-state.rkt#L74 
    Though we forgot to call `setup` on these players.

3. `running a game` functionality in the referee component

https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/585015b5c8f9ba8ab7685f3d237136def2c044f2/Q/Referee/referee.rkt#L24-L42

4. `managing a round` functionality in the referee component
    (This must be factored out to discover the end-of-game condition.)
    
    https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/585015b5c8f9ba8ab7685f3d237136def2c044f2/Q/Referee/referee.rkt#L58-L69

5. `managing an individual turn` functionality in the referee component

https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/585015b5c8f9ba8ab7685f3d237136def2c044f2/Q/Referee/referee.rkt#L71-L106

6. `informing survivors of the outcome` functionality in the referee component

We correctly return the list of winners and rulebreakers here: https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/585015b5c8f9ba8ab7685f3d237136def2c044f2/Q/Referee/referee.rkt#L42
But we forgot to call the method `win` on all of the players. This would be an easy change to implement, by adding a new function that gets called inside the main function in referee that sends the results before returning.

7. unit tests for the `referee`:

   - five distinct unit tests for the overall `referee` functionality

    We only had two unit tests: https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/585015b5c8f9ba8ab7685f3d237136def2c044f2/Q/Referee/referee.rkt#L148-L167 testing for both a round with one winner and testing with a rule-breaking strategy that gets kicked out.

   - unit tests for the abvove pieces of functionality 

    We did not unit test the individual pieces of functionality.
   
8. the explanation of what is considered ill-behaved player and how the referee deals with it.

https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/585015b5c8f9ba8ab7685f3d237136def2c044f2/Q/Referee/referee.rkt#L14-L23

The ideal feedback for each of these points is a GitHub perma-link to
the range of lines in a specific file or a collection of files.

A lesser alternative is to specify paths to files and, if files are
longer than a laptop screen, positions within files are appropriate
responses.

You may wish to add a sentence that explains how you think the
specified code snippets answer the request.

If you did *not* realize these pieces of functionality, say so.


