**If you use GitHub permalinks, make sure your link points to the most recent commit before the milestone deadline.**

## Self-Evaluation Form for Milestone 2

Indicate below each bullet which file/unit takes care of each task:

1. does your implementation come with a separate data representationn for tiles?

Yes.

https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/84658a9bc1c5359fb3d4d110dce37a853d3dbca1/Q/Common/data/tile.rkt#L30-L54
   
2. does your implementation come with a separate data representation for map coordinates?
   - does it include an interpretation statement?
   
Yes and yes.

Typo exists: RHS was meant to be `[Pairof Integer]`.

https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/84658a9bc1c5359fb3d4d110dce37a853d3dbca1/Q/Common/map.rkt#L29-L30
   
3. does your functionality for creating and extending maps come with
   signatures and purpose statements?

Yes.

https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/84658a9bc1c5359fb3d4d110dce37a853d3dbca1/Q/Common/map.rkt#L66

4. does your functionality for "determining all those places where a
   specific tile can be inserted so that it fits according to the
   matching rules of The Q Game" come with a signature and purpose
   statement? does it factor out the following, separate pieces of
   functionality?

    - finding all feasible places meaning free neighbor tiles
    
    Yes.
    
https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/84658a9bc1c5359fb3d4d110dce37a853d3dbca1/Q/Common/map.rkt#L130-L143

    - determining all (four) neighbors of a spot
    
    Yes.

https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/84658a9bc1c5359fb3d4d110dce37a853d3dbca1/Q/Common/map.rkt#L122-L136    

    - checking the basic "fits" rule from the Q description
    
    Yes.
    
https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/84658a9bc1c5359fb3d4d110dce37a853d3dbca1/Q/Common/map.rkt#L154-L170

#### Notes 

Remember that if you think the name of a method/function is _totally
obvious_, there is no need for a purpose statement.

The ideal feedback for each of these points is a GitHub perma-link to
the range of lines in a specific file or a collection of files.

A lesser alternative is to specify paths to files and, if files are
longer than a laptop screen, positions within files are appropriate
responses.

You may wish to add a sentence that explains how you think the
specified code snippets answer the request.

If you did *not* realize these pieces of functionality, say so.


