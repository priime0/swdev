The commit we tagged for your submission is 54be7e079aa77d101ba3327c2b2fba18d3f0c835.
**If you use GitHub permalinks, they must refer to this commit or your self-eval will be rejected.**
Navigate to the URL below to create permalinks and check that the commit hash in the final permalink URL is correct:

https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/tree/54be7e079aa77d101ba3327c2b2fba18d3f0c835

## Self-Evaluation Form for Milestone 8

Indicate below each bullet which file/unit takes care of each task:

- concerning the modifications to the referee: 

  - is the referee programmed to the observer's interface
    or is it hardwired?

The referee communicates with the `*obman*` parameter, which contains
an *observer manager* that implements the `observer<%>` interface. The
referee only uses methods specified by the `observer<%>` interface,
but we don't write an explicit constraint on the parameter that its
value has to implement the interface. This allows us to optionally
wire in actual observers, meaning that no observation is required.

Referee communicating with the observer:

https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/54be7e079aa77d101ba3327c2b2fba18d3f0c835/Q/Referee/referee.rkt#L77-L79

Observer parameter (note the lack of constraint that `*obman*` is an
implementation of the `observer<%>` interface):

https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/54be7e079aa77d101ba3327c2b2fba18d3f0c835/Q/Common/config.rkt#L29-L31

  - if an observer is desired, is every state per player turn sent to
    the observer? Where? 

The state after the turn is collected and then sent to the observer
manager:

https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/54be7e079aa77d101ba3327c2b2fba18d3f0c835/Q/Referee/referee.rkt#L151-L152

  - if an observer is not desired, how does the referee avoid calls to
    the observer?
    
__We chose a different design decision__. The referee *does not* avoid
calls to the observer manager. However, the observer manager manages
observers that connect to it, dispatching any received game states to
the observers it manages. So, if an observer is not desired, then the
observer manager does not have observers, and the referee's calls to
the observer manager are not directed to any observers.

https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/54be7e079aa77d101ba3327c2b2fba18d3f0c835/8/xgames-with-observer.rkt#L25-L28

- concerning the implementation of the observer:

  - does the purpose statement explain how to program to the
    observer's interface? 

Yes, concisely. The purpose statement tells a potential implementer 
everything they need to know, except for one thing we missed: the observers
and observer manager are run in the main thread, which we should either state,
or provide some protections against, so that implementers of the interface
know to run non-blocking visualizations.

https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/54be7e079aa77d101ba3327c2b2fba18d3f0c835/Q/Referee/observer.rkt#L19-L30

  - does the purpose statement explain how a user would use the
    observer's view? Or is it explained elsewhere? 

It does not. Purpose statement for the concrete visual observer here:

https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/54be7e079aa77d101ba3327c2b2fba18d3f0c835/Q/Referee/visual-observer.rkt#L15-L17

The ideal feedback for each of these three points is a GitHub
perma-link to the range of lines in a specific file or a collection of
files.

A lesser alternative is to specify paths to files and, if files are
longer than a laptop screen, positions within files are appropriate
responses.

You may wish to add a sentence that explains how you think the
specified code snippets answer the request.

If you did *not* realize these pieces of functionality, say so.

