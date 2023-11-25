The commit we tagged for your submission is e87dfe75d5f768a6cc00a8c6654bf7641c7c7cff.
**If you use GitHub permalinks, they must refer to this commit or your self-eval will be rejected.**
Navigate to the URL below to create permalinks and check that the commit hash in the final permalink URL is correct:

https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/tree/e87dfe75d5f768a6cc00a8c6654bf7641c7c7cff

## Self-Evaluation Form for Milestone 9

Indicate below each bullet which file/unit takes care of each task.

For `Q/Server/player`,

- explain how it implements the exact same interface as `Q/Player/player`

It extends our `playable<%>` interface, whose purpose statement states that:

> Any player must implement this functionality to play a game of Q.

https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/e87dfe75d5f768a6cc00a8c6654bf7641c7c7cff/Q/Server/player.rkt#L16

- explain how it receives the TCP connection that enables it to communicate with a client

It has a field that represents the TCP connection called `conn`.
`conn` is a *struct* that we implemented that abstracts over the TCP
connection.

Field:

https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/e87dfe75d5f768a6cc00a8c6654bf7641c7c7cff/Q/Server/player.rkt#L18

`connection` struct:

https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/e87dfe75d5f768a6cc00a8c6654bf7641c7c7cff/Q/Lib/connection.rkt#L18-L20

- point to unit tests that check whether it writes (proper) JSON to a mock output device

https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/e87dfe75d5f768a6cc00a8c6654bf7641c7c7cff/Q/Server/player.rkt#L63-L110

For `Q/Client/referee`,

- explain how it implements the same interface as `Q/Referee/referee`

In `Q/Referee/referee`, our referee has a single public function `run-game` that takes in multiple players. In `Q/Client/referee`, we implement the "same interface" because it has a single function that takes in a player.

https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/e87dfe75d5f768a6cc00a8c6654bf7641c7c7cff/Q/Client/referee.rkt#L31-L39

- explain how it receives the TCP connection that enables it to communicate with a server

A `connection` struct representing the TCP connection is passed into the "run" (`listen`) function.

- point to unit tests that check whether it reads (possibly broken) JSON from a mock input device

We wrote unit tests, but did not cover the "possibly broken JSON" case.

https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/e87dfe75d5f768a6cc00a8c6654bf7641c7c7cff/Q/Client/referee.rkt#L75-L164

For `Q/Client/client`, explain what happens when the client is started _before_ the server is up and running:

- does it wait until the server is up (best solution)
- does it shut down gracefully (acceptable now, but switch to the first option for 10)

It fails and errors, since the TCP connection does not exist. Connection is constructed here:

https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/e87dfe75d5f768a6cc00a8c6654bf7641c7c7cff/Q/Client/client.rkt#L15-L20

Which uses this `connection` construction function:

https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/e87dfe75d5f768a6cc00a8c6654bf7641c7c7cff/Q/Lib/connection.rkt#L43-L48

Which clearly does not handle waiting for the server to open up.

For `Q/Server/server`, explain how the code implements the two waiting periods.

We use [racket/engine](https://docs.racket-lang.org/reference/engine.html#%28def._%28%28lib._racket%2Fengine..rkt%29._engine%29%29).

https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/e87dfe75d5f768a6cc00a8c6654bf7641c7c7cff/Q/Server/server.rkt#L46-L70

When players are signing up, we create an engine that signs up players and times out if enough haven't signed up. If 4 players have signed up, then the `signup-players` function returns early. If the engine times out and the lobby is ready, then we start the game. If the engine times out and the lobby is not ready, then we retry (a finite amount of times).


The ideal feedback for each of these three points is a GitHub
perma-link to the range of lines in a specific file or a collection of
files.

A lesser alternative is to specify paths to files and, if files are
longer than a laptop screen, positions within files are appropriate
responses.

You may wish to add a sentence that explains how you think the
specified code snippets answer the request.

If you did *not* realize these pieces of functionality, say so.

