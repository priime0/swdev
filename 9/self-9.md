The commit we tagged for your submission is e87dfe75d5f768a6cc00a8c6654bf7641c7c7cff.
**If you use GitHub permalinks, they must refer to this commit or your self-eval will be rejected.**
Navigate to the URL below to create permalinks and check that the commit hash in the final permalink URL is correct:

https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/tree/e87dfe75d5f768a6cc00a8c6654bf7641c7c7cff

## Self-Evaluation Form for Milestone 9

Indicate below each bullet which file/unit takes care of each task.

For `Q/Server/player`,

- explain how it implements the exact same interface as `Q/Player/player`
- explain how it receives the TCP connection that enables it to communicate with a client
- point to unit tests that check whether it writes (proper) JSON to a mock output device

For `Q/Client/referee`,

- explain how it implements the same interface as `Q/Referee/referee`
- explain how it receives the TCP connection that enables it to communicate with a server
- point to unit tests that check whether it reads (possibly broken) JSON from a mock input device

For `Q/Client/client`, explain what happens when the client is started _before_ the server is up and running:

- does it wait until the server is up (best solution)
- does it shut down gracefully (acceptable now, but switch to the first option for 10)

For `Q/Server/server`, explain how the code implements the two waiting periods. 

The ideal feedback for each of these three points is a GitHub
perma-link to the range of lines in a specific file or a collection of
files.

A lesser alternative is to specify paths to files and, if files are
longer than a laptop screen, positions within files are appropriate
responses.

You may wish to add a sentence that explains how you think the
specified code snippets answer the request.

If you did *not* realize these pieces of functionality, say so.

