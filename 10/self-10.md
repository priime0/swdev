The commit we tagged for your submission is 7131ad18454dc7c8cedc17b1820b13a6da0de7b2.
**If you use GitHub permalinks, they must refer to this commit or your self-eval will be rejected.**
Navigate to the URL below to create permalinks and check that the commit hash in the final permalink URL is correct:

https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/tree/7131ad18454dc7c8cedc17b1820b13a6da0de7b2

## Self-Evaluation Form for Milestone 10

Indicate below each bullet which file/unit takes care of each task.

The data representation of configurations clearly needs the following
pieces of functionality. Explain how your chosen data representation 

- implements creation within programs _and_ from JSON specs 

We implement the configurations as a set of pre-defined global Racket parameters defined here:
https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/7131ad18454dc7c8cedc17b1820b13a6da0de7b2/Q/Common/config.rkt#L20-L119

These parameters are created at compile time, and can have their values set via the Racket `parameterize` body, which is where we actually use these configurations, for example, here in xclients: https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/7131ad18454dc7c8cedc17b1820b13a6da0de7b2/10/xclients.rkt#L24-L30

 We also facilitate creation of configurations from JSON specs defined here by defining a struct that contains these fields:
https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/7131ad18454dc7c8cedc17b1820b13a6da0de7b2/Q/Client/client.rkt#L15-L27
and a deserialization function for this Client config:
https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/7131ad18454dc7c8cedc17b1820b13a6da0de7b2/Q/Client/client.rkt#L68-L85

Here for the data definitions for the Server and Referee configuations:
https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/7131ad18454dc7c8cedc17b1820b13a6da0de7b2/Q/Server/server.rkt#L31-L51
and here are the deserialization functions for them: 
https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/7131ad18454dc7c8cedc17b1820b13a6da0de7b2/Q/Server/server.rkt#L154-L190

- enforces that each configuration specifies a fixed set of properties (no more, no less)

We enforce that each configuration specifies a fixed set of properties, as opposed to using something like a key-value map, by having a set of explicitly-defined configuration constants defined in the config, as mentioned above, and as well by defining the Client, Server, and Referee configurations as structs (also linked above), ensuring that those are the only parameters/config constants in the program.

- supports the retrieval of properties 

Since we chose to define all of the configurable settings as Racket parameters, which we provide out of `Q/Common/config.rkt`, any module that imports that submodule will have access to the constants via "de-referencing", as shown here, for example, in server.rkt: https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/7131ad18454dc7c8cedc17b1820b13a6da0de7b2/Q/Server/server.rkt#L105-L112 on line 108 where we access the timeout for a player name. We have also placed all of the purposes/data definitions for the parameters in `Q/Common/config.rkt`. 

We retrieve the properties from the configs (Referee, Server, Client) in the xscripts themselves, where we read in the configuration and parameterize the relevant parts: https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/7131ad18454dc7c8cedc17b1820b13a6da0de7b2/10/xserver.rkt#L20-L35

- sets properties (what happens when the property shouldn't exist?) 

Similarly as to the last question, we set the properties in the relevant xscript, like so: https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/7131ad18454dc7c8cedc17b1820b13a6da0de7b2/10/xserver.rkt#L20-L35

We chose to do it in this fashion because it is idiomatic, and because this ensures that the properties must exist, and that there can never be an instance where the property shouldn't exist.

- unit tests for these pieces of functionality

These are unit tests for setting and retrieving properties in our program, in `Q/Common/game-state.rkt`, as an example:
https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/7131ad18454dc7c8cedc17b1820b13a6da0de7b2/Q/Common/game-state.rkt#L515-L559
We set the properties with the `parameterize` body, and when we call `do-turn/score`, its helpers access the properties here:
https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/7131ad18454dc7c8cedc17b1820b13a6da0de7b2/Q/Common/game-state.rkt#L309-L322

We have similar unit tests for the referee starting state:
This is where `*start-state*`--the starting JState option from the config--is accessed: https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/7131ad18454dc7c8cedc17b1820b13a6da0de7b2/Q/Server/server.rkt#L68-L73

And this is where we test the parameterization: https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/7131ad18454dc7c8cedc17b1820b13a6da0de7b2/Q/Server/server.rkt#L197-L229

Explain how the server, referee, and scoring functionalities are abstracted
over their respective configurations.

Because of our choice for design--namely, having all of the game options exist as Racket parameters in our `config.rkt` file--all of our referee, server, and scoring functionality access those parameters in current dynamic scope directly, meaning that even though they have default values as defined in the config, client programs can set those parameters to be different for the duration of their runtime. Thus, all of our scoring, server, referee, and client functions don't rely on arguments, but rather directly access the current parameter values. For example, here the server access the start state, which is false if there is none, or a private state if one was set: https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/7131ad18454dc7c8cedc17b1820b13a6da0de7b2/Q/Server/server.rkt#L68-L73

Does the server touch the referee or scoring configuration, other than
passing it on?

No, the server only touches four parameters: 
https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/7131ad18454dc7c8cedc17b1820b13a6da0de7b2/Q/Server/server.rkt#L70-L73
https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/7131ad18454dc7c8cedc17b1820b13a6da0de7b2/Q/Server/server.rkt#L79-L82
https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/7131ad18454dc7c8cedc17b1820b13a6da0de7b2/Q/Server/server.rkt#L108

Does the referee touch the scoring configuration, other than passing
it on?

No, the referee only touches two parameters:
This one directly, which is our observer parameter: https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/7131ad18454dc7c8cedc17b1820b13a6da0de7b2/Q/Referee/referee.rkt#L84
and this one, indirectly: https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/7131ad18454dc7c8cedc17b1820b13a6da0de7b2/Q/Lib/macros.rkt#L45-L55
The latter is timed call to the player, which utilizes the player timeout paarameter.


The ideal feedback for each of these three points is a GitHub
perma-link to the range of lines in a specific file or a collection of
files.

A lesser alternative is to specify paths to files and, if files are
longer than a laptop screen, positions within files are appropriate
responses.

You may wish to add a sentence that explains how you think the
specified code snippets answer the request.

If you did *not* realize these pieces of functionality, say so.

