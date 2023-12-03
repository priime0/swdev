**TO:** Matthias Felleisen <matthias@ccs.neu.edu>, 
        Benjamin Lerner <blerner@ccs.neu.edu>

**FROM:** Andrey Piterkin <piterkin.a@northeastern.edu>, 
          Lucas Sta Maria <lucas@priime.dev>

**DATE:** 2023-12-03

**SUBJECT:** Configurations

This memo documents the changes made to configuring client, server,
referee, and scoring functionality.

The significant changes were made in 
[this commit](https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/commit/8471b4c6a8c972ed2f8eaf8b6956c762ccbcad8f).

The changes involved:

- Adding more Racket parameters for configuring the server: `*tries*`,
  `*server-client-timeout*`, and `server-quiet?`.
- Adding more Racket parameters for configuring the client:
  `*hostname*`, `*wait*`, and `client-quiet?`.
- Adding more Racket parameters for configuring the referee:
  `*start-state*`, `*per-turn*`, and `referee-quiet?`.
  
In our scripts, these are first deserialized into config objects,
added with these commits:

- https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/commit/22df3e059125c00ccd0378129923ba9b0c496bd0
- https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/commit/aa19fa97364f8ef3cef76c82da1209553f55e82e

In the `xserver` and `xclient` scripts, configs are read from STDIN,
deserialized into the config objects, and then *parameterized* such
that these global variables use the config object's values while
running the game.

https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/main/10/xserver.rkt
https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/blob/main/10/xclients.rkt

The rest of this code uses these parameters as global variables. We
added some code to accomodate these new parameters in this commit:

https://github.khoury.northeastern.edu/CS4500-F23/fearless-mice/commit/95adab4c50fb130378503d440a2b410f07611a39

We've needed to make relatively few changes, since we've been using
parameters significantly since the beginning -- `Q/Common/config.rkt`
has existed for a while.
