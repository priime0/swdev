**TO:** Matthias Felleisen <matthias@ccs.neu.edu>, 
        Benjamin Lerner <blerner@ccs.neu.edu>

**FROM:** Andrey Piterkin <piterkin.a@northeastern.edu>, 
          Lucas Sta Maria <lucas@priime.dev>

**DATE:** 11/09/2023

**SUBJECT:** Distributed Game of Q

### Server-Referee Protocol

#### Gathering Players

We specify a new protocol to support distributed games of Q to be
played, with the option of remote players participating. In this
protocol, we define several new components: the `Client`, the `Server`,
the `PlayerProxy`, and the `RefereeProxy`.

The gathering step is the process in which the server "gathers" players
to partake in the game.

The server's protocol is defined as follows:

1. The Server listens on its connection for any incoming play requests
   (*signup requests*).
2. The server waits for a prespecified amount of time. The server will
   start the game when the maximum amount of players (4) have joined the
   game OR there are at least two players and the prespecified amount of
   time has passed. If the server receives a valid request, it will
   notify the connection with a *signup response*. If the server
   receives an invalid request, it will notify the connection with a
   *signup failure notification*. Valid incoming player requests are
   specified below.
2. Once the internal player buffer contains enough players, it
   constructs a ProxyPlayer (defined later) for each connection and
   dispatches to a new thread for the referee to play the game with
   those proxy players, sorted by age in descending order.
3. After the referee is constructed, the server will go to Step 1 and
   repeat.

The client protocol is defined as follows:

1. Instantiate a client with their player of choice, and supply it with
   the destination for JSON communication.
2. The Client will send its signup message to the Server.
3. The Client will wait until its internal timeout, or until the Server
   responds with a game setup call to the proxy player.
4. The game will start.

A valid *signup request* looks like the following:

```json
{
    "request": "signup",
    "name": "matthias",
    "age": 5
}
```

In particular, a valid signup request adheres to the following
constraints:

1. It contains the `"request"` field that has the string value
   `"signup"`.
2. It contains the `"name"` field, with a string value that matches the
   regular expression `^[a-zA-Z0-9]+$`. The length of the value must be
   at least one character, and at most 20 (inclusive).
3. It contains the `"age"` field, whose value is an *non-negative
   integer* most the maximum value of a 32-bit integer.
   
After received a valid signup request, the server dispatches a *signup
response* notification to the player to indicate their valid
registration. A *signup response* has a singular field `"response"` that
has the value `"success"`. A *signup response* looks like the following:

```json
{
    "response": "success"
}
```

If the server received an invalid signup request, it will dispatch a
*signup failure notification* that looks like the following:

```json
{
    "response": "failure",
    "message": "<some message>"
}
```

The *signup failure notification* has a field `"response"` with the value
`"failure"`, and a field `"message"` with a string value that describes
the reason for the failure. In particular, the `"message"` field must
be one of the following:

1. `"invalid-name"`: The provided name is invalid.
2. `"invalid-age"`: The provided age is invalid.
3. `"duplicate-name"`: The provided name was already taken by another
   registering player.
4. `"game-started"`: The game has already started, so the server will no
    longer accept new players.
    

#### Launching and Running a Game

Upon startup, the server will wait for a prespecified amount of time
for players to join.

The server will start the game in one of two conditions:

1. The server has received at least two valid player signups, and the
   prespecified amount of time has passed.
2. The server has received four valid player signups -- regardless of
   the amount of time that has passed.
   
The server will then proceed to create the game via the Referee. In
order to do this, both the Server and the Client require **proxies** to
facilitate communication. The proxies allow the referee to communicate
with remote players, and vice versa. Since a `player-proxy%` implements
the `playable<%>` interface, the referee can treat it as if it were a
local player -- there are no changes to the referee's implementation.

The `player-proxy%` class implements the `playable<%>` interface, so it
behaves as if it were a normal player. Under the hood, the
`player-proxy%` is constructed with a connection provided by the Server,
which it will write to and read from to communicate that a player needs
to perform some action or to inform the referee of a result,
respectively.

The `referee-proxy%` is for clients to communicate with the server. The
`referee-proxy%` is constructed with a connection provided by the
client, which it will write to and read from to communicate that a
player needs to perform some action or to inform the referee of a
result, respectively. The `referee-proxy%` are how clients receive JSON
messages (following the JSON specifications) from the server connection,
using common parsing functions to deserialize the JSON into their
respective objects. The clients can then act on the deserialized object,
execute their expected actions, serialize the result, and sends the
serialized JSON to the server via the `referee-proxy%`.


##### The Server & Client

Broadly, the server and client act as the "referee" and "player", except
over the network. The server contains the referee and runs games, and
the clients contain players, and allow for communication with the
server.

We can imagine the client as having the interface:

```scheme

(define client<%>
   (interface ()
      #; {Playable Connection -> Client}
      ;; Create a client with the given player and the given connection
      ;; the server
      [create-client player connection]

      #; {Client -> Void}
      ;; Listens on the connection, running the game to completion.
      [listen]

```


#### Shutdown

Once a game has ended, and the referee has notified players and
connections of their results, the referee must shut down. The referee
will send a shutdown message to all players, and then close all
connections. The referee will then terminate. The server will clean up
remaining connections, and then reopen the server for new games.

After the game has ended, the server may wish to punish the misbehavers.
This could be done by retrieving the list of misbehavers from the
referee, then preventing future connections from players with the same
name or address. This could be achieved in the form of a blacklist.
