**TO:** Matthias Felleisen <matthias@ccs.neu.edu>, Benjamin Lerner <blerner@ccs.neu.edu>

**FROM:** Andrey Piterkin <piterkin.a@northeastern.edu>, Lucas Sta
Maria <lucas@priime.dev>

**DATE:** 11/09/2023

**SUBJECT:** Distributed Game of Q

### Server-Referee Protocol

#### Gathering Players

In order to support remote players, we will need some new mechanisms. In
our protocol, we will define several new components: the `Client`, the
`Server`, the `PlayerProxy`, and the `RefereeProxy`.

During the gathering step, the protocol will go as follows:
1. The Server listens on its connection for any incoming play requests,
   until enough players have been gathered for a game. Valid incoming
   player requests will contains names and ages, and invalid requests
   will be ignored.
2. Once the internal player buffer contains enough players, it
   constructs a ProxyPlayer (defined later) for each connection and
   dispatches to a new thread for the referee to play the game with
   those proxy players, sorted by age in descending order.
3. After the referee is constructed, the server will go to Step 1 and
   repeat.

On the Client side, clients of this program must:
1. Instantiate a client with their player of choice, and supply it with
   the destination for JSON communication.
2. The Client will send its signup message to the Server.
3. The Client will wait until its internal timeout, or until the Server
   responds with a game setup call to the proxy player.
4. The game will start.

#### Launching and Running a Game

As mentioned above, once the Server collections enough players during
its Gathering Players loop, it will create a game via the Referee. In
order to do this, both the Server and the Client require **proxies** to
facilitate communication.

The PlayerProxy implements the `playable<%>` interface, so it behaves as
if it were a normal player. Under the hood, the PlayerProxy is
constructed with a connection provided by the Server, which it will
write to and read from to communicate that a player needs to perform
some action or to inform the referee of a result, respectively.

The RefereeProxy lives on the side of the Client. Its job is to
translate the instructions rece



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
