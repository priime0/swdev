**TO:** Matthias Felleisen <matthias@ccs.neu.edu>, Benjamin Lerner <blerner@ccs.neu.edu>

**FROM:** Andrey Piterkin <piterkin.a@northeastern.edu>, Lucas Sta
Maria <lucas@priime.dev>

**DATE:** 2023-10-12

**SUBJECT:** Player-Referee Communication Protocol

#### Protocol

There are three phases of the game: set up, game play, and tear down.
During setup, a player will:

1. Player initiates communication with join request: `{"id": String,
   "age": Natural}`
2. Referee responds with `{"status": "creating-game"}`
3. Player waits for referee message `{"tiles": [Listof Tile]}` or until defined timeout

This marks the setup period, where the player registers with the
referee, giving it the age and its id (like a name), waits for an
acknowledgment, and then waits for a list of its tiles, to signify
both the beginning of a game, and to tell it of its tiles. If a player
tries to register twice, the referee will accept that and create an
additional player. The ID passed in the join request doesn't have to be
unique, but the referee will make it unique.

During game play, a player will:

1. Player receives communique from the referee: `{"map": Board, "your-state":
   PlayerState, "others": [Listof [Pair PlayerId Score]]", "history":
   History, "remaining-tiles": Natural}`
2. Player will respond with: `{"action": TurnAction}`
3. Referee will respond with `{"new-score": Natural, "new-tiles": [Listof Tiles]}`,
   or `{"message": "invalid-turn"}`, denoting that the player has made a
   mistake and has been kicked out.
4. Player will enter a waiting loop until this current player's next
   turn, or a user defined timeout

The communication between a player and a referee during normal play looks like
a wait loop, where the player waits for its next turn, gets the necessary info,
sends back a turn, and gets its new score and tiles, or receives a message that
it has been terminated for an invalid action, and will either stop or wait for
its next turn. If a player sends a message to a referee while another
player is taking their turn, or when the referee isn't expecting a
response, the referee will ignore the message. 

During teardown, a player will:

1. Receive a communique from the referee: `{"final-map": Board,
   "final-scores": [Listof [Pair PlayerId Natural]], "winner":
   PlayerId}` 
2. Stop, because the game has ended.

This marks the end of communication.
Note that this protocol ONLY describes the ordering of communication, as well as the
data transfer required for playing and refereeing a game. The representation of
messages with the syntax `{...}` only serves to describe that data sharing.
This could be implemented either with function calls, i.e.
`player.takeTurn(map, your-state, others, ...)` or JSON messages of the above
style.
