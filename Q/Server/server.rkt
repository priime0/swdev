#lang racket

(require Q/Server/player)
(require Q/Referee/referee)
(require Q/Common/player-state)
(require Q/Common/game-state)
(require Q/Common/config)
(require Q/Lib/connection)
(require Q/Lib/result)
(require Q/Lib/time)

(provide
 (struct-out server-config)
 (struct-out referee-config)
 (contract-out
  [run (-> natural? game-result?)]
  [hash->server-config (-> hash? server-config?)]
  [hash->referee-config (-> hash? referee-config?)]))

;; ========================================================================================
;; DATA DEFINITIONS
;; ========================================================================================

#; {type LobbyInfo = (server-info TcpListener [Listof Playable])}
;; A LobbyInfo represents the summary of the current lobby on the
;; server, and contains the listener on which the server accepts
;; client connections, and the server's current list of players
;; waiting in the lobby, in ascending age order.
(struct lobby-info (listener players) #:transparent)

#; {type RefereeConfig = (referee-config GameState Boolean Natural Natural Natural Boolean)}
;; Configuration for the referee.
(struct/contract referee-config
  ([state0   game-state?]
   [quiet?   boolean?]
   [qbo      natural?]
   [fbo      natural?]
   [per-turn natural?]
   [observe? boolean?])
  #:transparent)

#; {type ServerConfig = (server-config Natural Natural Natural Natural Boolean RefereeConfig)}
;; Configuration for the server.
(struct/contract server-config
  ([port            natural?]
   [server-tries    natural?]
   [server-wait     natural?]
   [wait-for-signup natural?]
   [quiet?          boolean?]
   [ref-spec        referee-config?])
  #:transparent)

#; {LobbyInfo -> [Listof Playable]}
;; Puts the lobby's players into descending age order
(define (in-descending-age info)
  (reverse (lobby-info-players info)))

#; {LobbyInfo -> [Listof Playable]}
;; Puts the lobby's players into ascending age order
(define (in-ascending-age info)
  (lobby-info-players info))

;; ========================================================================================
;; FUNCTIONALITY
;; ========================================================================================

#; {Natural -> GameResult}
;; Runs the server on the given port. The Server listens on this TCP
;; connection and tries to start a game at most twice, otherwise
;; returns an empty result.
(define (run port)
  (define reuse? #t)
  (define info0 (lobby-info (tcp-listen port (*max-players*) reuse?) '()))
  (define info1 (collect-players info0))
  (if (lobby-ready? info1)
      (run-game (in-descending-age info1))
      (list '() '())))

#; {[Listof Playable] -> GameResult}
;; Runs a game with the given list of players in the appropriate order.
(define (run-game players)
  (if (*start-state*)
      (play-game players #:game-state (*start-state*))
      (play-game players)))

#; {LobbyInfo -> LobbyInfo}
;; Collects players, if any, into the given lobby. Attempts to collect players *tries* amount of
;; times.
(define (collect-players info)
  (define info^ (box info))
  (let loop ([remaining-attempts (*tries*)])
    (cond
      [(or (zero? remaining-attempts)
           (success? (with-timeout (thunk (signup-players info^)) (*signup-timeout*)))
           (lobby-ready? (unbox info^)))
       (unbox info^)]
      [else (loop (sub1 remaining-attempts))])))

#; {[Boxof LobbyInfo] -> Void}
;; Signs up players until the lobby is full, or until the signup
;; window is ended.  If there are the minimum number of people in the
;; lobby when the signup window ends, that lobby is returned on the
;; given channel, otherwise returns false on the given channel.
(define (signup-players info)
  (let loop ()
    (cond
      [(lobby-full? (unbox info)) (void)]
      [else
       (signup-player info)
       (loop)])))


#; {[Boxof LobbyInfo] -> Void}
;; Signs up a new player to the given lobby. Waits for a player to
;; accept a connection, and then gives them at most
;; *server-client-timeout* seconds to send their name.
;; If they comply, sign them up. Otherwise, discard the connection.
(define (signup-player info)
  (define listener (lobby-info-listener (unbox info)))
  (define conn (connection-from-listener listener))
  (define maybe-json-response (conn-read/timeout conn (*server-client-timeout*)))
  (define player-name-result (jname->symbol maybe-json-response))
  (match player-name-result
    [(success player-name) (add-player! info conn player-name)]
    [(failure _)           (close-connection conn)]))


#; {[Boxof LobbyInfo] Connection Symbol -> Void}
;; Creates a new player with the given info and adds them to the given lobby.
;; EFFECT: mutates the lobby-info box to a new lobby-info with an updated list of players.
(define (add-player! info conn name)
  (match-define [lobby-info listener players] (unbox info))
  (define new-player (new player-proxy% [conn conn] [id name]))
  (define info+ (lobby-info listener (cons new-player players)))
  (set-box! info info+))

#; {LobbyInfo -> Boolean}
;; Is the given lobby full?
(define (lobby-full? info)
  (= (length (lobby-info-players info))
     (*max-players*)))

#; {LobbyInfo -> Boolean}
;; Is the given lobby ready for play? (i.e. does it have at least the
;; min number of players?)
(define (lobby-ready? info)
  (>= (length (lobby-info-players info))
      (*min-players*)))


#; {[Maybe JSExpr] -> [Result Symbol]}
;; Deserialize a received JSON response representing the jName into a
;; player name symbol, producing a failure if malformed.
(define (jname->symbol jname)
  (cond [(or (not jname)
             (not (string? jname))
             (not (player-name? (string->symbol jname))))
         (failure jname)]
        [else
         (success (string->symbol jname))]))

#; {JSExpr -> ServerConfig}
(define (hash->server-config jsexpr)
  (define required-keys '(port server-tries server-wait wait-for-signup quiet ref-spec))
  (define has-all-keys?
    (andmap (curry hash-has-key? jsexpr) required-keys))
  (unless has-all-keys?
    (error 'hash->client-config "missing fields for server config"))

  (define ref (curry hash-ref jsexpr))

  (define port (ref 'port))
  (define server-tries (ref 'server-tries))
  (define server-wait (ref 'server-wait))
  (define wait-for-signup (ref 'wait-for-signup))
  (define quiet (ref 'quiet))
  (define ref-spec (hash->referee-config (ref 'ref-spec)))

  (server-config port server-tries server-wait wait-for-signup quiet ref-spec))

#; {JSExpr -> RefereeConfig}
(define (hash->referee-config jsexpr)
  (define required-keys '(state0 quiet config-s per-turn observe))
  (define has-all-keys?
    (andmap (curry hash-has-key? jsexpr) required-keys))
  (unless has-all-keys?
    (error 'hash->client-config "missing fields for referee config"))

  (define ref (curry hash-ref jsexpr))

  (define state0 (hash->priv-state (ref 'state0)))
  (define quiet (ref 'quiet))
  (define config-s (ref 'config-s))
  (define qbo (hash-ref config-s 'qbo))
  (define fbo (hash-ref config-s 'fbo))
  (define per-turn (ref 'per-turn))
  (define observe (ref 'observe))

  (referee-config state0 quiet qbo fbo per-turn observe))


;; ========================================================================================
;; UNIT TESTS
;; ========================================================================================

(module+ test
  (require rackunit)
  (require Q/Common/map)
  (require Q/Player/player)
  (require Q/Player/dag)
  (require Q/Common/tile)

  (define playables (list (new player% [id 'andrey] [strategy (new dag%)])
                          (new player% [id 'lucas]  [strategy (new dag%)])))
  (define player-states1 (list (player-state 0 (list (tile 'blue 'square)) 'andrey #f)
                              (player-state 0 (list (tile 'blue 'circle)) 'lucas  #f)))
  (define gs1 (game-state (make-board (tile 'red 'square))
                         (list (tile 'red 'square))
                         player-states1))

  (define player-states2 (list (player-state 0 (list (tile 'blue 'circle)) 'andrey #f)
                               (player-state 0 (list (tile 'blue 'square)) 'lucas  #f)))
  (define gs2 (game-state (make-board (tile 'red 'square))
                         (list (tile 'red 'square))
                         player-states2)))

(module+ test
  (parameterize ([*start-state* gs1])
    (test-equal?
     "run simple game"
     (run-game playables)
     '(("andrey") ())))

  (parameterize ([*start-state* gs2])
    (test-equal?
     "run simple game"
     (run-game playables)
     '(("lucas") ()))))
