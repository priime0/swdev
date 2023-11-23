#lang racket

(require Q/Server/player)
(require Q/Referee/referee)
(require Q/Common/player-state)
(require Q/Common/config)
(require Q/Lib/connection)

(require racket/engine)

(provide
 (contract-out
  [run (-> natural? (list/c (listof string?) (listof string?)))]))

;; ========================================================================================
;; DATA DEFINITIONS
;; ========================================================================================

#; {type LobbyInfo = (server-info TcpListener [Listof Playable])}
;; A LobbyInfo represents the summary of the current lobby on the
;; server, and contains the listener on which the server accepts
;; client connections, and the server's current list of players
;; waiting in the lobby, in ascending age order.
(struct lobby-info (listener players) #:transparent)

;; ========================================================================================
;; FUNCTIONALITY
;; ========================================================================================

#; {Natural -> GameResult}
;; Runs the server on the given port. The Server listens on this TCP
;; connection and tries to start a game at most twice, otherwise
;; returns an empty result.
(define (run port)
  (parameterize ([current-custodian (make-custodian)])
    (define info0 (lobby-info (tcp-listen port) '()))
    (define info1 (collect-players info0))
    (define result
      (if (lobby-empty? info1)
          (list '() '())
          (play-game (reverse (lobby-info-players info1)))))
    (custodian-shutdown-all (current-custodian))
    result))


#; {LobbyInfo -> LobbyInfo}
;; Collects players into the given lobby.
(define (collect-players info)
  (define info^ (box info))
  (define signup-engine (engine (thunk* (signup-players info^))))
  (define timeout-ms (* *signup-timeout* 1000))
  (let loop ([remaining-attemps *tries*])
    (cond
      [(zero? remaining-attemps) info]
      [(engine-run timeout-ms signup-engine) (unbox info^)]
      [(lobby-ready? (unbox info^)) (unbox info^)]
      [else (loop (sub1 remaining-attemps))])))

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
  (define maybe-name (conn-read/timeout conn *server-client-timeout*))
  (cond
    [(not maybe-name) (close-connection conn)]
    [(not (player-name? (string->symbol maybe-name))) (close-connection conn)]
    [else (add-player info conn (string->symbol maybe-name))]))


#; {[Boxof LobbyInfo] Connection Symbol -> Void}
;; Creates a new player with the given info and adds them to the given lobby.
;; EFFECT: mutates the list of players in the given lobby-info
(define (add-player info conn name)
  (match-define [lobby-info listener players] (unbox info))
  (define new-player (new player-proxy% [conn conn] [id name]))
  (define info+ (lobby-info listener (cons new-player players)))
  (set-box! info info+))

#; {LobbyInfo -> Boolean}
;; Is the given lobby full?
(define (lobby-full? info)
  (= (length (lobby-info-players info))
     *max-players*))

#; {LobbyInfo -> Boolean}
;; Is the given lobby ready for play? (i.e. does it have at least the
;; min number of players?)
(define (lobby-ready? info)
  (>= (length (lobby-info-players info))
      *min-players*))

#; {LobbyInfo -> Boolean}
;; Is the given lobby empty?
(define (lobby-empty? info)
  (null? (lobby-info-players info)))
