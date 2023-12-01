#lang racket

(require threading)

(provide (all-defined-out))

;; =============================================================================
;;;; TILE RENDERING
;; =============================================================================

;; Determines the rendering size in pixels for each tile.
(define *game-size* (make-parameter 40))

;; Controls whether tiles will be rendered as outlines or solids.
(define *tile-shape-mode* (make-parameter 'solid))

;; The background color of the board
(define *background-color* (make-parameter 'white))

;; =============================================================================
;;;; GAME CONFIGURATION
;; =============================================================================

;; Determines the maximum size of a hand a player can hold.
(define *hand-size* (make-parameter 6))

;; Determine timeout in seconds for a player to take their turn
(define *timeout*   (make-parameter 6))

;; =============================================================================
;; SERVER + CLIENT CONFIGURATION
;; =============================================================================

#; {10000 <= Natural <= 60000}
;; Corresponds with "port" field of ServerConfig and ClientConfig spec.
(define *port* (make-parameter 10000))

;; =============================================================================
;; SERVER CONFIGURATION
;; =============================================================================

#; {Natural < 10}
;; The number of times to try and collect a lobby of players.
;; Corresponds with "server-tries" field in ServerConfig spec.
(define *tries* (make-parameter 2))

#; {Natural < 30}
;; The maximimum amount of time a server will wait to acquire the
;; minimum number of players for a game in seconds.
;; Corresponds with "server-wait" field in ServerConfig spec.
(define *signup-timeout* (make-parameter 20))

#; {Natural < 10}
;; The maximum amount of time a server will wait for a client to
;; supply their name after accepting their connection in seconds.
;; Corresponds with "wait-for-signup" field in ServerConfig spec.
(define *server-client-timeout* (make-parameter 3))

#; {Boolean}
;; Whether the server should mute output to standard out.
(define *server-quiet?* (make-parameter #t))

;; =============================================================================
;; CLIENT CONFIGURATION
;; =============================================================================

#; {String}
;; A valid hostname string.
;; Corresponds with "host" field in ClientConfig spec.
(define *hostname* (make-parameter "127.0.0.1"))

#; {Natural < 10}
;; Corresponds with "wait" field in ClientConfig spec.
(define *wait* (make-parameter 6))

#; {Boolean}
;; Corresponds with "quiet" field in ClientConfig spec.
(define *client-quiet?* (make-parameter #t))

;; =============================================================================
;; REFEREE CONFIGURATION
;; =============================================================================

#; {[Maybe GameState]}
;; Corresponds with "state0" field in RefereeConfig spec.
(define *start-state* (make-parameter #f))

#; {Boolean}
;; Corresponds with "quiet" field in RefereeConfig spec.
(define *referee-quiet?* (make-parameter #t))

#; {Natural <= 10}
;; End of game bonus
;; Corresponds with "fbo" field in RefereeStateConfig spec.
(define *bonus*     (make-parameter 6))

#; {Natural <= 10}
;; Q bonus
;; Corresponds with "qbo" field in RefereeStateConfig spec.
(define *points-per-q* (make-parameter 6))

#; {Natural <= 6}
;; Corresponds with "per-turn" in RefereeConfig spec.
(define *per-turn* (make-parameter 6))

#; {Natural where *min-players* <= *max-players*}
;; The min and max number of players in a game
(define *min-players* (make-parameter 2))
(define *max-players* (make-parameter 4))

#; {Observable}
;; The observer manager, who accepts observers.
(define *obman*
  (let ([observer-manager% (dynamic-require 'Q/Referee/observer 'observer-manager%)])
    (make-parameter (new observer-manager%))))
