#lang racket

(require Q/Lib/connection)
(require Q/Client/referee)

;; ========================================================================================
;; FUNCTIONALITY
;; ========================================================================================

#; {String PortNumber Playable -> Void}
;; Connects to the server and signs up the player, participating in a game.
(define (start hostname port playable)
  (define conn (connect hostname port playable))
  (sign-up conn playable)
  (listen conn playable))


#; {Connection Playable -> Void}
;; Signs up to the server through the given connection, retrieving and sending the player's name.
(define (sign-up conn playable)
  (define name (send playable name))
  (conn-write conn name))
