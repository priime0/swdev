#lang racket

(require threading)

(provide (all-defined-out))

;;;; TILE RENDERING

;; Determines the rendering size in pixels for each tile.
(define *game-size* (make-parameter 40))

;; Controls whether tiles will be rendered as outlines or solids.
(define *tile-shape-mode* (make-parameter 'solid))

;; The background color of the board
(define *background-color* (make-parameter 'white))

;;;; GAME CONFIGURATION

;; Determines the maximum size of a hand a player can hold.
(define *hand-size* (make-parameter 6))

;; Determine timeout in seconds for a player to take their turn
(define *timeout*   (make-parameter 6))

(define *bonus*     (make-parameter 6))
(define *points-per-q* (make-parameter 6))

;; SERVER CONFIGURATION

;; the maximum amount of time a server will wait for a client to
;; supply their name after accepting their connection in seconds
(define *server-client-timeout* 3)

;; the maximimum amount of time a server will wait to acquire the
;; minimum number of players for a game in seconds
(define *signup-timeout* 20)

;; number of times to try and collect a lobby of players
(define *tries* 2)

;; the min and max number of players in a game
(define *min-players* 2)
(define *max-players* 4)


(define *obman*
  (let ([observer-manager% (dynamic-require 'Q/Referee/observer 'observer-manager%)])
    (make-parameter (new observer-manager%))))
