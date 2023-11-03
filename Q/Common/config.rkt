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
(define *timeout*   (make-parameter 60))

(define *bonus*     (make-parameter 6))
(define *points-per-q* (make-parameter 6))
