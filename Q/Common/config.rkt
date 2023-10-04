#lang racket

(provide (all-defined-out))

;;;; TILE RENDERING

;; Determines the rendering size in pixels for each tile.
(define *game-size* (make-parameter 40))

;; Controls whether tiles will be rendered as outlines or solids.
(define *tile-shape-mode* (make-parameter 'solid))


;;;; GAME CONFIGURATION

;; Determines the maximum size of a hand a player can hold.
(define *hand-size* (make-parameter 6))
