#lang racket

(require json)

(require Q/Common/game-state)
(require Q/Common/turn-action)
(require Q/Common/game-state)
(require Q/Common/interfaces/serializable)

(define jpub        (read-json (current-input-port)))
(define jplacements (read-json (current-input-port)))

(define pub (hash->pub-state jpub))
(define placements (map hash->placement++ jplacements))

(unless (turn-valid? pub (place placements))
  (write-json #f)
  (displayln "")
  (flush-output)
  (exit))

(define b+ (game-state-board (do-turn/action pub (place placements))))

(define out-json (->jsexpr b+))

(write-json out-json (current-output-port))
(displayln "")
(flush-output)
