#lang racket

(require json)

(require Q/Common/game-state)
(require Q/Common/turn-action)
(require Q/Common/game-state)
(require Q/Common/interfaces/serializable)

(require Q/Lib/json)

(provide main)

(define (main)
  (let/ec return
    (define jpub        (read-json (current-input-port)))
    (define jplacements (read-json (current-input-port)))

    (define pub (hash->pub-state jpub))
    (define placements (map hash->placement jplacements))

    (unless (turn-valid? pub (place placements))
      (json-write+flush #f)
      (return))

    (define b+ (game-state-board (do-turn/action pub (place placements))))

    (define out-json (->jsexpr b+))

    (json-write+flush out-json)))

(module+ main
  (main))
