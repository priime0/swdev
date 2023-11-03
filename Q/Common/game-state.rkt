#lang racket

(require (rename-in (only-in lazy define)
                    [define define/lazy]))
(require racket/set)
(require racket/generic)

(require math/base)

(require struct-plus-plus)
(require threading)
(require 2htdp/image)
(require predicates)
(require json)

(require Q/Common/map)
(require Q/Common/player-state)
(require Q/Common/config)
(require Q/Common/turn-action)
(require Q/Common/tile)
(require Q/Common/posn)
(require Q/Lib/list)
(require Q/Lib/image)
(require Q/Lib/misc)
(require Q/Common/interfaces/serializable)
(require Q/Common/interfaces/playable)

(provide
 game-state/c
 pub-state/c
 priv-state/c
 hash->pub-state
 hash->priv-state
 render-game-state
 apply-players
 (struct-out game-state)
 end-turn
 (contract-out
  [any-players?
   (-> game-state/c boolean?)]
  [final-ranking
   (-> priv-state? boolean?)]
  [make-game-state
   (->i ([tiles (listof tile?)] [players (listof (is-a?/c playable<%>))])
        #:pre/name (tiles players)
        "not enough tiles!"
        (< (add1 (* (length players)
                    (*hand-size*)))
           (length tiles))
        [result priv-state/c])]
  [priv-state->pub-state
   (->i ([gs priv-state?])
        #:pre/name (gs)
        "no players left!"
        (any-players? gs)
        [result pub-state/c])]
  [apply-turn
   (->i ([gs game-state/c] [t turn-action?])
        #:pre/name (gs)
        "no players left!"
        (any-players? gs)
        [result game-state/c])]
  [remove-player
   (->i ([gs priv-state?])
        #:pre/name (gs)
        "no players left!"
        (any-players? gs)
        [result priv-state/c])]
  [valid-turn?
   (-> game-state/c turn-action? boolean?)]
  [score-turn
   (-> game-state/c turn-action? natural?)]
  [new-tiles
   (-> priv-state/c turn-action? (listof tile?))]
  #;
  [end-turn
   (-> priv-state/c turn-action?
       #:new-points natural?
       #:tiles-given natural?
       priv-state/c)]
  [turn-ends-game?
   (-> game-state/c
       turn-action?
       boolean?)]))


#; {type PrivateState = (game-state Board
                                    [Dequeof Tile]
                                    [Dequeof PlayerState])}
;; A PrivateState represents the omnisicient knowledge of the state of
;; a game at any instant of time, containing the board's current state
;; at that instant, along with the list of remaining tiles, and the
;; player states in order of turns to take.

#; {Any -> Boolean}
(define (priv-state? a)
  (and (game-state? a)
       (unprotected-board/c (game-state-board a))
       ((listof tile?) (game-state-tiles a))
       ((listof player-state?) (game-state-players a))))

(define priv-state/c (flat-named-contract 'priv-state/c priv-state?))

#; {type PublicState = (game-state Board
                                   Natural                                  
                                   [List PlayerState Natural ...])}
;; A PublicState represents the public knowledge a player needs to
;; take their turn, containing the board, the number of remaining
;; tiles, and a list of the current player's private
;; state and the public states of the other players.

#; {Any -> Boolean}
(define (pub-state? a)
  (and (game-state? a)
       (unprotected-board/c (game-state-board a))
       (natural? (game-state-tiles a))
       ((cons/c player-state? (listof natural?)) (game-state-players a))))

(define pub-state/c (flat-named-contract 'pub-state/c pub-state?))

#; {type GameState = (U PrivateState
                        PublicState)}
;; A GameState is one of a PrivateState or a PublicState.
(define game-state/c (or/c pub-state/c priv-state/c))

(struct++ game-state
          ([board    unprotected-board/c]
           [tiles    (or/c natural? (listof tile?))]
           [players  (or/c (cons/c player-state? (listof natural?))
                           (listof player-state?))])
          #:transparent)


#; {GameState -> Boolean}
;; Are there any players left?
(define (any-players? gs)
  ((length>? 0) (game-state-players gs)))

#; {([Listof Tile] -> [Listof Tile]) GameState -> GameState}
;; Updates the tiles of the given game state with the provided procedure
(define (apply-tiles f gs)
  (set-game-state-tiles gs (f (game-state-tiles gs))))

#; {(Board -> Board) GameState -> GameState}
;; Updates the board of the given game state with the provided procedure
(define (apply-board f gs)
  (set-game-state-board gs (f (game-state-board gs))))

#; {([Listof PlayerState] -> [Listof PlayerState]) GameState -> GameState}
;; Updates the player state queue of the given game state with the provided procedure
(define (apply-players f gs)
  (set-game-state-players gs (f (game-state-players gs))))

#; {[Listof Tile] [Listof Playable] -> PrivateState}
;; Creates a fresh game state with the given referee tile, set of tiles, and the playables.
;; ASSUME: the players are sorted by age in non-increasing order.
(define (make-game-state start-tiles playables)
  (match-define [cons start-tile rest-tiles] (shuffle start-tiles))
  (define handout-size           (* (length playables) (*hand-size*)))
  (define-values (handout tiles) (split-at rest-tiles handout-size))

  (define players
    (~>> handout
         (segment (*hand-size*))
         (map make-player-state _ playables)))
  
  (game-state++ #:board (make-board start-tile)
                #:tiles tiles
                #:players players))


#; {PrivateState -> PublicState}
;; Produce the public state for the current player to make a decision from, using the given game state.
(define (priv-state->pub-state gs)
  (match-define [game-state board tiles [cons state others]] gs)
  (define scores (map player-state-score others))
  (game-state board (length tiles) (cons state scores)))

#; {GameState TurnAction -> TurnAction}
;; Is the given action on the given game state this turn valid?
(define (valid-turn? gs action)
  (match-define [game-state board tiles [cons state others]] gs)
  (define hand      (player-state-hand state))
  (define available (if (priv-state? gs)
                        (length tiles)
                        tiles))

  (match action
    [(place pments) (valid-place? board hand pments)]
    [(exchange)     (valid-exchange? available (length hand))]
    [(pass)         #t]))

#; {PrivateState  -> PrivateState}
;; Remove the current player from the game state.
(define (remove-player gs)
  (define player (first (game-state-players gs)))
  (match-define [player-state score hand _] player)

  (~>> gs
       (apply-tiles (curryr append hand))
       (apply-players rest)))

#; {PrivateState TurnAction -> [Listof Tile]}
;; Gets the new tiles of the current player.
(define (new-tiles gs action)
  (define tiles* (game-state-tiles gs))
  (define hand-size (length (player-state-hand (first (game-state-players gs)))))
  (match action
    [(place pments)  (take tiles* (min (length tiles*) (length pments)))]
    [(exchange)      (take tiles* hand-size)]
    [_               (error 'new-tiles "new tiles requested on a pass")]))


#; {GameState TurnAction -> Boolean}
;; Does the given turn end the game?
(define (turn-ends-game? gs action)
  (define curr-player (first (game-state-players gs)))
  (and (place? action)
       (= (length (player-state-hand curr-player))
          (length (place-placements action)))))

#; {GameState TurnAction -> GameState}
;; Apply the turn action to the game state.
(define (apply-turn gs action)
  (match action
    [(place placements)  (apply-turn/placement gs placements)]
    [(exchange)          (apply-turn/exchange gs)]
    [(pass)              gs]))

#; {GameState [Listof TilePlacement] -> GameState}
;; Places the given list of placements on the board, removes them
;; from the current player's hand, and gives the player as many new ones as possible,
;; *WITHOUT* removing them from the referee's tiles.
(define (apply-turn/placement gs placements)
  (match-define [game-state board tiles [cons state other-players]] gs)
  (define board+        (add-tiles board placements))
  (define placed-tiles  (map placement-tile placements))
  (define state+        (remove-from-hand state placed-tiles))
  (define state++       (if (priv-state? gs)
                            (refill-hand state+ tiles (length placed-tiles))
                            state+))

  (game-state board+ tiles (cons state++ other-players)))



#; {GameState -> GameState}
;; Removes the current player's tiles from their hand, and adds them
;; back to the pool, and gives out new tiles *WITHOUT* removing them
;; from the referee's tiles.
(define (apply-turn/exchange gs)
  (match-define [game-state board tiles [cons state others]] gs)
  (define hand    (player-state-hand state))
  (define state+  (set-player-state-hand state '()))

  (define tiles+  (if (priv-state? gs)
                      (append tiles hand)
                      (+ tiles (length hand))))
  (define state++ (if (priv-state? gs)
                      (exchange-hand state+ tiles+)
                      state+))

  (game-state board tiles+ (cons state++ others)))



#; {Board Hand [Listof TilePlacement] -> Boolean}
;; Is the proposed placement for the given turn valid?
(define (valid-place? board hand placements)
  (define/lazy aligned?
    (same-axis? (map placement-posn placements)))
  (define/lazy in-hand?
    (contains-all? hand (map placement-tile placements)))

  (and (pair? placements)
       aligned?
       (valid-placements? board placements)
       in-hand?))


#; {Natural Natural -> Boolean}
;; Is an exchange a valid move with this many tiles remaining?
(define (valid-exchange? num-tiles length-hand)
  (>= num-tiles length-hand))


#; {GameState TurnAction -> Natural}
;; Score the given action for the given turn.
(define (score-turn gs action)
  (define board (game-state-board gs))
  (match action
    [(place pments) (score/placement board pments)]
    [_              0]))


#; {Board [Listof TilePlacement] -> Natural}
;; Score the given placements for this turn
(define (score/placement board pments)
  (define base-points   (length pments))
  (define seq-points    (score/sequences board pments))
  (define qs-points     (score/qs board pments))

  (+ base-points seq-points qs-points))

#; {Sequence -> Boolean}
;; Is the given sequence a Q?
(define (q-sequence? seq)
  (define tiles  (map placement-tile seq))
  (define colors (map tile-color tiles))
  (define shapes (map tile-shape tiles))
  (or (same-elements? colors tile-colors)
      (same-elements? shapes tile-shapes)))


#; {Board [Listof TilePlacement] -> [Listof [Listof TilePlacement]]}
;; Produce a list of sequences for each posn along every axis.
(define (get-sequences b placements)
  (define posns (map placement-posn placements))
  (define seqs
    (for*/set ([p posns] [axis axes])
      (collect-sequence b p axis)))
  (set->list seqs))

#; {Board [Listof TilePlacement] -> Natural}
;; Score the placements for the 2nd scoring rule: points added for
;; extending existing sequences
(define (score/sequences b pments)
  (~>> (get-sequences b pments)
       (filter-not (compose one? length))
       (map length)
       sum))

#; {Board [Listof TilePlacement] -> Natural}
;; Score the Qs generated by the placements on the board
(define (score/qs b pments)
  (~>> (get-sequences b pments)
       (filter q-sequence?)
       length
       (* (*points-per-q*))))

#; {PrivateState TurnAction -> PrivateState}
;; Ends the current turn. 
(define (end-turn gs action
                  #:new-points  [new-points 0]
                  #:tiles-given [tiles-given 0])
  (match-define [game-state board tiles [cons player others]] gs)
  (define player+ (add-points player new-points))
  (define tiles+  (drop tiles tiles-given))

  (~>> gs
       (set-game-state-players _ (cons player+ others))
       (set-game-state-tiles _ tiles+)
       (apply-players rotate-left-1)))

#; {PrivateState -> [Listof Natural]}
;; Gets the final ranking of all players, sorted in descending order of score.
(define (final-ranking gs)
  (~>> gs
       game-state-players
       (map player-state-score)
       (sort _ >)))

#; {JPub -> PublicState}
(define (hash->pub-state jpub)
  (define jmap (hash-ref jpub 'map))
  (define tile* (hash-ref jpub 'tile*))
  (define players (hash-ref jpub 'players))
  (define jplayer (first players))
  (define scores* (rest players))

  (define state (hash->player-state++ jplayer))
  (define board (hash->board++ jmap))
  (game-state board tile* (cons state scores*)))

#; {JState -> PrivateState}
(define (hash->priv-state jpub)
  (define jmap (hash-ref jpub 'map))
  (define tile* (hash-ref jpub 'tile*))
  (define jplayers (hash-ref jpub 'players))

  (define board (hash->board++ jmap))
  (define tiles (map hash->tile++ tile*))
  (define players (map hash->player-state++ jplayers))

  (game-state board tiles players))

#; {GameState -> Image}
(define (render-game-state gs)
  (match-define [game-state board tiles players] gs)
  (define states-image
    (for/fold ([img empty-image])
              ([ps players])
      (beside img
              (render-player-state ps)
              (empty-space 20 20))))
  (define board-image (render-board board))
  (above board-image (empty-space 20 20) states-image))


#;
(module+ test
  (require rackunit)
  (require Q/Lib/test)
  (define tile-product (cartesian-product tile-colors tile-shapes))
  (define tile-set (map (curry apply tile) tile-product))
  (define players1 '(lucas andrey luke))
  (define players2 '(lucas andrey luke john))
  (define gs1 (apply/seed 0 make-game-state tile-set players1))
  (define gs2 (apply/seed 0 make-game-state tile-set players2))
  (define tile-set-seeded (apply/seed 0 shuffle tile-set))


  #;
  (define gs1+ (take-turn gs1 (place (list (placement (posn 1 0) (tile 'green 'clover))
                                           (placement (posn 1 1) (tile 'blue 'clover))))))

  #;
  (define gs1++ (take-turn gs1+ (place (list (placement (posn 1 2) (tile 'red 'clover))
                                             (placement (posn 1 -1) (tile 'green 'star))
                                             (placement (posn 1 -2) (tile 'yellow 'star))
                                             (placement (posn 1 -3) (tile 'blue 'star))
                                             (placement (posn 1 -4) (tile 'blue 'circle)))))))

#;
(module+ test
  (test-case
   "make-game-state with 3 players"
   (match-define [game-state board tiles players] gs1)
   (check-equal? board
                 (make-board (first tile-set-seeded))
                 "board is created with one tile")

   (check-equal? (length players)
                 (length players1)
                 "players queue has the same length contains 3 players")
   (check-equal? (map player-state-id players)
                 players1
                 "players queue contains all players in the order given")
   (check-equal? (map player-state-score players)
                 (make-list (length players1) 0)
                 "players start with 0 points")
   (check-equal? (map (compose length player-state-hand) players)
                 (make-list (length players1) (*hand-size*))
                 "players start with *hand-size* tiles in hand")


   (define all-player-tiles (map player-state-hand players))
   (define board-tile       (placement-tile (first (collect-sequence board (posn 0 0) vertical-axis))))
   (define all-gs-tiles     (append (flatten all-player-tiles)
                                    tiles
                                    (list board-tile)))
   (check same-elements?
          all-gs-tiles
          tile-set-seeded
          "all game state tiles is the same set as all tiles passed into game state"))

  (test-case
   "make-game-state with 4 players"
   (match-define [game-state board tiles players] gs2)
   (check-equal? board
                 (make-board (first tile-set-seeded))
                 "board is created with one tile")

   (check-equal? (length players)
                 (length players2)
                 "players queue has the same length contains 3 players")
   (check-equal? (map player-state-id players)
                 players2
                 "players queue contains all players in the order given")
   (check-equal? (map player-state-score players)
                 (make-list (length players2) 0)
                 "players start with 0 points")
   (check-equal? (map (compose length player-state-hand) players)
                 (make-list (length players2) (*hand-size*))
                 "players start with *hand-size* tiles in hand")


   (define all-player-tiles (map player-state-hand players))
   (define board-tile       (placement-tile (first (collect-sequence board (posn 0 0) vertical-axis))))
   (define all-gs-tiles     (append (flatten all-player-tiles)
                                    tiles
                                    (list board-tile)))
   (check same-elements?
          all-gs-tiles
          tile-set-seeded
          "all game state tiles is the same set as all tiles passed into game state"))


  (test-case
   "priv state -> pub state"
   (match-define [game-state board tiles [cons curr-player others]] gs1)
   (check-equal? (priv-state->pub-state gs1)
                 (game-state++  #:players (cons curr-player (map player-state->pair others))
                                #:board board
                                #:tiles (length tiles))))
  (test-case
   "remove player"
   (match-define [game-state board tiles players] gs1)
   (define new-tiles (append tiles (player-state-hand (first players))))
   (check-equal? (remove-player gs1)
                 (game-state board new-tiles (rest players))
                 "removing lucas returns his tiles and removes him from the queue")))


#;
(module+ test
  (require rackunit)

  (define hand1  (list (tile 'red 'square)
                       (tile 'blue 'clover)
                       (tile 'blue 'square)
                       (tile 'green 'star)
                       (tile 'green '8star)
                       (tile 'orange 'diamond)))
  (define ps1 (player-state '|0| 0 hand1))
  (define b1  (~>> (make-board (tile 'red 'diamond))
                   (add-tile _ (placement (posn 0 1) (tile 'red 'circle)))
                   (add-tile _ (placement (posn 1 1) (tile 'blue 'circle)))))

  ;;;; Example turn infos
  (define ti1 (turn-info ps1 '() '() b1 10))
  (define ti2 (turn-info ps1 '() '() b1 5))
  (define ti3 (turn-info ps1 '() '() b1 6))

  (define all-placement (list (placement (posn 0 2) (tile 'red 'square))
                              (placement (posn 0 3) (tile 'blue 'clover))
                              (placement (posn 0 4) (tile 'blue 'square))
                              (placement (posn 0 5) (tile 'green 'star))
                              (placement (posn 0 6) (tile 'green '8star))
                              (placement (posn 0 7) (tile 'orange 'diamond))))
  (define ti+ (set-turn-info-board ti1 (add-tiles b1 all-placement)))

  ;;;; Example turn actions
  (define passa (pass))
  (define excha (exchange))
  ;; invalid empty placement
  (define empl '())
  ;; valid placement
  (define vpl (list (placement (posn 0 2) (tile 'red 'square))
                    (placement (posn 1 2) (tile 'blue 'square))))
  ;; placement with tile not in hand
  (define not-hand (list (placement (posn 0 2) (tile 'red 'square))
                         (placement (posn 1 2) (tile 'blue 'circle))))
  ;; placement with tiles not in same axis
  (define bad-axis (list (placement (posn 0 2) (tile 'red 'square))
                         (placement (posn 2 0) (tile 'blue 'clover))))
  ;; illegal placement by matching rules
  (define illpl (list (placement (posn 0 2) (tile 'red 'square))
                      (placement (posn 1 2) (tile 'blue 'clover))))

  ;; new turn info, after applying vpl
  (define ti1+ (set-turn-info-board ti1 (add-tiles b1 vpl)))

  ;; board with two unfinished q's
  (define b2 (~> (make-board (tile 'red 'diamond))
                 (add-tiles (list (placement (posn 0 1) (tile 'red 'circle))
                                  (placement (posn 0 2) (tile 'red 'clover))
                                  (placement (posn 0 3) (tile 'red 'star))
                                  (placement (posn 0 4) (tile 'red '8star))
                                  (placement (posn 1 0) (tile 'green 'diamond))
                                  (placement (posn 2 0) (tile 'blue 'diamond))
                                  (placement (posn 2 1) (tile 'blue '8star))
                                  (placement (posn 2 2) (tile 'blue '8star))
                                  (placement (posn 2 3) (tile 'blue 'square))
                                  (placement (posn 2 4) (tile 'blue 'circle))
                                  (placement (posn 2 -1) (tile 'blue 'star))))))
  (define ti4 (turn-info ps1 '() '() b2 10))
  (define q-placements (list (placement (posn 0 5) (tile 'red 'square))
                             (placement (posn 1 5) (tile 'blue 'square))
                             (placement (posn 2 5) (tile 'blue 'clover))))
  (define b2+ (add-tiles b2 q-placements))
  (define ti4+ (set-turn-info-board ti4 b2+)))

#;
(module+ test

  (test-false
   "exchange when ref has < 6 tiles"
   (valid-exchange? ti2))

  (test-true
   "exchange when ref has = 6 tiles"
   (valid-exchange? ti3))

  (test-true
   "exchange when ref has > 6 tiles"
   (valid-exchange? ti1))

  (test-true
   "valid placement is legal"
   (legal-placements? ti1 vpl))

  (test-false
   "tile doesn't match placement rules, therefore is invalid"
   (legal-placements? ti1 illpl))

  (test-false
   "tile not in hand is invalid action"
   (valid-place? ti1 not-hand))

  (test-false
   "empty place is an invalid action"
   (valid-place? ti1 empl))

  (test-false
   "tiles not aligned on axis is an invalid action"
   (valid-place? ti1 bad-axis))

  (test-false
   "placement not matching neighbor-wise q matching is invalid"
   (valid-place? ti1 illpl))

  (test-true
   "non-empty matching placement along same axis in hand is valid"
   (valid-place? ti1 vpl))

  (test-true
   "valid placement is a valid action"
   (valid-action? ti1 (place vpl)))

  (test-true
   "pass is a valid action"
   (valid-action? ti1 passa))

  (test-true
   "exchange with >= 6 tiles is a valid action"
   (valid-action? ti1 excha))

  (test-false
   "exchange with < 6 tiles is an invalid action"
   (valid-action? ti2 excha))

  (test-false
   "empty placement is an invalid action"
   (valid-action? ti1 (place empl)))

  (test-false
   "placement with tiles not in hand is invalid"
   (valid-action? ti1 (place not-hand)))

  (test-false
   "placement with tiles that dont match neighbors is an invalid action"
   (valid-action? ti1 (place illpl)))

  (test-false
   "placement not aligned along 1 axis is invalid"
   (valid-action? ti1 (place bad-axis)))

  ;;;; SCORING TESTS

  (test-equal?
   "test scoring for a valid placement"
   (score-turn ti1+ (place vpl))
   9)

  (test-equal?
   "exchange is worth 0 points"
   (score-turn ti1 excha)
   0)

  (test-equal?
   "pass is worth 0 points"
   (score-turn ti1 passa)
   0)

  (test-false
   "sequence on first row of b2 is not a q"
   (q-sequence? (collect-sequence b2 (posn 0 0) horizontal-axis)))

  (test-true
   "sequence on first row of b2+ is a q"
   (q-sequence? (collect-sequence b2+ (posn 0 0) horizontal-axis)))

  (test-false
   "sequence on first row of b2 is a not q"
   (q-sequence? (collect-sequence b2 (posn 2 0) horizontal-axis)))

  (test-false
   "sequence on first row of b2+ is a not q"
   (q-sequence? (collect-sequence b2+ (posn 2 0) horizontal-axis)))

  (test-check
   "get sequences returns correct sequences for placements"
   set=?
   (get-sequences b2+ q-placements)
   (list (collect-sequence b2+ (posn 0 5) horizontal-axis)
         (collect-sequence b2+ (posn 0 5) vertical-axis)
         (collect-sequence b2+ (posn 1 5) horizontal-axis)
         (collect-sequence b2+ (posn 1 5) vertical-axis)
         (collect-sequence b2+ (posn 2 5) horizontal-axis)
         (collect-sequence b2+ (posn 2 5) vertical-axis)))

  (test-equal?
   "score qs for a placement with one q"
   (score/qs b2+ q-placements)
   6)

  (test-equal?
   "score qs for a placement with one q"
   (score/sequences b2+ q-placements)
   16)

  (test-equal?
   "score placement"
   (score/placement ti4+ q-placements)
   25)

  (test-equal?
   "score entire hand placement"
   (score/placement ti+ all-placement)
   20)

  (test-equal?
   "score turn action of q placement"
   (score-turn ti4+ (place q-placements))
   25)

  (test-equal?
   "score turn action of entire hand placement"
   (score-turn ti+ (place all-placement))
   20))
