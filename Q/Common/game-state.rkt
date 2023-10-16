#lang racket

(require (rename-in (only-in lazy define)
                    [define define/lazy]))
(require racket/set)
(require racket/generic)
(require math/base)

(require struct-plus-plus)
(require threading)
(require 2htdp/image)

(require Q/Common/map)
(require Q/Common/player)
(require Q/Common/config)
(require Q/Common/data/turn-action)
(require Q/Common/data/tile)
(require Q/Common/data/posn)
(require Q/Common/data/event)
(require Q/Common/util/list)
(require Q/Common/util/image)
(require Q/Common/util/misc)
(require Q/Common/interfaces/serializable)

(provide
 player-id?
 turn-action?
 (struct-out turn-info)
 (struct-out game-state)
 hash->turn-info++
 (contract-out
  [make-game-state (-> (listof tile?) (listof player-id?) game-state?)]
  [valid-placements?
   (-> game-state? (listof placement?) boolean?)]
  [valid-turn?
   (-> game-state? turn-action? boolean?)]
  [game-state->turn-info
   (->i ([gs game-state?])
        #:pre/name (gs)
        "game has no players left!"
        (not (null? (game-state-players gs)))
        [result turn-info?])]
  [take-turn
   (->i ([gs game-state?] [action turn-action?])
        #:pre/name (gs action)
        "invalid action for the current player"
        (valid-turn? gs action)
        [result game-state?])]
  [remove-player
   (->i ([gs game-state?] [player player-id?])
        #:pre/name (gs player)
        "player not in game!"
        (player-in-game? gs player)
        [result game-state?])]))


#; {type TurnInfo = (turn-info PlayerState
                               [Listof [Pairof PlayerId Natural]]
                               History
                               Board)}
;; A TurnInfo represents all the data necessary for a player to take their turn, including their
;; private knowledge, along with the public knowledge of turn order, scores, history, and the board.
;; INVARIANT: `scores` contains about every other player, and is ordered in the order of turns.
(struct++ turn-info
          ([state      player-state?]
           [scores     (listof (cons/c player-id? natural?))]
           [history    history?]
           [board      board?]
           [tiles-left natural?])
          #:transparent
          #:methods gen:serializable
          [(define/generic ->jsexpr* ->jsexpr)
           (define (->jsexpr ti)
             (match-define [turn-info state scores _history board tiles-left] ti)
             (hash 'map     (->jsexpr* board)
                   'tile*   tiles-left
                   'players (cons (->jsexpr* state)
                                  (rest scores))))])

#; {JPub -> TurnInfo}
(define (hash->turn-info++ jpub)
  (define jmap (hash-ref jpub 'map))
  (define tile* (hash-ref jpub 'tile*))
  (define players (hash-ref jpub 'players))
  (define jplayer (first players))
  (define scores* (rest players))
  
  (define state (hash->player-state++ jplayer))
  (define board (hash->board++ jmap))
  (define scores
    (for/list ([score scores*] [i (in-naturals)])
      (cons (string->symbol (number->string i)) score)))
  
  (turn-info state scores '() board tile*))


#; {type GameState = (game-state Board
                                 [Dequeof Tile]
                                 History
                                 [Dequeof PlayerState])}
;; A GameState represents the state of a game at any instant of time, containing the board's current
;; state at that instant, along with the list of remaining tiles, history of moves made in order of
;; recency, and the player states in order of turns to take.
(struct++ game-state
          ([board    board?]
           [tiles    (listof tile?)]
           [history  history?]
           [players  (listof player-state?)])
          #:transparent)


#; {[Listof Tile] [Listof PlayerId] -> GameState}
;; Creates a fresh game state with the given referee tile, set of tiles (shuffling them), and the
;; player IDs.
;; ASSUME: the players are sorted by age in non-increasing order.
;; ASSUME: the `start-tiles` comprise all the tiles in the game, and there are enough tiles to place
;;         on the board and hand out to players.
;; TODO: validate above assumption in contract
(define (make-game-state start-tiles player-ids)
  (match-define [cons start-tile rest-tiles] (shuffle start-tiles))
  (define handout-size           (* (length player-ids) (*hand-size*)))
  (define-values (handout tiles) (split-at rest-tiles handout-size))

  (define players
    (~>> handout
         (segment (*hand-size*))
         (map make-player-state player-ids)))

  (game-state++ #:board (make-board start-tile)
                #:tiles tiles
                #:history '()
                #:players players))


#; {GameState -> TurnInfo}
;; Produce the TurnInfo for the current player to make a decision from, using the given game state.
(define (game-state->turn-info gs)
  (match-define [game-state board tiles history [cons state others]] gs)
  (define player-alist (map player-state->pair others))
  (turn-info state player-alist history board (length tiles)))


#; {GameState PlayerId -> Boolean}
;; Is the given player in the current game state?
(define (player-in-game? gs player)
  (~>> gs
       game-state-players
       (map player-state-id)
       (member? player)))


#; {GameState PlayerId -> GameState}
;; Remove the player from the current game state.
(define (remove-player gs p-id)
  (match-define [game-state board tiles history players] gs)
  (define is-player?^ (curry is-player? p-id))
  (define-values (player players+)
    (find-remf is-player?^ players))
  (match-define [player-state _ score hand] player)

  (define tiles+   (append tiles hand))
  (define history+ (cons (banish p-id score) history))

  (game-state board tiles+ history+ players+))


#; {GameState TurnAction -> Boolean}
;; Is the given turn action a valid turn for the current player in the given game state?
(define (valid-turn? gs action)
  (match action
    [(place-tile placements) (valid-placements? gs placements)]
    [(exchange)              (valid-exchange? gs)]
    [(pass)                  #t]))


#; {GameState [Listof TilePlacement] -> Boolean}
;; Is the proposed placement for the current player valid in the rules of Q game?
(define (valid-placements? gs placements)
  (match-define [game-state board tiles history [cons state others]] gs)
  (define hand         (player-state-hand state))
  (define posns        (map placement-posn placements))
  (define placed-tiles (map placement-tile placements))

  (define/lazy all-valid-placements?
    (andmap (curry valid-placement? board) placed-tiles posns))
  (define/lazy all-tiles-in-hand?
    (contains-all? hand placed-tiles))

  (and (same-axis? posns)
       all-valid-placements?
       all-tiles-in-hand?))


#; {GameState -> Boolean}
;; Is an exchange action valid for the current player in the rules of Q?
(define (valid-exchange? gs)
  (define tiles (game-state-tiles gs))
  (define num-available (length tiles))
  (>= num-available (*hand-size*)))


#; {GameState TurnAction -> GameState}
;; Performs the given turn action for the current player in this game state, and updates the turn
;; queue.
;; ASSUME: the given game state has a non-zero amount of players.
(define (take-turn gs action)
  (define gs+
    (match action
      [(place-tile placements) (take-turn/placement gs placements)]
      [(exchange)           (take-turn/exchange gs)]
      [(pass)               gs]))
  (end-turn gs+ action))


#; {GameState [Listof TilePlacement] -> GameState}
;; Places the given list of placements, updating the board, player hand, and remaining tiles.
(define (take-turn/placement gs placements)
  (match-define [game-state board tiles history [cons state others]] gs)
  (define board+        (place-tiles board placements))
  (define placed-tiles  (map placement-tile placements))
  (define state+        (remove-from-hand state placed-tiles))
  (define-values (state++ tiles+)
    (refill-hand state+ tiles))

  (define state+++ (score-turn state++ board+ placements))

  (game-state board+ tiles+ history (cons state+++ others)))


#; {GameState -> GameState}
;; Exchange the current player's tiles for new ones from the game state's tiles.
(define (take-turn/exchange gs)
  (match-define [game-state board tiles history [cons state others]] gs)
  (define-values (state+ hand) (clear-hand state))

  (define-values (state++ tiles+) (refill-hand state+ tiles))
  (define tiles++ (append tiles+ hand))

  (game-state board tiles++ history (cons state++ others)))


#; {GameState TurnAction -> GameState}
;; Ends the current turn, moving the current player to the end and adding the given turn action to
;; the history.
(define (end-turn gs action)
  (match-define [game-state board tiles history players] gs)
  (define id (player-state-id (first players)))
  (define history+ (cons (turn id action) history))

  (game-state board tiles history+ (rotate-left-1 players)))

#; {Board [Listof TilePlacement] -> [Setof [Listof TilePlacement]]}
;; Produce a set of sequences for each posn along every axis.
(define (score/sequences b placements)
  (define posns (map placement-posn placements))
  (for*/set ([p posns] [axis axes])
    (collect-sequence b p axis)))


#; {Board [Listof TilePlacement] -> Natural}
;; ASSUME the placements are valid.
(define (score/placement b placements)
  ;; A player receives one point per tile placed.
  (define acc-points (length placements))

  (define seqs
    (~>> (score/sequences b placements)
         set->list
         (filter-not (compose one? length))))

  ;; A player receives one point per tile in a contiguous sequence of tiles (in a row or column)
  ;; that contains at least one of its newly placed tiles.
  (set! acc-points (+ acc-points (sum (map length seqs))))

  (define q-seqs
    (filter (lambda (seq)
              (define tiles  (map placement-tile seq))
              (define colors (map tile-color tiles))
              (define shapes (map tile-shape tiles))
              (or (same-elements? colors tile-colors)
                  (same-elements? shapes tile-shapes)))
            seqs))

  ;; A player receives 6 bonus points for completing a Q, which is a contiguous sequence of tiles
  ;; that contains all shapes or all colors and nothing else.
  (define points-per-Q 6)
  (set! acc-points (+ acc-points (* points-per-Q (length q-seqs))))

  (define all-placed-points 6)
  ;; A player also receives 6 bonus points for placing all tiles in its possession.
  (when (= (length placements) (*hand-size*))
    (set! acc-points (+ acc-points all-placed-points)))

  acc-points)


#; {PlayerState Board [Listof TilePlacement] -> PlayerState}
;; Score the turn of a player for the given placement of tiles.
(define (score-turn ps b placements)
  (define score  (player-state-score ps))
  (define score+ (+ score (score/placement b placements)))
  (set-player-state-score ps score+))


#; {PlayerState -> Image}
(define (render-player-state ps)
  (match-define [player-state id score hand] ps)
  (define size (/ (*game-size*) 2))
  (define tiles-size (* (*game-size*) 2/3))
  (define text-image (text (~a id ": " score) size 'black))
  (define tiles-image
    (parameterize ([*game-size* tiles-size])
      (for/fold ([img empty-image])
                ([t (map render-tile hand)])
        (beside img t))))

  (above/align 'left text-image tiles-image))

#; {GameState -> Image}
(define (render-game-state gs)
  (match-define [game-state board tiles history players] gs)
  (define states-image
    (for/fold ([img empty-image])
              ([ps players])
      (beside img
              (render-player-state ps)
              (empty-space 20 20))))
  (define board-image (render-board board))
  (above board-image (empty-space 20 20) states-image))


(module+ test
  (require rackunit)
  (require Q/Common/util/test)
  (define tile-product (cartesian-product tile-colors tile-shapes))
  (define tile-set (map (curry apply tile) tile-product))
  (define players1 '(lucas andrey luke))
  (define players2 '(lucas andrey luke john))
  (define gs1 (apply/seed 0 make-game-state tile-set players1))
  (define gs2 (apply/seed 0 make-game-state tile-set players2))
  (define tile-set-seeded (apply/seed 0 shuffle tile-set))

  (define gs1+ (take-turn gs1 (place-tile (list (placement (posn 1 0) (tile 'green 'clover))
                                              (placement (posn 1 1) (tile 'blue 'clover))))))
  (define gs1++ (take-turn gs1+ (place-tile (list (placement (posn 1 2) (tile 'red 'clover))
                                                (placement (posn 1 -1) (tile 'green 'star))
                                                (placement (posn 1 -2) (tile 'yellow 'star))
                                                (placement (posn 1 -3) (tile 'blue 'star))
                                                (placement (posn 1 -4) (tile 'blue 'circle)))))))

(module+ test
  (test-case
   "make-game-state with 3 players"
   (match-define [game-state board tiles history players] gs1)
   (check-equal? board
                 (make-board (first tile-set-seeded))
                 "board is created with one tile")

   (check-equal? history
                 '()
                 "history starts empty")

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
   (match-define [game-state board tiles history players] gs2)
   (check-equal? board
                 (make-board (first tile-set-seeded))
                 "board is created with one tile")

   (check-equal? history
                 '()
                 "history starts empty")

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
   "game state -> turn info"
   (match-define [game-state board tiles history [cons curr-player others]] gs1)
   (check-equal? (game-state->turn-info gs1)
                 (turn-info++ #:state curr-player
                              #:scores (map player-state->pair others)
                              #:history history
                              #:board board
                              #:tiles-left (length tiles))))

  (test-case
   "player in game?"
   (check-true (player-in-game? gs1 'lucas) "lucas in game")
   (check-false (player-in-game? gs2 'nishil) "nishil not in game"))

  (test-case
   "remove player"
   (match-define [game-state board tiles history players] gs1)
   (define new-history (list (banish 'lucas 0)))
   (define new-tiles (append tiles (player-state-hand (findf (curry is-player? 'lucas) players))))
   (check-equal? (remove-player gs1 'lucas)
                 (game-state board new-tiles new-history (remf (curry is-player? 'lucas) players))
                 "removing lucas returns his tiles and removes him from the queue"))


  (test-case
   "valid exchange?"
   (check-true (valid-exchange? gs1)
              "game state has enough tiles for exchange")
   (check-false (valid-exchange? (make-game-state (drop tile-set-seeded 19) players1))
               "game state doesnt have enough tiles"))

  )
