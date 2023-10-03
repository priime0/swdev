#lang racket

(require struct-plus-plus)
(require threading)

(require Q/Common/map)
(require Q/Common/config)
(require Q/Common/data/tile)
(require Q/Common/util/list)

#; {type PlayerId = String}
;; A PlayerId represents a _unique_ identifier for a player during a game.

#; {type TurnInfo = (turn-info PlayerId
                               [Listof Tile]
                               [HashTable PlayerId Natural]
                               Board)};
;; A TurnInfo represents all the data necessary for a player to take their turn, containing their
;; player id, their tiles, the points of every player, and the current board state.
(struct++ turn-info
          ([player-id    string?]
           [player-tiles (listof tile?)]
           [scores       (hash/c string? natural?)]
           [board        board?])
          #:transparent)

#; {type TilePlacement = [Pair Posn Tile]}
;; A TilePlacement represents a request from the player to place the given tile at the given
;; position.

#; {type TurnAction = (U [List 'place-tile [Listof TilePlacement]]
                         [List 'exchange]
                         [List 'pass])}
;; A TurnAction represents a possible action during a player turn, and is one of:
;; - A placement of tiles onto the board at the corresponding locations in the given order
;; - An exchange of all tiles in a player's hand
;; - Skipping a player's turn (withdrawing from performing any actions)


#; {type PlayerState = (player-state Natural [Listof Tile])}
;; A PlayerState represents a participating player's state during any instant of time, containing
;; the points the player accrued during the game, along with the tiles in their hand during a move.
;; INVARIANT: The hand of a player state has a length L such that 0 ≤ L ≤ 6.
(struct++ player-state ([score  natural?]
                        [hand   (listof tile?)])
          #:transparent)


#; {type GameState = (game-state Board
                                 [Dequeof Tile]
                                 [HashTable PlayerId PlayerState]
                                 [Dequeof PlayerId])}
;; A GameState represents the state of a game at any instant of time, containing the board's current
;; state at that instant, along with the list of remaining tiles, state of participating players and
;; their turn orders.
;; INVARIANT: A player β is in the `players` hashtable iff β is in the `turn-queue` deque.
(struct++ game-state
          ([board board?]
           [tiles (listof tile?)]
           [player-states (hash/c string? player-state?)]
           turn-queue)
          #:transparent)

#; {Tile [Listof PlayerId] -> GameState}
;; Creates a fresh game state with the given referee tile, set of tiles (shuffling them), and the
;; player IDs.
;; ASSUME: the players are sorted by age in non-increasing order.
;; ASSUME: the `start-tile` and `tiles` comprise all the tiles in the game.
(define (make-game-state start-tile tiles player-ids)
  (define-values (states^^ tiles^^)
    (for/fold ([states^ (hash)]
               [tiles^ (shuffle tiles)])
              ([player-id player-ids])
      (define default-score 0)
      (define-values (hand tiles+) (split-at tiles^ (*hand-size*)))
      (define state (player-state default-score hand))
      (define states+ (hash-set states^ player-id state))
      (values states+ tiles+)))
  (game-state++ #:board (make-board start-tile)
                #:tiles tiles^^
                #:player-states states^^
                #:turn-queue player-ids))


#; {GameState PlayerId [Listof TilePlacement] -> GameState}
;; ASSUME: `placements` contains all valid placements of tiles.
(define (remove-from-hand gs p-id placements)
  (define states       (game-state-player-states gs))
  (define state        (hash-ref states p-id))
  (define hand         (player-state-hand state))
  (define placed-tiles (map cdr placements))
  (define hand+
    (for/fold ([hand^ hand])
              ([tile placed-tiles])
      (remove tile hand^)))
  (define state+ (set-player-state-hand state hand+))
  (define states+ (hash-set states p-id state+))

  (set-game-state-player-states gs states+))


#; {[HashTable PlayerId PlayerState] -> [HashTable PlayerId Natural]}
(define (player-states->player-scores states)
  (define (extract-score name state)
    (values name (player-state-score state)))
  (hash-map extract-score states))


#; {GameState -> TurnInfo}
;; Produce the TurnInfo for the current player to make a decision from, using the given game state.
(define (game-state->turn-info gs)
  (match-define [game-state board _ states turn-queue] gs)

  (define player-id (first turn-queue))
  (define state (hash-ref states player-id))
  (define player-tiles (player-state-hand state))

  (define scores (player-states->player-scores states))

  (turn-info player-id player-tiles scores board))

#; {GameState PlayerId -> GameState}
;; Kick the player from the current game state.
;; EXCEPT: When the given player id is not in the game.
(define (kick-player gs p-id)
  (match-define [game-state board tiles states turn-queue] gs)

  (unless (member? p-id turn-queue)
    (error 'kick-player
           "player ~a does not exist!"
           p-id))

  (define hand (player-state-hand (hash-ref states p-id)))

  (define tiles+      (append tiles hand))
  (define states+     (hash-remove states p-id))
  (define turn-queue+ (remf (curry string=? p-id) turn-queue))

  (game-state board tiles+ states+ turn-queue+))

#; {GameState TurnAction -> GameState}
;; ASSUME: the given game state has a non-zero amount of players.
(define (take-turn gs action)
  (define gs+
    (match action
      [(list 'place-tile placements) (take-turn/placement gs placements)]
      [(list 'exchange)              (take-turn/exchange gs)]
      [(list 'pass)                  (identity gs)]))
  (end-turn gs+))

#; {GameState [Listof TilePlacement] -> GameState}
(define (take-turn/placement gs placements)
  (match-define [game-state board tiles states turn-queue] gs)

  ;; TODO: Add scoring
  (define board+
    (for/fold ([board^ board])
              ([placement placements])
      (match-define [cons posn tile] placement)
      (board-add-tile board^ posn tile)))
  (define tiles+ (take (length placements) tiles))
  ;; TODO: Remove tiles from player's hand

  (game-state board+ tiles states turn-queue))

#; {GameState -> GameState}
;; Exchange the current player's tiles for new ones from the game state's tiles.
(define (take-turn/exchange gs)
  (match-define [game-state board tiles states turn-queue] gs)
  (define player-id (first turn-queue))
  (define player-state (hash-ref states player-id))
  (define hand (player-state-hand player-state))

  (define-values (hand+ tiles+) (split-at tiles (*hand-size*)))
  (define tiles++ (append tiles+ hand))
  (define player-state+ (set-player-state-hand hand+))
  (define states+ (hash-set states player-id player-state+))

  (game-state board tiles++ states+ turn-queue))

#; {GameState -> GameState}
;; Updates the turn queue, moving the current player to the end.
(define (end-turn gs)
  (define turn-queue (game-state-turn-queue gs))
  (set-game-state-turn-queue gs (rotate-left-1 turn-queue)))
