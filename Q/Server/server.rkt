#lang racket

;; Data required:
;; List of current remote clients
;; TCP connection
;; Above 2 is probably one struct (server-info)

;; Waiting period (param)

;; Functionality breakdown:

#; {Natural -> Void}
;; Runs the server on the given port. The
;; Server listens on this TCP connection and constantly polls it for
;; new incoming requests to start new games.
(define (run port)
  ;; 1. initialize server-info
  ;; 2. call signup
  ;; 3. if signup returns false, try once more. else, return ref result
  ;; 4. if signup fails twice, return empty result
  (void))

#; {ServerInfo -> [Maybe [Pair [Listof String] [Listof String]]]}
;; Tries to gather enough players for a game. If enough players didn't
;; connect, returns false, otherwise returns result of the game.
;; TODO: bad name
(define (signup info)
  ;; some kind of timeout (20s)
  ;; if we get some connection, call deal-with-signup which handles the protocol for signing up *one* player.
  ;; If we get the max, immediately start
  ;; If we have min, keep waiting until 20s timeout then start
  ;; Either run or exit
  (void))


