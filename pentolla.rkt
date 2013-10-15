#lang racket
;;;; Daniel Garcia
;;;; Jason Tupper

;;;; pentolla_soln.rkt
;;;; CPSC 481 - Fall 2013
;;;; Project 1 - pentolla state space
;;;;
;;;; In case it matters, this file is licensed according to the BSD
;;;; 2-clause license:
;;;;
;;;; Copyright (c) 2013, Kevin Wortman
;;;; All rights reserved.
;;;; 
;;;; Redistribution and use in source and binary forms, with or
;;;; without modification, are permitted provided that the following
;;;; conditions are met:
;;;; 
;;;;     Redistributions of source code must retain the above
;;;;     copyright notice, this list of conditions and the following
;;;;     disclaimer.
;;;; 
;;;;     Redistributions in binary form must reproduce the above
;;;;     copyright notice, this list of conditions and the following
;;;;     disclaimer in the documentation and/or other materials
;;;;     provided with the distribution.
;;;; 
;;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
;;;; CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
;;;; INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
;;;; CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
;;;; USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
;;;; AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
;;;; ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;;; POSSIBILITY OF SUCH DAMAGE.

(require rackunit srfi/1 srfi/26)

;;;; Contracts for all of this module's exports.
(provide
 (contract-out

  ;; (coord? v) returns true iff v is an exact integer.
  [coord? (-> any/c boolean?)]
  
  ;; A posn struct represents an (x, y) coordinate.
  [struct posn ((x coord?) (y coord?))]
  
  ;; The size of the board; should be 14.
  [board-dim exact-positive-integer?]

  ;; (coord-on-board? v) returns true iff coord v is on the board, i.e.
  ;; between 1 and board-dim, inclusive.
  [coord-on-board? (-> coord? boolean?)]
  
  ;; (posn-on-board? v) returns true iff both of v's coordinates are on
  ;; the board.
  [posn-on-board? (-> posn? boolean?)]

  ;; (blue? v) returns true iff v is the symbol 'blue.
  [blue? (-> any/c boolean?)]
  
  ;; (orange? v) returns true iff v is the symbol 'orange.
  [orange? (-> any/c boolean?)]
  
  ;; (player-color? v) returns true iff v is one of the symbols 'blue
  ;; or 'orange .
  [player-color? (-> any/c boolean?)]
  
  ;; A tile is a list of 1-5 distinct, contiguous posns forming a
  ;; polyomino, all of the same color.
  [struct tile ((color player-color?)
                (posns (non-empty-listof posn?)))]

  ;; Convenience procedures to test the color of a tile.
  [tile-blue? (-> tile? boolean?)]
  [tile-orange? (-> tile? boolean?)]
  
  ;; Returns the extreme (minimum or maximum) coordinates of any posn
  ;; in a tile.
  [tile-min-x (-> tile? coord?)]
  [tile-min-y (-> tile? coord?)]
  [tile-max-x (-> tile? coord?)]
  [tile-max-y (-> tile? coord?)]
  
  ;; (tile-at-origin? t) returns true iff t is positioned at the
  ;; origin, i.e. if its minimum x and y coordinates are both zero. We
  ;; use the convention that unplayed tiles are at the origin, and
  ;; played tiles are not.
  [tile-at-origin? (-> tile? boolean?)]
  
  ;; (tile-on-board? t) returns true iff t is entirely on the board,
  ;; i.e. all its posns satisfy posn-on-board? .
  [tile-on-board? (-> tile? boolean?)]
  
  ;; The list of the 42 unplayed tiles in the game.
  [start-tiles (listof (and/c tile? tile-at-origin?))]
  
  ;; (tile-length t) returns the number of posns in the tile.
  [tile-length (-> tile? exact-positive-integer?)]

  ;; (tile-translate t dx dy) returns a tile formed by translating t
  ;; by dx units on the x-axis and dy units on the y-axis. I.e. add dx
  ;; to every x coordinate and dy to every y coordinate.
  [tile-translate (-> tile? coord? coord? tile?)]

  ;; A rotation count is an exact integer between 0 and 3 inclusive.
  [rotation-count? (-> any/c boolean?)]

  ;; (tile-rotate t k) returns the tile that results from rotating
  ;; tile t 90 degrees clockwise k times. t must be at the origin, and
  ;; the resulting tile is at the origin.
  [tile-rotate (-> (and/c tile? tile-at-origin?)
                   rotation-count? 
                   (and/c tile? tile-at-origin?))]
  
  ;; (tile-overlaps/posn? t p) returns true iff tile t overlaps with
  ;; posn p, i.e. (tile-posns t) contains p.
  [tile-overlaps/posn? (-> tile? posn? boolean?)]
  
  ;; (tile-overlaps/tile? t1 t2) returns true iff tiles t1 and t2
  ;; overlap, i.e.  they have at least one posn in common.
  [tile-overlaps/tile? (-> tile? tile? boolean?)]
  
  ;; (tile-touches? t1 t2) returns true if tiles t1 and t2 touch along
  ;; sides. I.e. there exists a posn in t1 that is directly above,
  ;; below, right, or left of some posn in t2. You may not place a
  ;; tile such that it touches one of your own tiles.
  [tile-touches? (-> tile? tile? boolean?)]
  
  ;; (tile-on-corner? t1 t2) returns true if tiles t1 and t2 touch
  ;; along corners. I.e. there exists a posn in t1 that is diagonal to
  ;; some posn in t2. You must place each tile such that it is on the
  ;; corner of one of your own tiles.
  [tile-on-corner? (-> tile? tile? boolean?)]
  
  ;; A pass count is the number of times players have passed in a row,
  ;; which must be an exact integer that is 0, 1, or 2.
  [pass-count? (-> any/c boolean?)]
  
  ;; A state is a list of played tiles; a list of unplayed tiles; and
  ;; a pass count. The played tiles are in reverse chronological
  ;; order, so that the most recently played tile is at the front of
  ;; the list.
  [struct state ((played (listof (and/c tile? tile-at-origin?)))
                 (unplayed (listof (and/c tile? tile-on-board?)))
                 (passes pass-count?))]
  
  ;; (state-game-over s) returns true iff s is a finished game, i.e.
  ;; (state-passes s) is 2.
  [state-game-over? (-> state? boolean?)]
  
  ;; start-state is the starting state struct, i.e. the played list is
  ;; empty, unplayed list is start-tiles, and passes is zero.
  [start-state (and/c state? (not/c state-game-over?))]
  
  ;; Returns the color of the next player's turn.
  [state-whose-turn (-> state? player-color?)]
  
  ;; (pass? v) returns true iff v is the symbol 'pass .
  [pass? (-> any/c boolean?)]

  ;; A move represents the act of moving a tile from the unplayed list
  ;; to a specific position on the board, then rotating it a specific
  ;; number of times. The move-tile field is an _unplayed_ tile,
  ;; i.e. it is at the origin and the player intends to translate it
  ;; to the given x and y coordinates.
  [struct move ((tile (and/c tile? tile-at-origin?))
                (x coord-on-board?)
                (y coord-on-board?)
                (rotations rotation-count?))]
  
  ;; Returns the tile corresponding to the result of a move; i.e. the
  ;; unplayed move-tile subjected to the translation and rotation
  ;; indicated by the move.
  [move->tile-on-board (-> move? (and/c tile? tile-on-board?))]
  
  ;; (action? v) returns true iff v is a legal player action. On a
  ;; player's turn, they may either move a tile or pass. So this
  ;; procedure returns true iff v satisfies pass? or move?.
  [action? (-> any/c boolean?)]
  
  ;; (state-action-violation s a) analyzes the given state s and
  ;; action a. When the action violates one of the game rules, it
  ;; returns a descriptive error message in a string; this string
  ;; should be suitable for displaying to an end user, something like
  ;; "you cannot play tiles off the board". If the action is legal
  ;; according to all rules, the procedure returns #f.
  [state-action-violation (-> state? action? (or/c string? false?))]
  
  ;; (state-play-legal? s p) returns true iff p is a legal play on
  ;; state s.
  [state-action-legal? (-> state? action? boolean?)]
  
  ;; (state-transition s a) returns the state that results from making
  ;; a legal action a starting from state s.
  ;;
  ;; If a is pass? then the resulting state should be the same except
  ;; the pass count should be incremented.
  ;;
  ;; Otherwise a is a move. The played tile should be removed from the
  ;; unplayed list, translated then rotated, and inserted at the front
  ;; of the played list. The pass count should be reset to zero.
  [state-transition (->i ([s state?]
			  [a (s) (and/c action?
					(cut state-action-legal? <> s))])
			 [result state?])]

  ;; (state-legal-plays s) returns a list of all legal actions for
  ;; state s.
  [state-legal-actions (-> state? (listof action?))]
  
  ;; (state-children s) returns a list of all states that s can
  ;; legally transition to. Note that the list should be empty when
  ;; the game is over.
  [state-children (-> state? (listof state?))]
  
  ;; (state-score s c) returns the numeric score of player c at state
  ;; s, if the game were to end at that moment.
  [state-score (-> state? player-color? exact-integer?)]))

;;;; PROVIDED DEFINITIONS
  
(define coord? exact-integer?)

(struct posn (x y) #:prefab)

(define board-dim 14)

(define (coord-on-board? v)
  (and (coord? v)
       (<= 1 v board-dim)))

(struct tile (color posns) #:prefab)

(define start-tiles
  [for*/list ([color '(blue orange)]
              [coord-lists '(((0 0) (1 0) (2 0) (3 0) (3 1)) ; L5
                             ((0 0) (1 0) (2 0) (3 0) (2 1)) ; Y
                             ((0 0) (1 0) (2 0) (2 1) (3 1)) ; N
                             ((0 0) (1 0) (2 0) (3 0)) ; I4
                             
                             ((0 0) (0 1) (0 2) (1 2) (2 2)) ; V5
                             ((0 0) (1 0) (1 1) (1 2) (2 2)) ; Z5
                             ((1 0) (0 1) (1 1) (2 1) (1 2)) ; X
                             ((0 0) (1 0) (0 1) (1 1) (0 2)) ; P
                             ((0 0) (0 1) (1 1) (1 2)) ; Z4
                             ((0 0) (0 1) (0 2) (1 2)) ; L4
                             
                             ((0 0) (0 1) (1 1) (1 2) (2 2)) ; W
                             ((0 0) (1 0) (2 0) (1 1) (1 2)) ; T
                             ((1 0) (1 1) (2 1) (0 2) (1 2)) ; F
                             ((0 0) (2 0) (0 1) (1 1) (2 1)) ; U
                             ((0 0) (1 0) (0 1) (1 1)) ; O4
                             ((0 0) (0 1) (1 1) (0 2)) ; T4
                             
                             ((0 0) (1 0) (2 0) (3 0) (4 0)) ; I5
                             ((0 0) (1 0) (2 0)) ; I3
                             ((0 0) (0 1) (1 1)) ; V3
                             ((0 0) (1 0)) ; I2
                             ((0 0)) ; I1
                             )])
    (tile color (map (lambda (coord-list)
                       (apply posn coord-list))
                     coord-lists))])

(define (tile-at-origin? t)
  (and (zero? (tile-min-x t))
       (zero? (tile-min-y t))))

(struct state (played unplayed passes) #:prefab)

(struct move (tile x y rotations) #:prefab)

;;;; END OF PROVIDED DEFINITIONS

;;;; START OF STUDENT-AUTHORED DEFINITIONS

(define legal-rotations (list 0 1 2 3))
(define start-state (state '() start-tiles 0))
(define legal-coords (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14))

(define (tile-min-x t)
  (define posns (tile-posns t))
  (define x (map (lambda (pos)
                   (posn-x pos)) posns) )
  (first (sort x <))
)

(define (tile-min-y t)
  (define posns (tile-posns t))
  (define y (map (lambda (pos)
                   (posn-y pos)) posns) )
  (first (sort y <))
)

(define (tile-max-x t)
  (define posns (tile-posns t))
  (define x (map (lambda (pos)
                   (posn-x pos)) posns) )
  (last (sort x <))
)

(define (tile-max-y t)
  (define posns (tile-posns t))
  (define y (map (lambda (pos)
                   (posn-y pos)) posns) )
  (last (sort y <))
)

(define (tile-length t)
  (length (tile-posns t))
)

(define (player-color? color)
  (or (equal? color 'blue)
      (equal? color 'orange)
   )
)

(define (rotation-count? v)
  (if (member v legal-rotations)
      ;; then
      #t
      ;; else
      #f
   )
)

(define (tile-on-board? t)
  (andmap posn-on-board? (tile-posns t))
)

(define (pass-count? n) 
  (if(member n (list 0 1 2))
     ;; then
     #t
     ;; else
     #f
   )
)

(define (state-game-over? s ) ;;state
  (>= (state-passes s) 2)
)

(define (action? v)
  (if (or (pass? v)
          (move? v))
      #t
      ;; else
      #f
  )
)

(define (posn-on-board? pos) 
  (if
    (and (member (posn-x pos) legal-coords)
         (member (posn-y pos) legal-coords) )
    ;; then
    #t
    ;; else
    #f
  )
)

(define (blue? v)
  (eq? v 'blue)
)

(define (orange? v)
  (eq? v 'orange)
)

(define (tile-blue? tile)
  (blue? (tile-color tile))
)

(define (tile-orange? tile)
  (orange? (tile-color tile))
)

(define (tile-translate t dx dy)
  (define posns (tile-posns t))
  (define newPosns (map (lambda (pos)
                        (posn (+ (posn-x pos) dx) (+ (posn-y pos) dy)))
                        posns))
  (tile (tile-color t) newPosns)
)

(define (tile-rotate tile rotationCount)
  (cond 
    [(eq? 0 rotationCount) tile]
    [(eq? 1 rotationCount) (mirror-y (transpose tile))]
    [(eq? 2 rotationCount) (mirror-y (mirror-x tile))]
    [else (mirror-x (transpose tile))]; 3
    )
)

(define (transpose t)
  (define posns (tile-posns t))
  (define newP (map (lambda (pos)
         (posn (posn-y pos) (posn-x pos))) posns))
  (tile (tile-color t) newP)
)

(define (mirror-x t)
  (define maxOfx (tile-max-x t))
  (define newP (map (lambda (pos)
                    (posn (- maxOfx (posn-x pos)) (posn-y pos)))
                    (tile-posns t)))
  (tile (tile-color t) newP)
)

(define (mirror-y t)
  (define maxOfy (tile-max-y t))
  (define newP (map (lambda(pos)
                      (posn (posn-x pos) (- maxOfy (posn-y pos))))
                      (tile-posns t)))
  (tile (tile-color t) newP)
)

(define (tile-overlaps/posn? tile position)
  (ormap (lambda (pos)
           (and (eq? (posn-x pos) (posn-x position))
                (eq? (posn-y pos) (posn-y position))))
           (tile-posns tile))
)

(define (tile-overlaps/tile? tile1 tile2)
  (ormap (lambda (tile2pos)
           (tile-overlaps/posn? tile1 tile2pos))
         (tile-posns tile2))
)

(define (tile-touches? tile1 tile2)
  (define allPos (append* (map (lambda (pos)
          (list (posn (+ 1 (posn-x pos)) (posn-y pos));right
               (posn (- (posn-x pos) 1) (posn-y pos));left
               (posn (posn-x pos) (+ 1 (posn-y pos)));down
               (posn (posn-x pos) (- (posn-y pos) 1));up
               ))
       (tile-posns tile1))))
  (ormap (lambda (pos)
           (tile-overlaps/posn? tile2 pos))
          allPos)
)

(define (tile-on-corner? tile1 tile2)
  (define cornerPosns (append* (map (lambda (pos)
          (list (posn (+ (posn-x pos) 1) (+ (posn-y pos) 1));btm-right
                (posn (+ (posn-x pos) 1) (- (posn-y pos) 1));btm-left
                (posn (- (posn-x pos) 1) (+ (posn-y pos) 1));top-right
                (posn (- (posn-x pos) 1) (- (posn-y pos) 1));top-left
               ))
       (tile-posns tile1))))
  (ormap (lambda (pos)
           (tile-overlaps/posn? tile2 pos))
         cornerPosns)
)

(define (state-whose-turn state)
  (if (eq? (length (state-played state)) 0)
      'blue
      ;; else
      (if (blue? (tile-color (first (state-played state))))		
          (if (eq? (state-passes state) 0)		
              'orange	;;if the last played tile color is blue and we have 0 passes
              'blue	;;if the last played tile color is blue and we have 1 passes
              )
          (if (eq? (state-passes state) 0)
              'blue	;;if the last played tile color is orange and we have 0 passes
              'orange	;;if the last played tile color is orange and we have 1 passes
              )
      )
  )
)

(define (pass? v)
  (eq? v 'pass)
)

(define (move->tile-on-board m);move
  (tile-translate (tile-rotate (move-tile m) (move-rotations m)) 
                  (move-x m)
                  (move-y m))
)

(define (state-action-violation s action)
  (cond 
    [(not (action? action))
      "This is not an action."]
    [(and (< (length (state-played s)) 2)
          (pass? action))
     "You can't pass on your first turn."]
    [(pass? action) #f]
    ;;After this we can assume the action is a move
    ;; Correct color
    [(not (eq? (state-whose-turn s) (tile-color (move-tile action))))
     "You must choose a tile of your own color."]
    ;; Tile out of bounds
    [(not (tile-on-board? (tile-translate (tile-rotate 
                  (move-tile action) (move-rotations action)) 
                  (move-x action) (move-y action))))
     "Tile must be placed on board."]
    ;; Blue's first turn: must cover (5,5)
    [(eq? s start-state)
     (if (member (posn 5 5) (tile-posns (move->tile-on-board action)))
         #f
         "You must place your tile on (5, 5)"
     )]
    ;; Orange's first turn: must cover (10,10)
    [(eq? (length (state-played s)) 1)
     (if (member (posn 10 10) (tile-posns (move->tile-on-board action)))
         #f
         "You must place your tile on (10, 10)")]
    ;; Tile not previously played
    [(not (member (move-tile action) (state-unplayed s)))
     "You already played that tile"]
    ;; Tile not overlapping other tiles
    [(ormap (lambda (tile)
               (tile-overlaps/tile? (move->tile-on-board action) tile))
             (state-played s))
     "Your tile cannot be on top of other tiles."]
    ;; Tile not touching other tiles of the same color
    [(ormap (lambda (tile)
               (tile-touches? (move->tile-on-board action) tile))
             (if (blue? (state-whose-turn s))
                 (filter tile-blue? (state-played s))
                 (filter tile-orange? (state-played s))))
     "Your tile cannot touch other tiles of the same color."]
    ;; Tile must be in the corner of other tile with the same color
    [(not (ormap (lambda (tile)
               (tile-on-corner? (move->tile-on-board action) tile))
             (if (blue? (state-whose-turn s))
                 (filter tile-blue? (state-played s))
                 (filter tile-orange? (state-played s)))))
     "Your tile must be placed at the corner of other tiles w/the same color."]
    [else #f]
  )
)

(define (state-action-legal? state action)
  (not (state-action-violation state action))
)

(define (state-transition s action)
  (if (pass? action)
      (state (state-played s) 
             (state-unplayed s) 
             (+ 1 (state-passes s)))
      ;; else
      (state (list* (move->tile-on-board action) (state-played s))
             (delete (move-tile action) (state-unplayed s))
             0
      )
   )
)

(define (which-color? c)
  (if (blue? c)
      tile-blue?
      tile-orange?
  )
)

(define (state-legal-actions s)
  ;;get color of the next tile to be placed
  (define color (state-whose-turn s))
  
  ;   for each tile (of player's color)
  (define possibleTiles (filter (which-color? color) (state-unplayed s)))
       
  (filter (lambda(x) (state-action-legal? s x))
          (append (list 'pass)
                  (for*/list ([t possibleTiles]
                              [x legal-coords]
                              [y legal-coords]
                              [rot legal-rotations])
                    (move t x y rot))))
)

(define (state-children s)
  (for/list ([action (state-legal-actions s)])
    (state-transition s action))
)

(define (state-score state color)
  (define (calc-score played unplayed)
    (if (eq? (length unplayed) 0)
      (if (eq? (length (tile-posns (first played))) 1)
          20
          15
          )
      (* -1 (apply + (map (lambda (x) (length (tile-posns x)))
                    unplayed))
      ))
   )
  (cond
    [(blue? color) 
     (define unplayed (filter tile-blue? (state-unplayed state)))
     (define played (filter tile-blue? (state-played state)))
     (calc-score played unplayed)
     ]
    [(orange? color)
     (define unplayed (filter tile-orange? (state-unplayed state)))
     (define played (filter tile-orange? (state-played state)))
     (calc-score played unplayed)
     ]
    )
)

;;;; END OF STUDENT-AUTHORED DEFINITIONS

(module+ test
  ;;;; PROVIDED UNIT TESTS
  (check-true (coord? -1))
  (check-true (coord? 0))
  (check-true (coord? 1))
  (check-true (coord? 14))
  (check-false (coord? 'blue))
  (check-false (coord? 1/2))
  
  (check-equal? board-dim 14)
  
  (check-false (coord-on-board? 0))
  (check-true (coord-on-board? 1))
  (check-true (coord-on-board? 13))
  (check-true (coord-on-board? 14))
  (check-false (coord-on-board? 15))
  
  (check-false (posn-on-board? (posn 0 0)))
  (check-false (posn-on-board? (posn 0 1)))
  (check-false (posn-on-board? (posn 1 0)))
  (check-true (posn-on-board? (posn 1 1)))
  (check-true (posn-on-board? (posn 14 14)))
  (check-false (posn-on-board? (posn 15 15)))
  
  (check-true (player-color? 'blue))
  (check-true (player-color? 'orange))
  (check-false (player-color? 0))
  
  (check-true (blue? 'blue))
  (check-false (blue? 'orange))
  (check-true (orange? 'orange))
  (check-false (orange? 'blue))
   
  (define blue-I1 (tile 'blue (list (posn 0 0))))
  (define orange-I1 (struct-copy tile blue-I1 [color 'orange]))
  
  (check-true (tile? blue-I1))
  (check-true (tile? orange-I1))
  (check-false (tile? 'blue))
  
  (check-true (tile-blue? blue-I1))
  (check-false (tile-orange? blue-I1))
  (check-false (tile-blue? orange-I1))
  (check-true (tile-orange? orange-I1))
  
  (define blue-I2 (tile 'blue (list (posn 0 0) (posn 1 0))))
  
  (check-equal? (tile-min-x blue-I2) 0)
  (check-equal? (tile-min-y blue-I2) 0)
  (check-equal? (tile-max-x blue-I2) 1)
  (check-equal? (tile-max-y blue-I2) 0)

  (check-true (tile-at-origin? blue-I1))
  (check-true (tile-at-origin? orange-I1))
  (check-true (tile-at-origin? blue-I2))
  (check-false (tile-at-origin? (tile-translate blue-I2 3 4)))
  
  (check-false (tile-on-board? blue-I1))
  (check-false (tile-on-board? orange-I1))
  (check-false (tile-on-board? blue-I2))
  (check-true (tile-on-board? (tile-translate blue-I2 1 1)))
  
  (define (tile-count color length)
    (count (lambda (tile)
             (and (symbol=? color (tile-color tile))
                  (= length (tile-length tile))))
           start-tiles))
  
  (check-true (andmap tile? start-tiles))
  (check-equal? (length start-tiles) 42)

  (check-equal? (tile-count 'blue 1) 1)
  (check-equal? (tile-count 'orange 1) 1)
  
  (check-equal? (tile-count 'blue 2) 1)
  (check-equal? (tile-count 'orange 2) 1)
  
  (check-equal? (tile-count 'blue 3) 2)
  (check-equal? (tile-count 'orange 3) 2)
  
  (check-equal? (tile-count 'blue 4) 5)
  (check-equal? (tile-count 'orange 4) 5)
  
  (check-equal? (tile-count 'blue 5) 12)
  (check-equal? (tile-count 'orange 5) 12)
  
  (check-equal? (tile-length orange-I1) 1)
  (check-equal? (tile-length blue-I2) 2)
  
  (check-equal? (tile-posns (tile-translate blue-I2 3 7))
                (list (posn 3 7) (posn 4 7)))
  
  (check-false (rotation-count? -1))
  (check-true (rotation-count? 0))
  (check-true (rotation-count? 1))
  (check-true (rotation-count? 2))
  (check-true (rotation-count? 3))
  (check-false (rotation-count? 4))
  (check-false (rotation-count? 'blue))

  (define blue-V3 (tile 'blue (list (posn 0 0)
                                    (posn 0 1)
                                    (posn 1 1))))
  
  (check-equal? (tile-rotate blue-V3 0) blue-V3)

  (check-true (lset= equal?
                     (tile-posns (tile-rotate blue-V3 1))
                     (list (posn 0 1) (posn 1 1) (posn 1 0))))

  (check-true (lset= equal?
                     (tile-posns (tile-rotate blue-V3 2))
                     (list (posn 0 0) (posn 1 0) (posn 1 1))))

  (check-true (lset= equal?
                     (tile-posns (tile-rotate blue-V3 3))
                     (list (posn 0 0) (posn 0 1) (posn 1 0))))

  (check-true (tile-overlaps/posn? blue-V3 (posn 0 0)))
  (check-true (tile-overlaps/posn? blue-V3 (posn 0 1)))
  (check-true (tile-overlaps/posn? blue-V3 (posn 1 1)))
  (check-false (tile-overlaps/posn? blue-V3 (posn 1 0)))

  (check-true (tile-overlaps/tile? blue-V3 blue-I2))
  (check-true (tile-overlaps/tile? blue-V3 (tile-translate blue-I2 1 1)))
  (check-false (tile-overlaps/tile? blue-V3 (tile-translate blue-I2 1 0)))
  (check-false (tile-overlaps/tile? blue-V3 (tile-translate blue-I2 2 2)))
  
  (check-false (tile-touches? blue-I2 (tile-translate blue-I1 -1 1)))
  (check-true (tile-touches? blue-I2 (tile-translate blue-I1 0 1)))
  (check-true (tile-touches? blue-I2 (tile-translate blue-I1 1 1)))
  (check-false (tile-touches? blue-I2 (tile-translate blue-I1 2 1)))
  (check-true (tile-touches? blue-I2 (tile-translate blue-I1 -1 0)))
  (check-true (tile-touches? blue-I2 (tile-translate blue-I1 2 0)))
  (check-false (tile-touches? blue-I2 (tile-translate blue-I1 -1 -1)))
  (check-true (tile-touches? blue-I2 (tile-translate blue-I1 0 -1)))
  (check-true (tile-touches? blue-I2 (tile-translate blue-I1 1 -1)))
  (check-false (tile-touches? blue-I2 (tile-translate blue-I1 2 -1)))

  (check-true (tile-on-corner? blue-I2 (tile-translate blue-I1 -1 1)))
  (check-true (tile-on-corner? blue-I2 (tile-translate blue-I1 2 1)))
  (check-true (tile-on-corner? blue-I2 (tile-translate blue-I1 -1 -1)))
  (check-true (tile-on-corner? blue-I2 (tile-translate blue-I1 2 -1)))
  (check-false (tile-on-corner? blue-I2 (tile-translate blue-I1 5 5)))
  
  (check-false (pass-count? -1))
  (check-true (pass-count? 0))
  (check-true (pass-count? 1))
  (check-true (pass-count? 2))
  (check-false (pass-count? 3))
  (check-false (pass-count? 'blue))

  (check-false (state-game-over? (state empty start-tiles 0)))
  (check-false (state-game-over? (state empty start-tiles 1)))
  (check-true (state-game-over? (state empty start-tiles 2)))
  
  (check-equal? (state-played start-state) empty)
  (check-equal? (state-unplayed start-state) start-tiles)
  (check-equal? (state-passes start-state) 0)
  
  (check-pred blue? (state-whose-turn start-state))
  
  (check-true (pass? 'pass))
  (check-false (pass? 7))
  (check-false (pass? (move blue-I1 0 0 0)))
  
  (define blue-L5 (first (state-unplayed start-state)))
  (define move-blue-L5 (move blue-L5 4 5 0))

  (check-equal? (move-tile move-blue-L5) (first (state-unplayed start-state)))
  (check-equal? (move-x move-blue-L5) 4)
  (check-equal? (move-y move-blue-L5) 5)
  (check-equal? (move-rotations move-blue-L5) 0)
  
  (check-equal? (move->tile-on-board move-blue-L5)
                (tile-translate (first (state-unplayed start-state)) 4 5))
                                   
  (check-true (action? 'pass))
  (check-true (action? move-blue-L5))
  (check-false (action? 'blue))

  ;; can't pass on blue's first turn
  (check-false (state-action-legal? start-state 'pass))
  (check-pred string? (state-action-violation start-state 'pass))
  
  ;; does not cover (5, 5)
  (check-false (state-action-legal? start-state (move blue-I1 1 1 0)))
  (check-pred string? (state-action-violation start-state (move blue-I1 1 1 0)))
  
  ;; wrong color tile
  (check-false (state-action-legal? start-state (move orange-I1 5 5 0)))
  (check-pred string? (state-action-violation start-state (move orange-I1 5 5 0)))
  
  ;; off the board
  (check-false (state-action-legal? start-state (move blue-I2 15 1 0)))
  (check-pred string? (state-action-violation start-state (move blue-I2 15 1 0)))
  
  ;; OK
  (check-true (state-action-legal? start-state move-blue-L5))
  (check-false (state-action-violation start-state move-blue-L5))
  (define turn2 (state-transition start-state move-blue-L5))
  (check-equal? (length (state-played turn2)) 1)
  (check-equal? (length (state-unplayed turn2)) 41)
  (check-pred blue? (tile-color (first (state-played turn2))))
  (check-equal? (state-passes turn2) 0)
  (check-pred orange? (state-whose-turn turn2))
  
  ;; can't pass on orange's first turn
  (check-false (state-action-legal? turn2 'pass))
  (check-pred string? (state-action-violation turn2 'pass))
  
  ;; does not cover (10, 10)
  (check-false (state-action-legal? turn2 (move orange-I1 9 9 0)))
  (check-pred string? (state-action-violation turn2 (move orange-I1 9 9 0)))
  
  ;; wrong color tile
  (check-false (state-action-legal? turn2 (move blue-I2 1 1 0)))
  (check-pred string? (state-action-violation turn2 (move blue-I2 1 1 0)))
  
  ;; OK
  (check-true (state-action-legal? turn2 (move orange-I1 10 10 1)))
  (check-false (state-action-violation turn2 (move orange-I1 10 10 1)))
  (define turn3 (state-transition turn2 (move orange-I1 10 10 1)))
  (check-equal? (length (state-played turn3)) 2)
  (check-equal? (length (state-unplayed turn3)) 40)
  (check-pred orange? (tile-color (first (state-played turn3))))
  (check-equal? (state-passes turn3) 0)
  (check-pred blue? (state-whose-turn turn3))
  
  ;; pass is legal
  (check-true (state-action-legal? turn3 'pass))
  (check-false (state-action-violation turn3 'pass))
  
  ;; tile already played
  (check-false (state-action-legal? turn3 (move blue-L5 1 1 0)))
  (check-pred string? (state-action-violation turn3 (move blue-L5 1 1 0)))
  
  ;; overlaps previously played tile
  (check-false (state-action-legal? turn3 (move blue-I1 5 5 0)))
  (check-pred string? (state-action-violation turn3 (move blue-I1 5 5 0)))
  
  ;; next to tile of same color
  (check-false (state-action-legal? turn3 (move blue-I1 5 4 0)))
  (check-pred string? (state-action-violation turn3 (move blue-I1 5 4 0)))
  
  ;; OK
  (check-true (tile-on-corner? (second (state-played turn3)) 
                               (tile-translate blue-I1 3 4)))
  (check-true (state-action-legal? turn3 (move blue-I1 3 4 0)))
  (check-equal? (state-action-violation turn3 (move blue-I1 3 4 0)) #f)
  (define turn4 (state-transition turn3 (move blue-I1 3 4 0)))
  (check-equal? (length (state-played turn4)) 3)
  (check-equal? (length (state-unplayed turn4)) 39)
  (check-pred blue? (tile-color (first (state-played turn4))))
  (check-equal? (state-passes turn4) 0)
  (check-pred orange? (state-whose-turn turn4))
  
  ;; orange passes
  (check-true (state-action-legal? turn4 'pass))
  (check-false (state-action-violation turn4 'pass))
  (define turn5 (state-transition turn4 'pass))
  (check-equal? (length (state-played turn5)) 3)
  (check-equal? (length (state-unplayed turn5)) 39)
  (check-pred blue? (tile-color (first (state-played turn5))))
  (check-equal? (state-passes turn5) 1)
  (check-pred blue? (state-whose-turn turn5))
  
  ;; if blue passes then game is over
  (check-true (state-action-legal? turn5 'pass))
  (check-false (state-action-violation turn5 'pass))
  (define game-over (state-transition turn5 'pass))
  (check-equal? (state-passes game-over) 2)
  (check-true (state-game-over? game-over))
  
  ;; if blue takes a move then pass counter resets to zero
  (define blue-I3 (tile 'blue (list (posn 0 0) (posn 1 0) (posn 2 0))))
  (check-true (state-action-legal? turn5 (move blue-I3 8 7 0)))
  (check-equal? (state-action-violation turn5 (move blue-I3 8 7 0)) #f)
  (define turn6 (state-transition turn5 (move blue-I3 9 6 0)))
  (check-equal? (state-passes turn6) 0)

  ;; My code says there are 264 legal moves in the first turn. I didn't
  ;; analyze this by hand so I am not 100% certain that figure is correct,
  ;; but it's surely in the right ballpark.
  
  ;; 1 domino * 2 posns covering (5, 5) each * 4 rotations = 8
  ;; 2 triominoes * 3 posns covering (5, 5) each * 4 rotations = 24
  ;; 5 tetrominoes * 4 posns covering (5, 5) each * 4 rotations = 80
  ;; 12 pentominoes * 5 posns covering (5, 5) each * 4 rotations = 240
  ;; 8+24+80+240 = 356
  (define start-state-children-count 356)
  (check-equal? (length (state-legal-actions start-state))
                start-state-children-count)
  (check-equal? (count pass? (state-legal-actions start-state)) 0)
  
  (check-equal? (length (state-children start-state)) start-state-children-count)
  
  ;; check scoring for the previous series of game states
  (check-equal? (state-score start-state 'blue) -89)
  (check-equal? (state-score start-state 'orange) -89)
  
  (check-equal? (state-score turn2 'blue) -84)
  (check-equal? (state-score turn2 'orange) -89)
  
  (check-equal? (state-score turn3 'blue) -84)
  (check-equal? (state-score turn3 'orange) -88)
  
  (check-equal? (state-score turn4 'blue) -83)
  (check-equal? (state-score turn4 'orange) -88)

  (check-equal? (state-score turn5 'blue) -83)
  (check-equal? (state-score turn5 'orange) -88)

  (check-equal? (state-score game-over 'blue) -83)
  (check-equal? (state-score game-over 'orange) -88)
  
  (check-equal? (state-score turn6 'blue) -80)
  (check-equal? (state-score turn6 'orange) -88)
  
  ;; +20 points for playing all pieces ending with the monomino
  (define orange-start-tiles (filter tile-orange? start-tiles))
  (define blue-start-tiles (map (cut tile-translate <> 1 1)
                                (filter tile-blue? start-tiles)))
  (define (tile-length=1? tile)
    (= 1 (tile-length tile)))
  (define blue-start-tiles/monomino-first (cons (find tile-length=1? blue-start-tiles)
                                                (filter (negate tile-length=1?) blue-start-tiles)))
  (check-equal? (state-score (state blue-start-tiles/monomino-first
                                    orange-start-tiles
                                    0)
                             'blue)
                20)
 
  ;; +15 points for playing all pieces but not ending with the monomino
  (check-equal? (state-score (state (reverse blue-start-tiles/monomino-first)
                                    orange-start-tiles
                                    0)
                             'blue)
                15)

  ;;;; END OF PROVIDED UNIT TESTS

  ;;;; (replace this with your own unit tests, if any)

  )

