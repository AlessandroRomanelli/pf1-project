#lang racket

;====================================================================================================
;============██████████████====███████====██====██===█████████====██====██===========================
;============██==========██====██====██===██====██===██===========██====██===========================
;============██==========██====██====██===██====██===██===========██====██===========================
;============██==========██====███████====██====██===█████████====████████===========================
;============██==========██====██==██=====██====██==========██====██====██===========================
;============██==========██====██===██====██====██==========██====██====██===========================
;============██████████████====██=====██==████████===█████████====██====██===========================
;====================================================================================================

(require 2htdp/image)
(require 2htdp/universe)

(define HEIGHT 700)
(define WIDTH (/ (* HEIGHT 9) 16))

(define SCENE
  (empty-scene WIDTH HEIGHT))

(define BACKGROUND
  (rectangle WIDTH HEIGHT "solid" "black"))

(define BLOCK-SIZE
  (/ WIDTH 5))

(define BULLET-SIZE
  (/ WIDTH 30))

(define BULLET-SPRITE
  (circle BULLET-SIZE "solid" "white"))

(define PLAYER-SPRITE
  (square BLOCK-SIZE "solid" "cyan"))

; WorldState is a (make-world player bullets walls score) where:
; - player is a Player
; - bullets is a ListOf<Bullet>
; - walls is a ListOf<Wall>
; - score is a PosInt
; Intrpr: It is the current state of the world at any given time during our
; big bang application
(define-struct world [player bullets walls score])

; Player is a (make-player size x y) where:
; - size is a PosInt
; - x is a Pixel
; - y is a Pixel
; Intrpr: It is the variable holding all the information relative to the player's sprite
(define-struct player [size x y])

; Bullet is a (make-bullet x y dx dy) where:
; - x is a Pixel
; - y is a Pixel
; - dx is a Speed
; - dy is a Speed
; Intrpr: It is the single bullet being fired by the player
(define-struct bullet [x y dx dy])

; Pixel is a PosInt
; Intrpr: It is a single component of a position in a image

; Speed is a Int
; Intrpr: It is the change in position over time

; Wall is a ListOf<Block>
; Intrpr: It is a single row of blocks coming towards the player

; Block is a (make-block x y dy hp state) where:
; - x is a Pixel
; - y is a Pixel
; - dy is a Speed
; - hp is a PosInt
; - state is a StringOrFalse
; Intrpr: It is the smallest unit of a wall, with a position, a vertical speed, health and a
; state if the block is special else #false.
(define-struct block [x y dy hp state])

; Score is a PosInt
; Intpr: it is the score held by the player

; draw-blocks: ListOf<Block> Image --> Image
(define (draw-blocks blocks image)
  (cond
    [(empty? blocks) image]
    [else (draw-blocks (rest blocks) (place-image
                                      (square BLOCK-SIZE "solid" "green")
                                      (block-x (first blocks)) (block-y (first blocks))
                                      image))]
    )
  )
    

; draw-walls: ListOf<Wall> Image --> Image
(define (draw-walls walls image)
  (cond
    [(empty? walls) image]
    [else (draw-walls (rest walls) (draw-blocks (first walls) image))]
    )
  )

; draw-world: World --> Image
; Given a world state it returns the current representation of the ongoing game
(define (draw-world w)
  (draw-walls (world-walls w)
  (place-image
   (scale (/ (player-size (world-player w)) 100) PLAYER-SPRITE)
   (player-x (world-player w)) (player-y (world-player w))
   BACKGROUND))
  )

; mouse-handler: World x y Event --> World
; Given a world state, two mouse coordinates and an event, it returns a new updated world
(define (mouse-handler w x y event)
  (cond
    [(string=? "move" event) (move-player w x y)]
    [else w])
  )

; move-player: World X Y --> World
(define (move-player w x y)
  (local [(define PLAYER-WIDTH
            (* (/ WIDTH 5) (/ (player-size (world-player w)) 100)))]
    (make-world
     (make-player
      (player-size (world-player w))
      (cond
        [(> x (- WIDTH (/ PLAYER-WIDTH 2)))
          (- WIDTH (/ PLAYER-WIDTH 2))]
        [(< x (/ PLAYER-WIDTH 2))
          (/ PLAYER-WIDTH 2)]
        [else x])
      (player-y (world-player w))
      )
     (world-bullets w)
     (world-walls w)
     (world-score w))
    )
  )

; move-blocks: ListOf<Block> --> ListOf<Block>
; Given a list of blocks, it returns a list of blocks with all its elements with updated position
(define (move-blocks blocks)
  (map (lambda (block) (make-block
                        (block-x block)
                        (+ (block-y block) (block-dy block))
                        (block-dy block)
                        (block-hp block)
                        (block-state block)))
       blocks)
  )

; move-walls: ListOf<Wall> --> ListOf<Wall>
; Given a list of walls it returns an updated list of walls
(define (move-walls walls)
  (map move-blocks walls))

; animate-world: World --> World
; Given a world state, it return the walls with the updated position
(define (animate-world w)
  (make-world
   (world-player w)
   (world-bullets w)
   (cond
     [(and (new-wall? (world-walls w)) (< (length (world-walls w)) 2)) (create-wall (world-walls w))]
     [(and (kill-wall? (world-walls w)) (> (length (world-walls w)) 1)) (delete-wall (world-walls w))]
     [else (move-walls (world-walls w))])
   (world-score w))
  )

; new-wall?: ListOf<Wall> --> Boolean
; Given a list of walls, returns #true if a wall is at the position ]
; where a new one should be spawned at the top of the screen
(define (new-wall? walls)
  (cond
    [(empty? walls) #false]
    [(> (block-y (first (last walls))) (ceiling (/ HEIGHT 1.75))) #true]
    [else (new-wall? (rest walls))])
  )

; kill-wall?: ListOf<Wall> --> Boolean
; Given a list of walls, returns #true if a wall exits the frame
(define (kill-wall? walls)
  (cond
    [(empty? walls) #false]
    [(> (block-y (first (last walls))) (ceiling (+ HEIGHT (/ WIDTH 10)))) #true]
    [else (kill-wall? (rest walls))])
  )

; create-wall: ListOf<Wall> --> ListOf<Wall>
; Given a list of wall, inserts a new wall
(define (create-wall walls)
  (move-walls (cons EXAMPLE-WALL walls))
  )

; delete-wall: ListOf<Wall> --> ListOf<Wall>
; Given a list of wall, deletes the oldest wall
(define (delete-wall walls)
  (move-walls (remove (last walls) walls))
  )

; detect-collision: World --> Boolean
; Given a world state, returns #true if there was a collision between the player and a block
(define (detect-collision w)
  (check-collision (last (world-walls w)) (world-player w))
  )

; check-collision: ListOf<Block> Player --> Boolean
; Given a list of blocks and a player, returns #true if there was a collision
(define (check-collision blocks player)
  (local [(define PLAYER-WIDTH
            (* (/ WIDTH 5) (/ (player-size player) 100)))]
    (cond
      [(empty? blocks) #false]
      [(and
        (<= (player-x player) (+ (block-x (first blocks)) (/ WIDTH 10) (/ PLAYER-WIDTH 2)))
        (>= (player-x player) (- (block-x (first blocks)) (/ WIDTH 10) (/ PLAYER-WIDTH 2)))
        (<= (player-y player) (+ (block-y (first blocks)) (/ WIDTH 10) (/ PLAYER-WIDTH 2)))
        (>= (player-y player) (- (block-y (first blocks)) (/ WIDTH 10) (/ PLAYER-WIDTH 2)))) #true]
      [else (check-collision (rest blocks) player)])
    )
  )

(define EXAMPLE-WALL
  (list
   (make-block
    (+ (* 0 (/ WIDTH 5)) (/ WIDTH 10))
    (floor (- 0 (/ WIDTH 5)))
    5
    100
    #f)
   (make-block
    (+ (* 1 (/ WIDTH 5)) (/ WIDTH 10))
    (floor (- 0 (/ WIDTH 5)))
    5
    100
    #f)
   (make-block
    (+ (* 2 (/ WIDTH 5)) (/ WIDTH 10))
    (floor (- 0 (/ WIDTH 5)))
    5
    100
    #f)
   (make-block
    (+ (* 3 (/ WIDTH 5)) (/ WIDTH 10))
    (floor (- 0 (/ WIDTH 5)))
    5
    100
    #f)
   (make-block
    (+ (* 4 (/ WIDTH 5)) (/ WIDTH 10))
    (floor (- 0 (/ WIDTH 5)))
    5
    100
    #f))
  )
   

(define INITIAL-PLAYER
  (make-player
   33
   (/ WIDTH 2)
   (- HEIGHT (/ WIDTH 5)))
  )

(define INITIAL-STATE
  (make-world
   INITIAL-PLAYER
   '()
   (list
    EXAMPLE-WALL)
   0)
  )


(big-bang INITIAL-STATE
          [to-draw draw-world]
          [on-tick animate-world (/ 1 60)]
          [stop-when detect-collision]
          [on-mouse mouse-handler])

