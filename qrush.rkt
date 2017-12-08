#lang racket

;====================================================================================================
;============██████████████====███████====██====██===█████████====██====██===========================
;============██████████████====██====██===██====██===██===========██====██===========================
;============██████████████====██====██===██====██===██===========██====██===========================
;============██████████████====███████====██====██===█████████====████████===========================
;============██████████████====██==██=====██====██==========██====██====██===========================
;============██████████████====██===██====██====██==========██====██====██===========================
;============██████████████====██=====██==████████===█████████====██====██===========================
;====================================================================================================

(require 2htdp/image)
(require 2htdp/universe)
(require test-engine/racket-tests)

(define HEIGHT 700)
(define WIDTH (/ (* HEIGHT 9) 16))

(define SCENE
  (empty-scene WIDTH HEIGHT))

(define BACKGROUND
  (rectangle WIDTH HEIGHT "solid" "black"))

(define BLOCK-SIZE
  (/ WIDTH 5))

(define BULLET-SIZE
  (/ WIDTH 50))

(define BULLET-SPRITE
  (circle BULLET-SIZE "solid" "white"))

(define PLAYER-SPRITE
  (square BLOCK-SIZE "solid" "cyan"))

; WorldState is a (make-world player bullets walls score time) where:
; - player is a Player
; - bullets is a ListOf<Bullet>
; - walls is a ListOf<Wall>
; - score is a PosInt
; - time is a PosInt
; Intrpr: It is the current state of the world at any given time during our
; big bang application
(define-struct world [player bullets walls score time])

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

; draw-bullets: ListOf<Bullet> Image --> Image
(define (draw-bullets bullets image)
  (cond
    [(empty? bullets) image]
    [else (draw-bullets (rest bullets) (place-image
                                        (circle BULLET-SIZE "solid" "white")
                                        (bullet-x (first bullets)) (bullet-y (first bullets))
                                        image))])
  )

; draw-blocks: ListOf<Block> Image --> Image
(define (draw-blocks blocks image)
  (cond
    [(empty? blocks) image]
    [else (draw-blocks (rest blocks) (place-image
                                      (text (number->string (inexact->exact (block-hp (first blocks)))) 20 "white")
                                      (block-x (first blocks)) (block-y (first blocks))
                                      (place-image
                                       (square BLOCK-SIZE "solid" (pick-color
                                                                   1
                                                                   15
                                                                   (block-hp (first blocks))))
                                       (block-x (first blocks)) (block-y (first blocks))
                                       image)))]))

; pick-color: PosInt PosInt HP --> String
; Given a min and a max and a block, returns the color of the block
(define (pick-color min max hp)
  (local [(define width
            (- max min))]
  (cond
    [(and
      (>= hp min)
      (<= hp (+ (/ width 5) min))) "DodgerBlue"]
    [(and
      (> hp (+ (/ width 5) min))
      (<= hp (+ (* (/ width 5) 2) min))) "LawnGreen"]
    [(and
      (> hp (+ (* (/ width 5) 2) min))
      (<= hp (+ (* (/ width 5) 3) min))) "Gold"]
    [(and
      (> hp (+ (* (/ width 5) 3) min))
      (<= hp (+ (* (/ width 5) 4) min))) "DarkOrange"]
    [(and
      (> hp (+ (* (/ width 5) 4) min))
      (<= hp max)) "OrangeRed"]
    [(> hp max) "Red"])))
  

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
  (place-image
   (text (string-append "Score: " (number->string (world-score w))) 30 "white")
   (/ WIDTH 2) (/ WIDTH 10)
   (draw-bullets (world-bullets w)
                 (draw-walls (world-walls w)
                             (place-image
                              (scale (/ (player-size (world-player w)) 100) PLAYER-SPRITE)
                              (player-x (world-player w)) (player-y (world-player w))
                              BACKGROUND))))
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
     (world-score w)
     (world-time w))
    )
  )

; move-blocks: ListOf<Block> Time --> ListOf<Block>
; Given a list of blocks, it returns a list of blocks with all its elements with updated position
(define (move-blocks blocks t)
  (map (lambda (block) (make-block
                        (block-x block)
                        (+ (block-y block) (block-dy block))
                        (+ (- (log (/ 1 (expt (/ t 60) 2)))) 3)
                        (block-hp block)
                        (block-state block)))
       blocks)
  )

; move-walls: ListOf<Wall> Time --> ListOf<Wall>
; Given a list of walls it returns an updated list of walls
(define (move-walls walls t)
  (map (lambda (x) (move-blocks x t)) walls))

; create-bullet: Pixel Pixel --> Bullet
(define (create-bullet x y)
  (make-bullet
   x
   y
   0
   -10)
  )

; move-bullets: ListOfBullets --> ListOfBullets
(define (move-bullets bullets)
  (map (lambda (bullet) (make-bullet
                         (+ (bullet-x bullet) (bullet-dx bullet))
                         (+ (bullet-y bullet) (bullet-dy bullet))
                         (bullet-dx bullet)
                         (bullet-dy bullet))) bullets
       )
  )

; check-block? Block ListOf<Bullet> --> Boolean
(define (check-block? block bullets)
  (cond
    [(empty? bullets) #false]
    [(and
      (<= (bullet-x (first bullets)) (+ (block-x block) (/ WIDTH 10) (/ BULLET-SIZE 2)))
      (>= (bullet-x (first bullets)) (- (block-x block) (/ WIDTH 10) (/ BULLET-SIZE 2)))
      (<= (bullet-y (first bullets)) (+ (block-y block) (/ WIDTH 10) (/ BULLET-SIZE 2)))
      (>= (bullet-y (first bullets)) (- (block-y block) (/ WIDTH 10) (/ BULLET-SIZE 2)))) #true]
    [else (check-block? block (rest bullets))]))

; block-hit? ListOf<Block> ListOf<Bullet> --> Boolean
; Given a list of blocks and bullets, return #true if a bullets hits a block, else #false
(define (block-hit? blocks bullets)
  (cond
    [(empty? blocks) #false]
    [(check-block? (first blocks) bullets) #true]
    [else (block-hit? (rest blocks) bullets)])
  )

; check-block Block ListOf<Bullet> --> Block
(define (check-block block bullets)
  (cond
    [(empty? bullets) '()]
    [(and
      (<= (bullet-x (first bullets)) (+ (block-x block) (/ WIDTH 10) (/ BULLET-SIZE 2)))
      (>= (bullet-x (first bullets)) (- (block-x block) (/ WIDTH 10) (/ BULLET-SIZE 2)))
      (<= (bullet-y (first bullets)) (+ (block-y block) (/ WIDTH 10) (/ BULLET-SIZE 2)))
      (>= (bullet-y (first bullets)) (- (block-y block) (/ WIDTH 10) (/ BULLET-SIZE 2))))
     block]
    [else (check-block block (rest bullets))]))
  

; find-block ListOf<Block> ListOf<Bullet> --> Block
; Given a list of blocks and bullets, return the block being hit or empty list
(define (find-block blocks bullets)
  (cond
    [(empty? blocks) '()]
    [(block? (check-block (first blocks) bullets)) (check-block (first blocks) bullets)]
    [else (find-block (rest blocks) bullets)])
  )

; find-bullet ListOf<Bullet> ListOf<Wall> --> Bullet
; Given a list of bullets, return the bullet that hit the wall
(define (find-bullet bullets walls)
  (cond [(empty? bullets) '()]
        [(wall-hit? walls (list (first bullets))) (first bullets)]
        [else (find-bullet (rest bullets) walls)]))


  
; wall-hit? ListOf<Wall> ListOf<Bullet> --> Boolean
; Given a list of walls and bullets, return #true if a bullet hits the wall, else #false
(define (wall-hit? walls bullets)
  (cond
    [(empty? walls) #false]
    [(block-hit? (first walls) bullets) #true]
    [else (wall-hit? (rest walls)  bullets)])
  )

; block-hit ListOf<Wall> ListOf<Bullet> --> Block
; Given a list of walls and bullets, return the block that was hit
(define (block-hit walls bullets)
  (cond
    [(empty? walls) '()]
    [(block? (find-block (first walls) bullets)) (find-block (first walls) bullets)]
    [else (block-hit (rest walls)  bullets)])
  )

; wall-hit: ListOf<Wall> ListOf<Bullet> --> Wall
; Given a list of walls and  bullets, return the wall that was hit
(define (wall-hit walls bullets)
  (cond
    [(empty? walls) '()]
    [(block-hit? (first walls) bullets) (first walls)]
    [else (wall-hit (rest walls) bullets)])
  )

; damage-wall: ListOf<Wall> ListOf<Bullets> Time --> ListOf<Wall>
; Given a hit wall, a hit block and a list of walls, it returns the updated list of walls
(define (damage-wall walls bullets t)
  (local [(define hit-wall
            (wall-hit walls bullets))
          (define hit-block
            (block-hit walls bullets))]
    (move-walls
     (cons
      (remove-blocks
       (cons
        (make-block
         (block-x hit-block)
         (block-y hit-block)
         (block-dy hit-block)
         (sub1 (block-hp hit-block))
         (block-state hit-block))
        (remove hit-block hit-wall)) '()) (remove hit-wall walls)) t)))

  
; remove-blocks: ListOf<Block> EmptyList --> ListOf<Block>
; Given a list of walls, it deletes any block with less than 1 hp
(define (remove-blocks blocks-old new)
  (cond
    [(empty? blocks-old) new]
    [(<= (block-hp (first blocks-old)) 0) (remove-blocks (rest blocks-old) new)]
    [else (remove-blocks (rest blocks-old) (cons (first blocks-old) new))]))



; animate-world: World --> World
; Given a world state, it return the walls with the updated position
(define (animate-world world)
  (local
    [(define p (world-player world))
     (define t (world-time world))
     (define b (world-bullets world))
     (define w (world-walls world))
     (define s (world-score world))]
  (make-world
   p
   (cond
     [(empty? b)
      (move-bullets (cons
       (create-bullet (player-x p) (- HEIGHT (/ WIDTH 5) (/ WIDTH 10))) b))]
     [(= (modulo t 9) 0)
      (move-bullets (cons
       (create-bullet (player-x p) (- HEIGHT (/ WIDTH 5) (/ WIDTH 10))) b))]
     [(< (bullet-y (last b)) 0)
      (move-bullets (remove (last b) b))]
     [(wall-hit? w b)
      (move-bullets (remove (find-bullet b w) b))]
     [else (move-bullets b)])
   (cond
     [(wall-hit? w b)
      (damage-wall w b t)]
     [(and (new-wall? w) (< (length w) 2)) (create-wall w t)]
     [(and (kill-wall? w) (> (length w) 1)) (delete-wall w t)]
     [else (move-walls w t)])
   (if (wall-hit? w b)
       (add1 s)
       s)
   (add1 t))
  ))

; new-wall?: ListOf<Wall> --> Boolean
; Given a list of walls, returns #true if a wall is at the position ]
; where a new one should be spawned at the top of the screen
(define (new-wall? walls)
  (cond
    [(empty? walls) #false]
    [(and (cons? (last walls)) (> (block-y (first (last walls))) (ceiling (/ HEIGHT 1.75)))) #true]
    [else (new-wall? (rest walls))])
  )

; kill-wall?: ListOf<Wall> --> Boolean
; Given a list of walls, returns #true if a wall exits the frame
(define (kill-wall? walls)
  (cond
    [(empty? walls) #false]
    [(and (cons? (last walls)) (> (block-y (first (last walls))) (ceiling (+ HEIGHT (/ WIDTH 10))))) #true]
    [else (kill-wall? (rest walls))])
  )

; create-wall: ListOf<Wall> Time --> ListOf<Wall>
; Given a list of walls and a time, inserts a new wall
(define (create-wall walls t)
  (move-walls (cons (generate-wall t) walls) t)
  )

; delete-wall: ListOf<Wall> --> ListOf<Wall>
; Given a list of wall, deletes the oldest wall
(define (delete-wall walls t)
  (move-walls (remove (last walls) walls) t)
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

; create-block: PosInt Time --> Block
; Given a number (order) and the time elapsed, creates a block with more and more health as the time goes by.
(define (create-block n t)
  (make-block
   (+ (* n (/ WIDTH 5)) (/ WIDTH 10))
   (floor (- 0 (/ WIDTH 5)))
   (+ (- (log (/ 1 (expt (/ t 60) 2)))) 3)
   (ceiling (* (random) (ceiling (* 5 (log (sqrt (/ t 60)))))))
   #f))
; generate-wall Time --> ListOf<Block>
; Given the time elapsed, creates a new wall made up by smaller blocks.
(define (generate-wall t)
  (build-list 5 (lambda (x) (create-block x t))))
    

(define EXAMPLE-WALL
  (generate-wall 300)
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
   (list
    (create-bullet (/ WIDTH 2) (- HEIGHT (/ WIDTH 5) (/ WIDTH 10))))
   (list
    EXAMPLE-WALL)
   0
   1)
  )


(big-bang INITIAL-STATE
          [to-draw draw-world]
          [on-tick animate-world (/ 1 60)]
          [stop-when detect-collision]
          [on-mouse mouse-handler])