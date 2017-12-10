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
(require rsound)

(define song1 (rs-read "Theory_Of_Everything.wav"))
(define song2 (rs-read "dryout.wav"))
(define song3 (rs-read "Highscore.wav"))
(define song4 (rs-read "Vortex.wav"))




(define playlist (list song1 song2 song3 song4))

(pstream-play (make-pstream #:buffer-time 0.900)
              (list-ref playlist (inexact->exact (floor (* (random) (length playlist))))))

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

; WorldState is a (make-world player bullets walls score time pause) where:
; - player is a Player
; - bullets is a ListOf<Bullet>
; - walls is a ListOf<Wall>
; - score is a PosInt
; - time is a PosInt
; - record is a PosInt
; - last is a PosInt
; - state is a GameState
; Intrpr: It is the current state of the world at any given time during our
; big bang application, holding all the key information relevant to the world
(define-struct world [player bullets walls score time record last pause])

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

; Block is a (make-block x y dy hp state) where:
; - x is a Pixel
; - y is a Pixel
; - dy is a Speed
; - hp is a PosInt
; - state is a StringOrFalse
; Intrpr: It is the smallest unit of a wall, with a position, a vertical speed, health and a
; state if the block is special else #false.
(define-struct block [x y dy hp state])

; Pixel is a PosInt
; Intrpr: It is a single component of a position in a image

; Speed is a Int
; Intrpr: It is the change in position over time

; Wall is a ListOf<Block>
; Intrpr: It is a single row of blocks coming towards the player

; GameState is one of:
; - 0: for the main-menu
; - 1: for when the game starts
; - 2: for when the game is over
; - 3: for when the game is paused
; Intrpr: it is the current state of the game

; Score is a PosInt
; Intpr: it is the score held by the player

; draw-bullets: ListOf<Bullet> Image --> Image
(define (draw-bullets bullets image)
  (cond
    [(empty? bullets) image]
    [else (draw-bullets (rest bullets) (place-image
                                        (circle BULLET-SIZE "solid" "white")
                                        (bullet-x (first bullets)) (bullet-y (first bullets))
                                        image))]))

; draw-blocks: ListOf<Block> Image --> Image
(define (draw-blocks blocks image)
  (cond
    [(empty? blocks) image]
    [else (draw-blocks
           (rest blocks)
           (draw-block (first blocks) image))]))

; draw-block: Block Image --> Image
(define (draw-block block image)
  (place-image
   (if (> (block-hp block) 15)
       (empty-scene 0 0)
       (text (number->string (inexact->exact (block-hp block))) 20 "white"))
   (block-x block) (block-y block)
   (place-image
    (square BLOCK-SIZE "solid" (pick-color
                                1
                                16
                                (block-hp block)))
    (block-x block) (block-y block)
    image)))

; pick-color: PosInt PosInt HP --> String
; Given a min and a max and a block, returns the color of the block
(define (pick-color min max hp)
  (local [(define width
            (- max min))]
  (cond
    [(and
      (>= hp min)
      (<= hp (+ 1 min))) "DodgerBlue"]
    [(and
      (> hp (+ 1 min))
      (<= hp (+ 3 min))) "LawnGreen"]
    [(and
      (> hp (+ 3 min))
      (<= hp (+ 5 min))) "Gold"]
    [(and
      (> hp (+ 5 min))
      (<= hp (+ 7 min))) "DarkOrange"]
    [(and
      (> hp (+ 7 min))
      (<= hp max)) "OrangeRed"]
    [(> hp max) "LightSlateGray"])))
  

; draw-walls: ListOf<Wall> Image --> Image
(define (draw-walls walls image)
  (cond
    [(empty? walls) image]
    [else (draw-walls (rest walls) (draw-blocks (first walls) image))]))

; draw-world: World --> Image
; Given a world state it returns the current representation of the ongoing game
(define (draw-world w)
  (place-image
   (text (string-append "Previous: " (number->string (world-last w))) 10 "white")
   (/ WIDTH 4) (/ WIDTH 10)
   (place-image
    (text (string-append "Record: " (number->string (world-record w))) 10 "white")
    (- WIDTH (/ WIDTH 4)) (/ WIDTH 10)
    (place-image
     (text (string-append "Score: " (number->string (world-score w))) 30 "white")
     (/ WIDTH 2) (/ WIDTH 10)
     (draw-bullets (world-bullets w)
                   (draw-walls (world-walls w)
                               (place-image
                                (scale (/ (player-size (world-player w)) 100) PLAYER-SPRITE)
                                (player-x (world-player w)) (player-y (world-player w))
                                BACKGROUND)))))))

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
     (world-time w)
     (world-record w)
     (world-last w)
     (world-pause w))))

; move-blocks: ListOf<Block> Time --> ListOf<Block>
; Given a list of blocks, it returns a list of blocks with all its elements with updated position
(define (move-blocks blocks t)
  (map (lambda (block) (make-block
                        (block-x block)
                        (+ (block-y block) (block-dy block))
                        (log (+ (expt (/ t 60) 2) 10))
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
     (remove-wall
      (cons
       (remove-blocks
        (cons
         (make-block
          (block-x hit-block)
          (block-y hit-block)
          (block-dy hit-block)
          (sub1 (block-hp hit-block))
          (block-state hit-block))
         (remove hit-block hit-wall)) '()) (remove hit-wall walls)) '()) t)))

  
; remove-blocks: ListOf<Block> EmptyList --> ListOf<Block>
; Given a list of walls, it deletes any block with less than 1 hp
(define (remove-blocks blocks-old new)
  (cond
    [(empty? blocks-old) new]
    [(< (block-hp (first blocks-old)) 1) (remove-blocks (rest blocks-old) new)]
    [else (remove-blocks (rest blocks-old) (cons (first blocks-old) new))]))

; remove-wall: ListOf<Wall> EmptyList --> ListOf<Wall>
(define (remove-wall walls-old new)
  (cond
    [(empty? walls-old) new]
    [(empty? (first walls-old)) (remove-wall (rest walls-old) new)]
    [else (remove-wall (rest walls-old) (cons (first walls-old) new))]))


; animate-world: World --> World
; Given a world state, it return the walls with the updated position
(define (animate-world w)
  (if (check-wall-collision (world-walls w) (world-player w))
      (begin
        (stop)
        (pstream-play (make-pstream #:buffer-time 0.900)
                      (list-ref playlist (inexact->exact (floor (* (random) (length playlist))))))
        (if (> (world-score w) (world-record w))
            (new-game (world-time w) (world-score w) (world-score w))
            (new-game (world-time w) (world-record w) (world-score w))))
      (make-world
       (animate-player (world-player w) (world-time w))
       (animate-bullets (world-player w) (world-time w) (world-bullets w) (world-walls w))
       (animate-walls (world-time w) (world-bullets w) (world-walls w))
       (if (wall-hit? (world-walls w) (world-bullets w))
           (add1 (world-score w))
           (world-score w))
       (add1 (world-time w))
       (world-record w)
       (world-last w)
       (world-pause w))))

; animate-player: Player Time --> Player
(define (animate-player player time)
  (make-player
   (+ 20 (log time) (- 0 (* (log (/ time 120)) (cos (* 6 (sin (* 1/20 time)))))))
   (player-x player)
   (player-y player)))

; animate-bullets: Player Time ListOf<Bullet> ListOf<Wall> --> ListOf<Bullet>
(define (animate-bullets player time bullets walls)
   (cond
     [(spawn-time time)
      (move-bullets (cons
       (create-bullet (player-x player) (- HEIGHT (/ WIDTH 5) (/ WIDTH 10))) bullets))]
     [(and (cons? bullets) (< (bullet-y (last bullets)) 0))
      (move-bullets (remove (last bullets) bullets))]
     [(wall-hit? walls bullets)
      (move-bullets (remove (find-bullet bullets walls) bullets))]
     [else (move-bullets bullets)]))

; spawn-time: Time --> Boolean
; Given a time, returns true if it is time to spawn a new bullet
(define (spawn-time time)
  (cond
    [(and (> time 0) (<= time 900))
     (= (modulo time 20) 0)]
    [(and (> time 900) (<= time 3000))
     (= (modulo time 15) 0)]
    [(and (> time 3000) (<= time 10200))
     (= (modulo time 10) 0)]
    [(and (> time 10200) (<= time 32000))
     (= (modulo time 5) 0)]
    [else (= (modulo time 1))]))

; animate-walls: Time ListOf<Bullet> ListOf<Wall> --> ListOf<Wall>
(define (animate-walls time bullets walls)
  (cond
    [(wall-hit? walls bullets)
     (damage-wall walls bullets time)]
    [(and (new-wall? walls time) (< (length walls) 2)) (create-wall walls time)]
    [(wall-too-low? walls) (delete-wall walls '() time)]
    [else (move-walls walls time)]))

; new-wall?: ListOf<Wall> Time --> Boolean
; Given a list of walls, returns #true if a wall is at the position ]
; where a new one should be spawned at the top of the screen
(define (new-wall? walls time)
  (or (empty? walls) (wall-exit? walls)))

; wall-exit?: ListOf<Wall> --> Boolean
(define (wall-exit? walls)
  (cond
    [(empty? walls) #false]
    [(block-exit? (first walls)) #true]
    [else (wall-exit? (rest walls))]))

; block-exit?: ListOf<Block> --> Boolean
(define (block-exit? blocks)
  (cond
    [(empty? blocks) #false]
    [(> (block-y (first blocks)) (ceiling (/ HEIGHT 1.75))) #true]
    [else (block-exit? (rest blocks))]))

; wall-too-low?: ListOf<Wall> --> Boolean
(define (wall-too-low? walls)
  (cond
    [(empty? walls) #false]
    [(block-too-low? (first walls)) #true]
    [else (wall-too-low? (rest walls))]))

; block-too-low? : ListOf<Block> --> Boolean
(define (block-too-low? blocks)
  (cond
    [(empty? blocks) #false]
    [(> (block-y (first blocks)) (ceiling (+ HEIGHT (/ WIDTH 10)))) #true]
    [else (block-too-low? (rest blocks))]))
    
    

; create-wall: ListOf<Wall> Time --> ListOf<Wall>
; Given a list of walls and a time, inserts a new wall
(define (create-wall walls t)
  (move-walls (cons (generate-wall t) walls) t)
  )

; delete-wall: ListOf<Wall> EmptyList Time --> ListOf<Wall>
; Given a list of wall, deletes the oldest wall
(define (delete-wall walls new t)
  (cond
    [(empty? walls) new]
    [(wall-too-low? (list (first walls))) (delete-wall (rest walls) new t)]
    [else (delete-wall (rest walls) (cons (first walls) new) t)]))

; check-wall-collision: ListOf<Wall> Player --> Boolean
(define (check-wall-collision walls player)
  (cond
    [(empty? walls) #false]
    [(check-block-collision (first walls) player) #true]
    [else (check-wall-collision (rest walls) player)]))
  

; check-block-collision: ListOf<Block> Player --> Boolean
; Given a list of blocks and a player, returns #true if there was a collision
(define (check-block-collision blocks player)
  (local [(define PLAYER-WIDTH
            (* (/ WIDTH 5) (/ (player-size player) 100)))]
    (cond
      [(empty? blocks) #false]
      [(and
        (<= (player-x player) (+ (block-x (first blocks)) (/ WIDTH 10) (/ PLAYER-WIDTH 2)))
        (>= (player-x player) (- (block-x (first blocks)) (/ WIDTH 10) (/ PLAYER-WIDTH 2)))
        (<= (player-y player) (+ (block-y (first blocks)) (/ WIDTH 10) (/ PLAYER-WIDTH 2)))
        (>= (player-y player) (- (block-y (first blocks)) (/ WIDTH 10) (/ PLAYER-WIDTH 2)))) #true]
      [else (check-block-collision (rest blocks) player)])
    )
  )

; create-block: PosInt Time --> Block
; Given a number (order) and the time elapsed, creates a block with more and more health as the
; time goes by.
(define (create-block n t)
  (make-block
   (+ (* n (/ WIDTH 5)) (/ WIDTH 10))
   (floor (- 0 (/ WIDTH 5)))
   (- 0 (* 3 (log (/ 1 (* 2 (/ t 60))))))
   (if (< (* (random) 5) 1)
       1000
       (ceiling (* (random) (abs (ceiling (log (/ t 60)))))))
   #f))
; generate-wall Time --> ListOf<Block>
; Given the time elapsed, creates a new wall made up by smaller blocks.
(define (generate-wall t)
  (build-list 5 (lambda (x) (create-block x t))))
   

(define INITIAL-PLAYER
  (make-player
   33
   (/ WIDTH 2)
   (- HEIGHT (/ WIDTH 5)))
  )

; new-game: Time Record Last --> WorldState
(define (new-game time record last)
  (make-world
   INITIAL-PLAYER
   (list
    (create-bullet (/ WIDTH 2) (- HEIGHT (/ WIDTH 5) (/ WIDTH 10))))
   (list
    (generate-wall time))
   0
   120
   record
   last
   #false)
  )


(big-bang (new-game 120 0 0)
          [to-draw draw-world]
          [on-tick animate-world (/ 1 60)]
          ;[stop-when detect-collision]
          [on-mouse mouse-handler])