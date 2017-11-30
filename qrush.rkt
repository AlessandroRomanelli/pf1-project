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

