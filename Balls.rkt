;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Balls) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;; BALLS - DATA DEFINITIONS

(require rackunit)
(require "extras.rkt") 
(require 2htdp/universe)
(require 2htdp/image)

(provide
 BALL-COLOR
 BALL-RADIUS
 INITIAL-BALL-X
 INITIAL-BALL-Y
 INITIAL-BALL-VX
 INITIAL-BALL-VY
 make-ball
 ball?
 ball-x
 ball-y
 ball-vx
 ball-vy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; CONSTANTS

(define BALL-COLOR "black")
(define BALL-RADIUS 6)
;;; Ball is rendered as a circle of color: BALL-COLOR
;;; and radius: BALL-RADIUS

(define INITIAL-BALL-X 330)
(define INITIAL-BALL-Y 384)
;;; starting position of the ball at the start of the game,
;;; that is in ready-to-serve state
 
(define INITIAL-BALL-VX 20)
(define INITIAL-BALL-VY 20)
;;; initial velocity of ball in ready-to-serve state

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DATA DEFINITION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Ball
;;; It is rendered as a circle in the simulation. 
;;;
;;; REPRESENTATION:
;;;
;;; A Ball is represented as a struct (make-ball x y vx vy)
;;; with the following fields:
;;; x :  Int    x-coordinate of the center of the ball, in pixels,
;;;                relative to the origin of the scene
;;; y :  Int    y-coordinate of the center of the ball, in pixels,
;;;                relative to the origin of the scene
;;; vx : Int    x component of the ball's velocity, in pixels/tick
;;; vy : Int    y component of the ball's velocity, in pixels/tick
;;; life : NonNegInt  time for which ball is alive
;;;
;;; EXPLAINATION:
;;;
;;; We use graphics coordinates, where top left corner denotes the origin,
;;; x increases towards right, and y increases as we go down.
;;; A positive value for vy means the ball or racket is moving downward.

;;; IMPLEMENTATION OF BALL
(define-struct ball (x y vx vy))

;;; CONSTRUCTOR TEMPLATE
;;; (make-ball Integer Integer Integer Integer NonNegInt)

;;; OBSERVER TEMPLATE
;;; ball-fn : Ball -> ??
(define (ball-fn b)
  (... (ball-x b)
       (ball-y b)
       (ball-vx b)
       (ball-vy b)))

;;; A BallList is represented as a list of Ball
;;;
;;; CONSTRUCTOR TEMPLATES:
;;; empty
;;; (cons b bl)
;;; -- WHERE
;;;    b  is a Ball
;;;    bl is a BallList
;;;
;;; OBSERVER TEMPLATE:
;;; ball-list-fn : BallList -> ??
;;;(define (ball-list-fn list)
;;;  (cond
;;;    [(empty? list) ...]
;;;    [else (...
;;;            (first list)
;;;	    (ball-list-fn (rest list)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EXAMPLES OF WORLDS FOR TESTING:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;