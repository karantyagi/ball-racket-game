;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Racket) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "extras.rkt") 
(require 2htdp/image)

(provide 
 MOUSE-COLOR
 MOUSE-RADIUS
 MOUSE-RACKET-DISTANCE
 RACKET-COLOR
 RACKET-WIDTH
  RACKET-HEIGHT
 HALF-RACKET-WIDTH
 INITIAL-RACKET-X
 INITIAL-RACKET-VX
 INITIAL-RACKET-Y
 INITIAL-RACKET-VY
 make-racket
 racket?
 racket-x
 racket-y
 racket-vx
 racket-vy
 racket-mouse
 racket-selected?
 make-mouse
 mouse?
 mouse-x
 mouse-y)

;;; CONSTANTS

(define MOUSE-COLOR "blue")
(define MOUSE-RADIUS 4)
;;; Mouse is rendered as a circle drawn at current mouse co-ordinates,
;;; iff Racket is selected.
;;; color of circle:  MOUSE-COLOR
;;; radius of circle: MOUSE-RADIUS

(define MOUSE-RACKET-DISTANCE 25)
;;; The maximum distance of mouse from rectangle, to grab and select
;;; the rectangle.

(define RACKET-COLOR "Green")
(define RACKET-WIDTH 70)
(define HALF-RACKET-WIDTH (/ RACKET-WIDTH 2))
(define RACKET-HEIGHT 12)
;;; Racket is rendered as a rectangle of color: RACKET-COLOR
;;; with width: RACKET-WIDTH and height: RACKET-HEIGHT.

(define INITIAL-RACKET-X 300)
(define INITIAL-RACKET-Y 500)
;;; starting position of the racket at the start of the game,
;;; that is in ready-to-serve state
  
(define INITIAL-RACKET-VX 5)
(define INITIAL-RACKET-VY 10)
;;; initial velocity of racket in ready-to-serve state 

;;; Racket
;;; It is rendered as a rectangle in the simulation. 
;;;
;;; REPRESENTATION:
;;;
;;; A Racket is represented as a struct (make-racket x y vx vy mouse selected?)
;;; with the following fields:
;;; x  : Inte  x-coordinate of the center of the racket, in pixels, 
;;;            relative to the origin of the scene, assuming the racket
;;;            to be a straight line of a fixed length
;;; y  : Int   y-coordinate of the center of the racket, in pixels,
;;;            relative to the origin of the scene
;;; vx : Int   x component of the racket's velocity, in pixels/tick
;;; vy : Int   y component of the racket's velocity, in pixels/tick
;;; mouse : Mouse  describes the coordinates of mouse on grabbing and 
;;;                selecting the racket for smooth dragging
;;; selected? : Boolean   true iff the racket is selected

;;; IMPLEMENTATION:
(define-struct racket (x y vx vy mouse selected?))

;;; CONSTRUCTOR TEMPLATE
;;; (make-racket Integer Integer Integer Integer Mouse Boolean)

;;; OBSERVER TEMPLATE
;;; racket-fn : Racket -> ??
(define (racket-fn r)
  (... (racket-x r)
       (racket-y r)
       (racket-vx r)
       (racket-vy r)
       (racket-mouse r)
       (racket-selected?)))


;;; Mouse
;;; It is rendered as a blue circle in the simulation, iff racket is selected. 
;;;
;;; REPRESENTATION:
;;;
;;; A Mouse is represented as a struct (make-mouse x y)
;;; with the following fields:
;;; x : Integer    x-coordinate of the mouse, in pixels, 
;;;                relative to the origin of the scene, assuming the racket
;;;                to be a straight line of a fixed length
;;; y : Integer    y-coordinate mouse, relative to the origin of the scene

;;; IMPLEMENTATION:
(define-struct mouse (x y))

;;; CONSTRUCTOR TEMPLATE
;;; (make-mouse Integer Integer)

;;; OBSERVER TEMPLATE
;;; mouse-fn : Mouse -> ??
(define (mouse-fn m)
  (... (mouse-x m)
       (mouse-y m)))
