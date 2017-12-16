;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname World) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;; DOUBTS  - how to put playing window in centre ??


;;;;;;;;

;;; WORLD - DATA DEFINITIONS

(require rackunit)
(require "extras.rkt") 
(require 2htdp/universe)
(require 2htdp/image)

(provide
 TOTAL-MISS
 FPS
 WIDTH
 HEIGHT
 RESET-TIME
 RESETTING-COURT
 EMPTY-COURT
 make-world
 world?
 world-balls
 world-racket
 world-state
 world-time
 world-playtime-ticker
 world-resetting-ticker
 world-score)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; CONSTANTS

(define FPS 30)

(define TOTAL-MISS 10)

;;; Dimensions of the playing window
(define WIDTH 1340)  ;;; Window Width
(define HEIGHT 680)  ;;; Window Height

(define EMPTY-COURT (empty-scene WIDTH HEIGHT "white"))
;;; Court is rendered as a white rectangle with a black border

(define RESETTING-COURT (empty-scene WIDTH HEIGHT "yellow"))
;;; Court turns from white to yellow for 3 seconds while resetting in which
;;; rally state resets to ready-to-serve state.

(define RESET-TIME  2.5)
;;; the number of seconds to complete the reset process in real time

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DATA DEFINITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; World
;;; It is rendered as a scene in the simulation. 

;;; REPRESENTATION:
;;;
;;; A World is represented as a struct
;;; (make-world balls racket state time playtime-ticker resetting-ticker score)
;;; with the following fields:
;;;
;;; balls   : BallList   describes the list of balls in the world 
;;; racket  : Racket     describes the position and veloctiy of the racket
;;; state   : State      is true iff the world is in the serve state 
;;; time    : NonNegInt  is the timer of the game - units? fps....
;;; playtime-ticker : NonNegInt   : playtime in fps
;;; resetting-ticker : NonNegInt   : playtime in fps
;;; Score : NonNegInt   : Score as per scoring criterion

;;; EXPLAINATION:
;;;
;;; The reset process lasts for 3 seconds of real time.
;;;
;;; Starting the reset process starts the tick-counter(intially at 0).
;;; The tick-counter counts upto a total number of ticks equivalent to
;;; 3 seconds of real time.
;;; 


;;; IMPLEMENTATION OF WORLD
(define-struct world
  (balls racket state time miss))

;;; CONSTRUCTOR TEMPLATE:
;;; (make-world BallList Racket Boolean PosInt NonNegInt)

;;; OBSERVER TEMPLATE:
;;; world-fn : World -> ??
(define (world-fn w)
  (... (world-balls w) 
       (world-racket w) 
       (world-state w)
       (world-time w)
       (world-miss w)))


;; a STATE is represented by one of :

;; INTERPRETATION:

;; A State is one of
;; -- "ready"
;; -- "play"
;; -- "reset" 


;; CONSTRUCTOR TEMPLATE: Not needed.

;; OBSERVER TEMPLATE:
;; state-fn : State-> ?
#;(define (state-fn s)
  (cond
    [(string=? s "ready") ...]
    [(string=? s "play")  ...]
    [(string=? s "reset")   ...]))