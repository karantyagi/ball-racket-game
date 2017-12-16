;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname update-racket) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt") 
(require 2htdp/universe)
(require 2htdp/image)
(require "Balls.rkt")
(require "Racket.rkt")
(require "World.rkt")


(provide
 update-racket-after-tick
 update-racket)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UPDATE RACKET AFTER TICK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; update-racket-after-tick : Racket -> Racket
;;; GIVEN: a racket
;;; RETURNS: the racket with its updated position and velocity after a tick
;;; EXAMPLES:
;;; (update-racket-after-tick (make-racket 188 206 1 1 (make-mouse 0 0) #false))
;;;           => (make-racket 189 207 1 1 (make-mouse 0 0) #false)
;;; (update-racket-after-tick
;;; (make-racket 422 300 -10 0(make-mouse 0 0) #false))
;;;           => (make-racket 401 300 -10 0 (make-mouse 0 0) #false)
;;; (update-racket-after-tick
;;; (make-racket 3 300 10 0(make-mouse 0 0) #false))
;;;           => (make-racket 24 300 10 0 (make-mouse 0 0) #false)
;;;                          
;;; DESIGN STRATEGY: Cases on racket-x, Calling simpler functions

(define (update-racket-after-tick r)
  (if (racket-selected? r) r
      (update-racket r)))

;;; update-racket : Racket -> Racket
;;; GIVEN: a racket
;;; RETURNS: the racket at its new position as per its current velocity
;;; EXAMPLES:
;;; (update-racket-after-tick (make-racket 200 200 1 1 (make-mouse 0 0) #false))
;;;           => (make-racket 201 201 1 1 (make-mouse 0 0) #false)
;;; (update-racket-after-tick
;;;       (make-racket 400 300 -10 0(make-mouse 0 0) #false))
;;;           => (make-racket 390 300 -10 0 (make-mouse 0 0) #false)
;;; (update-racket-after-tick
;;;       (make-racket 300 300 0 0(make-mouse 0 0) #false))
;;;           => (make-racket 300 300 0 0 (make-mouse 0 0) #false)
;;;
;;; DESIGN STRATEGY: Using constructor template on Racket

(define (update-racket r)
  (make-racket
   (+(racket-x r) (racket-vx r))
   (+(racket-y r) (racket-vy r))
   (racket-vx r) (racket-vy r)
   (racket-mouse r)
   (racket-selected? r)))

;;;TESTS:



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; END OF UPDATE RACKET AFTER TICK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;