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
(require "Balls.rkt")
(require "Racket.rkt")

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

(define SPACE " ")

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
;; -- "pause" 


;; CONSTRUCTOR TEMPLATE: Not needed.

;; OBSERVER TEMPLATE:
;; state-fn : State-> ?
#;(define (state-fn s)
  (cond
    [(string=? s "ready") ...]
    [(string=? s "play")  ...]
    [(string=? s "reset")   ...]))


;;;;;;;;;;;;;;; Function ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (simulation speed)
  (big-bang (initial-world speed)          
            (on-tick world-after-tick speed)
            (on-draw world-to-scene)
            (on-key world-after-key-event)
            (on-mouse world-after-mouse-event)))

;;; initial-world : PosReal -> World
;;; GIVEN:   the speed of the simulation, in seconds per tick
;;;          (so larger numbers run slower)
;;; RETURNS: the ready-to-serve state of the world
;;; EXAMPLES:  (initial-world 0.5)
;;;            (initial-world 1/24)
;;; DESIGN STRATEGY: Use constructor template on World
(define (initial-world speed)
  (make-world
   (cons
    (make-ball
     INITIAL-BALL-X INITIAL-BALL-Y
     INITIAL-BALL-VX INITIAL-BALL-VY) empty)
     (make-racket INITIAL-RACKET-X
                INITIAL-RACKET-Y
                INITIAL-RACKET-VX
                INITIAL-RACKET-VY
                0 0
                #false)
      "ready"                            
       0 0))


; world-after-key-event : World KeyEvent -> World
; GIVEN: a world w and a key event kev
; RETURNS: the world that should follow the given world
;     after the given key event
;;EXAMPLES :
;; (world-paused? (world-after-key-event (initial-world 1/24) " ")) -->  #f
; DESIGN STRATEGY : divide into cases on kev and w
(define (world-after-key-event w kev)
  (cond
    [(key=? kev SPACE) (world-state-after-space w)]
;    [(and (key=? kev B-KEY) (world-rally-state? w))
;     (add-balls-in-world w)]
    [else w]))


;;;;world-state-after-space : World -> World
;;GIVEN:  a world w 
;;RETURN: either a world in rally state if it was in ready-to-serve
;; or world in paused state if it was in paused-state
;;EXAMPLES:
;; (check-equal?
;;   ( world-paused? ((world-state-after-space
;;                      (world-in-rally-state
;;                                (initial-world 1/24))))
;;                   -> #t

;;DESIGN STRATEGY: divide into cases on world w state 
(define (world-state-after-space w)
  (cond
    [(string=? (world-state w) "play") (world-in-pause-state w)]
    [(string=? (world-state w) "ready") (world-in-start-state w)]
    [(string=? (world-state w) "pause") (world-in-play-state w)]))


;; world-after-mouse-event: World Int Int MouseEvent -> World
; GIVEN: a world, the x and y coordinates of a mouse event,
;     and the mouse event
; RETURNS: the world that should follow the given world after
;     the given mouse event

;; EXAMPLES:
;; (world-ready-to-serve?
;;    (world-after-mouse-event
;;       (initial-world 1/24) 330 380 "button-down"))
;;  ->#t

;;DESIGN STRATEGY: cases on world(w) state 

(define (world-after-mouse-event w mx my mev)
  (if (string=? (world-state w) "play")
      (make-world
       (world-balls w)
       (racket-after-mouse-event (world-racket w) mx my mev)
       (world-time w)
       (world-miss w))
      w))

;; world-in-paused-state : World -> World
;; GIVEN :  a world w in rally state
;; RETURNS: a world just like the given one, but with paused? toggled
;; DESIGN STRATEGY: use constructor template for World on w
;; EXAMPLES:
;; (world-paused? (world-in-paused-state (initial-world 1/24)))
;; -----> #t

(define (world-in-paused-state w)
  (make-world
   (world-balls w)
   (world-racket w)
   "pause"
   (world-time w)
   (world-miss w)))


;; world-after-tick : World -> World
;; GIVEN: a world  w
;; RETURNS: the world that should follow w after a tick.  If the world
;;   is paused, returns it unchanged.  Otherwise, builds a new world
;;   with updated ball and racket.
;; EXAMPLES:
;; (world-paused?(world-after-tick (initial-world 1/24))) -> #f

;; STRATEGY: Cases on world w, then use constructor template of World 
(define (world-after-tick w)
  (cond
    [ ((string=? (world-state w) "pause")) w]
    [(empty? (world-balls w))
     (world-in-paused-state w)]
    [(racket-collide-frontwall? (world-racket w))
     (world-at-game-over w)]
    [ else
      (world-during-play-state w)]))




