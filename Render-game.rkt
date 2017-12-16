;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Render-game) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt") 
(require 2htdp/universe)
(require 2htdp/image)
(require "Balls.rkt")
(require "Racket.rkt")
(require "update-racket.rkt")
(require "World.rkt")


;;;; RENDER world

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; FUNCTION DEFINITION FOR WORLD-TO-SCENE FUNCTION



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
                (make-mouse 0 0) ;;;; assigning values to mouse coordinates
                ;;;; arbitarily at the beginning
                #false)          ;;;; Racket cannot be selected in this state 
   "ready"                            
   0 0 0 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; WORLD-AFTER-TICK FUNCTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; world-after-tick : World -> World
;;; GIVEN: any world that's possible for the simulation
;;; RETURNS: the world that should follow the given world
;;;          after a tick
;;; EXAMPLE:
;;;(world-after-tick (make-world
;;;                   (cons (make-ball 40 40 -1 -1)empty)
;;;                   (make-racket 300 300 0 0 (make-mouse 0 0) #true)
;;;                   #false   0.5   0)) =>
;;;                       (make-world
;;;                         (cons (make-ball 39 39 -1 -1)empty)
;;;                         (make-racket 300 300 0 0 (make-mouse 0 0) #true)
;;;                          #false   0.5   0)

;;; DESIGN STRATEGY: Calling simpler functions
(define (world-after-tick w)
  (update-world w))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UPDATE WORLD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; update-world : World -> World
;;; GIVEN: a world
;;; RETURNS: the world that should follow the given world
;;;          after a tick
;;; EXAMPLES:
;;;       (update-world world5-in-rally-state)
;;;                                  => world5-after-ball-left-wall-collision
;;;       (update-world world8-in-rally-state)
;;;                                  => world8-after-racket-left-wall-collision
;;;
;;; STRATEGY: Use constructor template on World, calling simpler functions

(define (update-world w)
  (make-world 
   (update-balls-after-tick (world-balls w))
   (update-racket-after-tick (world-racket w))
   (world-state w)
   (world-time)
   (world-playtime-ticker w)
   (world-resetting-ticker w) (world-score)))

;;;TESTS:

;;; update-balls-after-tick : BallList -> BallList
;;; GIVEN:   a Balllist
;;; RETURNS: the BallList with updated balls after a tick
;;; EXAMPLES:
;;;( update-balls-after-tick
;;;      (list (make-ball 222 205 0 10) (make-ball 100 20 9 -5))) =>
;;;                 (list (make-ball 222 215 0 10) (make-ball 109 15 9 -5)))
;;;
;;; DESIGN STRATEGY: Using HOF map on blist

#;(define (update-balls-after-tick blist)
    (cond
      [(empty? blist) empty]
      [else
       (cons (update-ball (first blist))
             (update-balls-after-tick (rest blist)))]))



(define (update-balls-after-tick blist)
  (map
   (lambda (n) (update-ball n)) blist)) 


;;; update-ball : Ball -> Ball
;;; GIVEN: a ball
;;; RETURNS: the ball with its updated position and velocity after a tick
;;; EXAMPLES:
;;; (update-ball (make-ball 222 205 0 10)) => (make-ball 222 215 0 10)
;;; (update-ball (make-ball 421 200 10 -2)) => (make-ball 419 198 -10 -2)
;;;
;;; DESIGN STRATEGY: Cases on Ball

(define (update-ball b)
  (make-ball
   (+(ball-x b) (ball-vx b))(+(ball-y b) (ball-vy b))
   (ball-vx b) (ball-vy b)))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; DEFINITION FOR WORLD-AFTER-MOUSE-EVENT FUNCTION:

;;; world-after-mouse-event : World Int Int MouseEvent -> World
;;; GIVEN:   a world, the x and y coordinates of a mouse event,
;;;          and the mouse event button-down, drag or button-up
;;; RETURNS: the world that should follow the given world after
;;;          the given mouse event.
;;; DESIGN STRATEGY:
;;; calling simpler function

(define (world-after-mouse-event w mx my mev)
  (if(string=? (world-state w) "play")
     (make-world
      (world-balls w)
      (racket-after-mouse-event
       (world-racket w)
       mx
       my
       mev)
      (world-state w)
      (world-time w) (world-playtime-ticker w)
      (world-resetting-ticker w) (world-score))
     w))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HELPER FUNCTIONS FOR WORLD-AFTER-MOUSE-EVENT FUNCTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; racket-after-mouse-event : Racket Int Int MouseEvent -> Racket
;;; GIVEN: a racket, the x and y coordinates of a mouse event,
;;;        and the mouse event button-down, drag or button-up
;;; RETURNS: the racket as it should be after the given mouse event
;;; DESIGN STRATEGY:
;;; Cases on MouseEvent
(define (racket-after-mouse-event r mx my mev)
  (cond
    [(mouse=? mev "button-down")
     (if (grab-racket? r mx my)
         (make-racket
          (racket-x r)
          (racket-y r)
          (racket-vx r)
          (racket-vy r)
          (make-mouse mx my)
          #true)
         r)]
    [(mouse=? mev "button-up")
     (make-racket
      (racket-x r)
      (racket-y r)
      (racket-vx r)
      (racket-vy r)
      (racket-mouse r)
      #false)]
    [(mouse=? mev "drag")
     (if (racket-selected? r)
         (racket-after-drag r mx my)
         r)]
    [else r]))

;;; TESTS:

;;; HELPER FUNCTION FOR RACKET-AFTER-MOUSE-EVENT:

;;; grab-racket? : Racket Int Int -> Boolean
;;; GIVEN: a racket and the x and y coordinates of the button-down mouse event
;;; RETURNS: true iff coordinates at button-down are within a distance of
;;; 25 pixels from the current position of the racket.
;;; EXPLINATION:
;;; racket is selected if mouse coordinates at button-down are within a
;;; distance of 25 pixels from the current position of the racket
;;; EXAMPLES:
;;; (grab-racket?
;;;      (make-racket 202 206 0 0 (make-mouse 0 0) #false) mx my) => false
;;; DESIGN STRATEGY:
;;; Transcribe formula
(define (grab-racket? r mx my)
  (<=
   (expt (+
          (expt (- (racket-x r) mx) 2)
          (expt (- (racket-y r) my) 2))
         1/2)
   MOUSE-RACKET-DISTANCE))

;;; TESTS:


;;; racket-after-drag : Racket Int Int -> Racket
;;; GIVEN:   a racket, the x and y coordinates of drag event     
;;; RETURNS: the racket as it should be after the given mouse event
;;; EXAMPLE:
;;;(racket-after-drag (make-racket 202 206 0 0 (make-mouse 0 0) #false) 300 300)
;;;  => (make-racket 502 506 0 0 (make-mouse 300 300) #false)
;;;
;;; DESIGN STRATEGY:
;;; using constructor template for Racket
(define (racket-after-drag r mx my)
  (make-racket
   (+(racket-x r)
     (- mx (mouse-x (racket-mouse r))))
   (+(racket-y r)
     (- my (mouse-y (racket-mouse r))))
   (racket-vx r)
   (racket-vy r)
   (make-mouse mx my)
   (racket-selected? r)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; FUNCTION DEFINITION FOR WORLD-TO-SCENE FUNCTION

;;; world-to-scene : World -> Scene
;;; RETURNS: a Scene that portrays the given world.
;;; DESIGN STRATEGY:
;;; Place the ball and racket on the court at their right position.

(define (world-to-scene w)
  (if(string=? (world-state w) "ready")
     ;;;; Court background is yellow during reset state
     (add-ball-to-scene (world-balls w) (racket-in-empty-court-scene w))
     (if
      (racket-selected? (world-racket w)) ;;;; show mouse location as a
      ;;;; circle iff racket is selected
      (place-image
       (circle MOUSE-RADIUS "solid" MOUSE-COLOR)
       (mouse-x(racket-mouse (world-racket w)))
       (mouse-y(racket-mouse (world-racket w)))
       (add-ball-to-scene (world-balls w) (racket-in-empty-court-scene w)))  
      ;;;; otherwise don't show mouse location
      
      (add-ball-to-scene (world-balls w) (reset-scene w)))))

;;;reset-scene : World -> Scene
;;; GIVEN: a world
;;; RETURNS: the scence with racket on the yellow court
;;; DESIGN STRATEGY:
;;; Place the racket on a yellow court at the right position
(define (reset-scene w)
  (place-image
   (rectangle RACKET-WIDTH RACKET-HEIGHT "solid"  RACKET-COLOR)
   (racket-x (world-racket w))
   (racket-y (world-racket w))
   RESETTING-COURT))


;;; racket-in-empty-court-scene : World -> Scene
;;; GIVEN: a world
;;; RETURNS: the scence with racket on the court
;;; DESIGN STRATEGY:
;;; Place the racket on an empty canvas at the right position
(define (racket-in-empty-court-scene w)
  (place-image
   (rectangle RACKET-WIDTH RACKET-HEIGHT "solid"  RACKET-COLOR)
   (racket-x (world-racket w))
   (racket-y (world-racket w))
   EMPTY-COURT))

;;; add-ball-to-scene : BallList -> Scene 
;;; GIVEN: a Balllist
;;; RETURNS: a scene  with all balls from the ballList, scene added onto the
;;;          given scene
;;; EXAMPLE:
;;; (add-ball-to-scene (cons make-ball 222 205 0 10) empty) (place-image
;;;            (rectangle RACKET-WIDTH RACKET-HEIGHT "solid"  RACKET-COLOR)
;;;            (racket-x (world-racket w))
;;;            (racket-y (world-racket w))
;;;            EMPTY-COURT))
;;; DESIGN STRATEGY: Place the balls on the given scene at the right position.
(define (add-ball-to-scene blst scene)
  (cond
    [(empty? blst) scene ]
    [else (place-image
           (circle BALL-RADIUS "solid"  BALL-COLOR)
           (ball-x (first blst)) (ball-y (first blst)) 
           (add-ball-to-scene (rest blst) scene))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(define (simulation speed)
;  (big-bang (initial-world speed)          
;            (on-tick world-after-tick speed)
;            (on-draw world-to-scene)
;            (on-key world-after-key-event)
;            (on-mouse world-after-mouse-event)))

(define (runSimulation speed)
  (big-bang (initial-world FPS)
            (on-tick world-after-tick speed)
            (on-draw world-to-scene)))


;;; Implementing the simulation:
(runSimulation 15);


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; END FUNCTION DEFINITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
