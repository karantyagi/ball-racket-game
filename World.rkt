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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; CONSTANTS

;; dimensions of the court

(define COURT-WIDTH 940)
(define COURT-HEIGHT 680)

;;scenes 
(define EMPTY-COURT (empty-scene COURT-WIDTH COURT-HEIGHT "white"))
;(define EMPTY-COURT-AT-PAUSE (empty-scene COURT-WIDTH COURT-HEIGHT "yellow"))

(define FPS 1/20)

(define TOTAL-MISS 10)

;;; Dimensions of the playing window
(define WIDTH 940)  ;;; Window Width
(define HEIGHT 680)  ;;; Window Height

;(define EMPTY-COURT (empty-scene WIDTH HEIGHT "white"))
(define EMPTY-COURT (bitmap "court.jpg"))
;;; Court is rendered as a white rectangle with a black border

(define RESETTING-COURT (empty-scene WIDTH HEIGHT "yellow"))
;;; Court turns from white to yellow for 3 seconds while resetting in which
;;; rally state resets to ready-to-serve state.

(define SPACE " ")

;; starting state of ball and racket
(define START-X-COORD 330)
(define START-Y-COORD 384)
(define START-VX 0)
(define START-VY 0)

;; ball in rally state
(define RALLY-VX 3)
(define RALLY-VY -9)
;; dimensions of the ball
(define BALL-IMAGE (bitmap "ball.png"))
;(define BALL-IMAGE (circle 5 "solid" "black"))


;; dimensions of the racket
(define RACKET-IMAGE (rectangle 50 8 "solid" "brown"))
(define HALF-LENGTH 47/2)

(define MOUSE-POINTER (circle 3 "solid" "blue"))

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
;;; state   : State       represent the state 
;;; time    : NonNegInt  is the timer of the game - units? fps....
;;; miss    : NonNegInt   : Score as per scoring criterion

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
  (big-bang (initial-world FPS)          
            (on-tick world-after-tick FPS)
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
   (cons (make-ball START-X-COORD START-Y-COORD START-VX  START-VY) empty)
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
    [(string=? (world-state w) "play") (world-in-paused-state w)]
    [(string=? (world-state w) "ready") (world-in-play-state w)]
    [(string=? (world-state w) "pause") (world-in-resume-state w)]))


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
       "play"
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


(define (world-in-resume-state w)
  (make-world
  (world-balls w)
   (world-racket w)
   "play"
   (world-time w)
   (world-miss w)))


(define (world-at-game-over w)
  (make-world
   (world-balls w)
   (world-racket w)
   "pause"
   (world-time w)
   TOTAL-MISS))

(define (world-in-play-state w)
    (make-world
   (ball-rally-state (world-balls w))
   (world-racket w)
   "play"
    1 0
   ))

(define (ball-rally-state bl)
  (map
   (lambda (b) (make-ball (ball-x b) (ball-y b) RALLY-VX RALLY-VY))
   bl))




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
    [(string=? (world-state w) "pause") w]
    [(string=? (world-state w) "ready") w]
    [(or (racket-collide-frontwall? (world-racket w)) (= (world-miss w) TOTAL-MISS))   
        (world-at-game-over w)]
    [ else
      (world-during-play-state w)]))



(define (world-during-play-state w)
  (make-world
   (balls-after-tick (world-balls w) w)
   (racket-after-tick w)
   "play"
   (+ (world-time w) 1)
   (update-miss w)
   ))



(define (update-miss w)
     (local (; Ball->Boolean
           ;GIVEN: ball b in world
           ;RETURN: true iff given ball has not hit backwall 
           (define (ball-backwall-collide? b)
             (< 680 (+ (ball-y b) (ball-vy b)))))
    (+ (world-miss w)(length (filter ball-backwall-collide? (world-balls w))))))

;; world-to-scene : World -> Scene
;; RETURNS: a Scene that portrays the given world.
;;EXAMPLES:
;;(world-to-scene (initial-world 1/24)) ->
;      (scene-with-ball
;          (make-ball 330 384 0 0)
;           (scene-with-racket
;              (make-racket 330 384 0 0 0 0 #f)
;                   EMPTY-COURT))
;; STRATEGY: combine simpler functions
(define (world-to-scene w)
  (cond
    [(string=? (world-state w) "ready")
     (place-image
      (text "START GAME" 50 "red") 500 500
      (scene-with-balls-list
       (world-balls w)
       (scene-with-mouse
        (world-racket w)
        (scene-with-racket
         (world-racket w)
         (court-scene w)))))]
    
    [(string=? (world-state w) "play")
     (scene-with-balls-list
      (world-balls w)
      (scene-with-mouse
       (world-racket w)
       (scene-with-racket
        (world-racket w)
        (court-scene w))))]
    
    [(and (string=? (world-state w) "pause") (>= (world-miss w) TOTAL-MISS))
 (place-image
      (text "GAME OVER  \n SCORE:"  50 "red") 500 500
      (scene-with-balls-list
       (world-balls w)
       (scene-with-mouse
        (world-racket w)
        (scene-with-racket
         (world-racket w)
         (court-scene w)))))]
    
    [(string=? (world-state w) "pause")
     (place-image
      (text "game paused" 30 "red") 500 500
      (scene-with-balls-list
       (world-balls w)
       (scene-with-mouse
        (world-racket w)
        (scene-with-racket
         (world-racket w)
         (court-scene w)))))]
    ))
    


    ;;scene-with-mouse : Racket Scene -> Scene
    ;GIVEN: racket r and scene s
    ;;RETURN: mouse pointer in the given scene s
    ;;EXAMPLES:
    ;; (scene-with-mouse
    ;; (make-racket 330 384 0 0 2 2 #t)
    ;;  (world-to-scene (initial-world 1/24))) =>
    ;; (place-image
    ;;    MOUSE-POINTER
    ;;    2
    ;;    2
    ;;    (world-to-scene (initial-world 1/24)))
    ;;DESIGN STRATEGY: using in built function place-image
    ;;and using observer template

    (define (scene-with-mouse r s)
      (if (racket-selected? r)
          (place-image
           MOUSE-POINTER
           (racket-mx r) (racket-my r)
           s)
          s))


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; scene-with-balls-list: BallList Scene -> Scene
    ;; GIVEN: a scene and BallList bl
    ;;RETURNS: a scene like the given one , but with the list of ball in it 
    ;; EXAMPLES:
    ;;(scene-with-balls-list
    ;;    (list(make-ball 330 384 0 0))
    ;;   (scene-with-mouse
    ;;    (make-racket 330 384 0 0 0 0 #f)
    ;;     (scene-with-racket
    ;;      (make-racket 330 384 0 0 0 0 #f)
    ;;         EMPTY-COURT)))
    ;; => (place-image
    ;;    BALL-IMAGE
    ;    330
    ;    384
    ;   (scene-with-mouse
    ;     (make-racket 330 384 0 0 0 0 #f)
    ;     (scene-with-racket
    ;      (make-racket 330 384 0 0 0 0 #f)
    ;      EMPTY-COURT)))
    ;; DESIGN STRATEGY: usinh HOF Foldr on bl

    ;(define (scene-with-balls-list bl s)
    ;  (cond
    ;    [(empty? bl) s]
    ;    [else
    ;     (scene-with-balls-list
    ;      (rest bl)
    ;      (scene-with-ball (first bl) s))]))

    (define (scene-with-balls-list bl s)
      (foldr scene-with-ball s bl))


    ;;scene-with-ball : Ball Scene -> Scene
    ;; GIVEN: a scene and a ball
    ;;RETURNS: a scene like the given one , but with the ball in it
    ;;EXAMPLES :
    ;;(scene-with-ball  (make-ball 330 384 0 0) 
    ;;                     (world-to-scene (initial-world 1/24)) )
    ;; -> (place-image
    ;;       BALL-IMAGE  330 384 (world-to-scene (initial-world 1/24)))

    ;; STRATEGY: using in built function place-image and using observer template
    (define (scene-with-ball b s)
      (place-image
       BALL-IMAGE
       (ball-x b) (ball-y b)
       s)) 

    ;;scene-with-racket : Racket Scene -> Scene
    ;; GIVEN:  a racket and a scene
    ;;RETURNS: a scene like the given one, but with the racket in it
    ;EXAMPLES:
    ;;(scene-with-racket (make-racket 330 384 0 0 0 0 #f)
    ;;                      (world-to-scene (initial-world 1/24)) )
    ;; -> (place-image
    ;;    RACKET-IMAGE 330 384 (world-to-scene (initial-world 1/24)))

    ;; STRATEGY: using in built function place-image and using observer template
    (define (scene-with-racket r s)
      (place-image
       RACKET-IMAGE
       (racket-x r) (racket-y r)
       s))



    ;;court-scene:World->Scene
    ;;GIVEN: a world in some state
    ;;RETURN : a scene for either paused state or unpaused state
    ;EXAMPLES:(court-scene (initial-world 1/24)) ->EMPTY-COURT
    ;;STRATEGY: using case for world w on world-paused?
    (define (court-scene w)
      (if(string=? (world-state w) "pause") RESETTING-COURT  EMPTY-COURT))


(define (balls-after-tick bl w)
     (cond
       [(and (> (world-time w) 0) (= 0 (modulo (world-time w) (* 40 2))))
          (cons (make-ball (+ 300 (random 50)) (+ 300 (random 50))  7 -9)
                 (map
   ;Ball-> Ball
   ;GIVEN: ball b
   ;RETURN: updated ball after tick 
   (lambda (b) (ball-after-tick b w))
   (local (; Ball->Boolean
           ;GIVEN: ball b in world
           ;RETURN: true iff given ball has not hit backwall 
           (define (ball-backwall-collision? b)
             (not(< 680 (+ (ball-y b) (ball-vy b))))))
     (filter ball-backwall-collision? bl))))]

       [else
        (map
   ;Ball-> Ball
   ;GIVEN: ball b
   ;RETURN: updated ball after tick 
   (lambda (b) (ball-after-tick b w))
   (local (; Ball->Boolean
           ;GIVEN: ball b in world
           ;RETURN: true iff given ball has not hit backwall 
           (define (ball-backwall-collision? b)
             (not(< 680 (+ (ball-y b) (ball-vy b))))))
     (filter ball-backwall-collision? bl)))]))



(define (ball-after-tick b w)
  ;(ball-next-move b))
  (cond
    [(ball-collide-wall? b) (after-ball-collide-wall b)]
    [(ball-collide-racket? b (world-racket w))
     (ball-after-colliding-racket b (world-racket w))]
    [else (ball-next-move b)]))

(define (racket-after-tick w)
     (racket-next-move (world-racket w)))




(define (racket-next-move r)
  (make-racket
   (+ (racket-x r) (racket-vx r))
   (+ (racket-y r) (racket-vy r))
   (racket-vx r)
   (racket-vy r)
   (racket-mx r)
   (racket-my r)
   (racket-selected? r)))

    