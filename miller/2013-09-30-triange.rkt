;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 2013-09-30-triange) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require rackunit/text-ui)
(require 2htdp/universe)
(require 2htdp/image)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; MAIN FUNCTION.

;; main : figure -> World
;; GIVEN: the figure to follow
;; EFFECT: runs the simulation, with a dot walking around the figure
;; RETURNS: the final state of the world
;;
;; Run with (main INITIAL-FIGURE)
(define (main fig)
  (big-bang (initial-world fig)
            (on-tick world-after-tick 0.5 (* 6 (+ SPEED VERTEX-PAUSE)))
            (on-draw world-to-scene)))

(define HEIGHT 400)
(define WIDTH 200)
(define BORDER 10)
(define TOP (make-posn (/ WIDTH 2) BORDER))
(define BOTTOM-LEFT (make-posn BORDER (- HEIGHT BORDER)))
(define BOTTOM-RIGHT (make-posn (- WIDTH BORDER) (- HEIGHT BORDER)))
(define SPEED 8) ; In clock ticks per side
(define VERTEX-PAUSE 4) ; In clock clicks
(define BACKGROUND-FIGURE-COLOR "yellow")
(define MOVING (circle 3 "outline" "red"))
(define BIGGEST 8)
(define (waiting clicks-to-go)
  (star (- BIGGEST clicks-to-go) "solid" "blue"))

(define (add-points-line scene from to color)
  (add-line scene (posn-x from) (posn-y from) (posn-x to) (posn-y to) color))

(define (make-background color) 
  (add-points-line
   (add-points-line
    (add-points-line
     (empty-scene WIDTH HEIGHT)
     TOP BOTTOM-LEFT color)
    BOTTOM-LEFT BOTTOM-RIGHT color)
   BOTTOM-RIGHT TOP color))

(define-struct figure (one two three))
;; A figure is (make-figure posn posn posn)
;; Interp: A triangle with the three posns as the three corners

(define INITIAL-FIGURE (make-figure TOP BOTTOM-LEFT BOTTOM-RIGHT))

;; rotate-figure: figure -> figure
;; GIVEN: A figure
;; RETURNS: The same figure, but with the vertices rotated so the second becomes
;; the first, the third becomes the second, and the first becomes the third
;; Examples:
;; (rotate-figure INITIAL-FIGURE) =
;;   (make-figure BOTTOM-LEFT BOTTOM-RIGHT TOP)
(define (rotate-figure fig)
  (make-figure (figure-two fig) (figure-three fig) (figure-one fig)))
;; Tests
(check-equal? (rotate-figure INITIAL-FIGURE)
              (make-figure BOTTOM-LEFT BOTTOM-RIGHT TOP)
              "Rotated INITIAL-FIGURE")

;; Four helper methods used below.  They are both function composition and
;; structural decomposition, but are so trivial that I decided to cheat and
;; consider them part of the data definition of a figure.

(define (figure-first-x fig)
  (posn-x (figure-one fig)))

(define (figure-first-y fig)
  (posn-y (figure-one fig)))

(define (figure-second-x fig)
  (posn-x (figure-two fig)))

(define (figure-second-y fig)
  (posn-y (figure-two fig)))

;; Some domain knowledge: what's the correct deltax and deltay per step
;; to allow a point to move from one end of a line segment to the other
;; in SPEED steps

(define (step-x fig)
  (step (figure-first-x fig)
        (figure-second-x fig)))

(define (step-y fig)
  (step (figure-first-y fig)
        (figure-second-y fig)))

(define (step from to)
  (/ (- to from) SPEED))

(define-struct world (figure x y waiting?))
;; A world is (make-world figure Number Number Boolean)
;; Interp: drawing the given figure, current location at (x, y) on
;; the first line in the figure, and waiting? true iff we're waiting 
;; at a vertex

;; initial-world: figure -> world
;; GIVEN: a figure
;; RETURNS: a world that will draw the figure
;; Note: I've chosen to start at the first vertex, paused until the
;; vertex is displayed in all its glory
;; Examples omitted
;; Strategy: function composition
(define (initial-world fig)
  (make-world fig
              (figure-first-x fig) 
              (figure-first-y fig)
              true))

;; world-to-scene: world -> scene
;; GIVEN: a world
;; RETURNS: a scene to display that world
;; Examples omitted
;; Strategy: structural decomposition on world with simple function composition
(define (world-to-scene world)
  (place-image 
   (if (world-waiting? world) 
       (waiting (world-clicks-to-go world))
       MOVING)
   (world-x world)
   (world-y world)
   (make-background "yellow")))

;; world-after-tick: world -> world
;; GIVEN: a world
;; RETURNS: the world after one clock tick
;; Examples omitted
;; Strategy: structural decomposition on world
(define (world-after-tick world)
  (cond
    [(world-waiting? world) (wait world)]
    [else (move world)]))

;; wait: world -> world
;; GIVEN: a world that is waiting at a vertex
;; RETURNS: the world after one tick
;; Examples omitted
;; Strategy: structural decomposition on world
(define (wait world)
  world)       ; Fix this

;; move: world -> world
;; GIVEN: a world that is moving along a line segment
;; RETURNS: the world after one tick
;; Examples omitted
;; Strategy: structural decomposition on world
(define (move world)
  world)        ; Fix this


