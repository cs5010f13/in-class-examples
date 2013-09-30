;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 2013-09-30-triangle-final) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
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

;; add-points-line: scene posn posn string -> scene
;; GIVEN: a scene, two positions (defining a line segment), and a color (as a string)
;; RETURNS: a scene with the line segment drawn in the specified color.
;; Examples omitted
;; Strategy: function composition
(define (add-points-line scene from to color)
  (add-line scene (posn-x from) (posn-y from) (posn-x to) (posn-y to) color))

;; make-background: world string -> scene
;; GIVEN: a world and a color (as a string)
;; RETURNS: a scene with the triangle defined by fig shown in the chose color
;; Examples omitted
;; Strategy: structural decomposition on world
(define (make-background world color)
  (make-background-from-figure color (world-figure world)))
  
;; make-background-from-figure: string figure -> scene
;; GIVEN: a color (as a string) and a figure
;; RETURNS: a scene with the triangle defined by fig shown in the chosen color
;; Examples omitted
;; Strategy: structural decomposition on figure
(define (make-background-from-figure color fig)
  (make-background-from-posns color (figure-one fig) (figure-two fig) (figure-three fig)))

;; make-background-from-figure color posn posn posn -> scene
;; GIVEN: a the position of three vertices
;; RETURNS: a scene of the triangle, drawn in the given color
;; Examples omitted
;; Strategy: function composition
(define (make-background-from-posns color pos1 pos2 pos3)
  (add-points-line
   (add-points-line
    (add-points-line (empty-scene WIDTH HEIGHT) pos1 pos2 color)
    pos2 pos3 color)
   pos3 pos1 color))

(define-struct figure (one two three))
;; A figure is (make-figure posn posn posn)
;; Interp: A triangle with the three posns as the three corners
;; Template: figure-fn: fig -> ??
;; (define (figure-fn fig)
;;   (... (figure-one fig) (figure-two fig) (figure-three fig)))

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

;; step-x: posn posn -> Number
;; GIVEN: the start and end points (as posn) of a line segment
;; RETURNS: the amount to step in the x direction in a single clock tick
;; Examples omitted
;; Strategy: function composition
(define (step-x start end)
  (step (posn-x start)
        (posn-x end)))

;; step-y: posn posn -> Number
;; GIVEN: the start and end points (as posn) of a line segment
;; RETURNS: the amount to step in the x direction in a single clock tick
;; Examples omitted
;; Strategy: function composition
(define (step-y start end)
  (step (posn-y start)
        (posn-y end)))

;; step: Number Number -> Number
;; GIVEN: a coordinate (x or y) from the start and end of a line segment
;; RETURNS: the correct delta step for that coordinate in one time click
;; Examples omitted
;; Strategy: domain knowledge
(define (step from to)
  (/ (- to from) SPEED))

(define-struct world (figure x y deltax deltay clicks-to-go waiting?))
;; A world is (make-world figure Number Number Number Number PosNum)
;; Interp: drawing the given figure, current location at (x, y) on
;; the first line in the figure, with each click advancing deltax and deltay pixels,
;; with clicks-to-go more clicks before starting to move or changing line segments,
;; and waiting? true iff we're waiting before starting on the current line segment.
;; Template: world-fn: world -> ??
;; (define (world-fn w)
;;   (... (world-figure w) (world-x w) (world-y w)
;;        (world-deltax w) (world-deltay w) (world-clicks-to-go w) (world-waiting? w)))

;; initial-world: figure -> world
;; GIVEN: a figure
;; RETURNS: a world that will draw the figure
;; Note: I've chosen to start at the first vertex, paused until the
;; vertex is displayed in all its glory
;; Examples omitted
;; Strategy: structural decomposition on world
(define (initial-world fig)
  (make-initial-world fig (figure-one fig) (figure-two fig)))

;; make-initial-world: figure PosNum Boolean posn posn -> world
;; GIVEN: a figure, and the positions of the start and end of the first line segment in the figure
;; RETURNS: an initial world for that figure
;; Strategy: structural decomposition on position
(define (make-initial-world fig first-posn second-posn)
  (make-world-from-x-y-and-posn fig (posn-x first-posn) (posn-y first-posn)
                                first-posn second-posn))
;; make-world-from-x-y-and-posn: Number Number posn posn -> world
;; GIVEN: a figure, an x and y position in that figure, and the positions of the
;;        start and end of the first line segment of the figure
;; RETURNS: an initial world for that figure
(define (make-world-from-x-y-and-posn fig x y first-posn second-posn)
  (make-world fig x y 
              (step-x first-posn second-posn) (step-y first-posn second-posn) 
              VERTEX-PAUSE true))

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
   (make-background world "yellow")))

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
  (if (= (world-clicks-to-go world) 0)
      (start-moving world)
      (decrease-clicks world)))

;; move: world -> world
;; GIVEN: a world that is moving along a line segment
;; RETURNS: the world after one tick
;; Examples omitted
;; Strategy: structural decomposition on world
(define (move world)
  (if (= (world-clicks-to-go world) 0)
      (initial-world (rotate-figure (world-figure world)))
      (one-step world)))

;; decrease-clicks: world -> world
;; GIVEN: a world
;; RETURNS: a world with one less click until the next major event
;; Examples omitted
;; Strategy: structural decomposition on world
(define (decrease-clicks world)
  (make-world (world-figure world)
              (world-x world)
              (world-y world)
              (world-deltax world)
              (world-deltay world)
              (- (world-clicks-to-go world) 1)    ; Notice this changes
              (world-waiting? world)))

;; start-moving: world -> world
;; GIVEN: a world
;; RETURNS: a world that starts down the segment to which it was initialized
;; Examples omitted
;; Strategy: structural decomposition on world
(define (start-moving world)
  (make-world (world-figure world)
              (world-x world)
              (world-y world)
              (world-deltax world)
              (world-deltay world)
              SPEED              ; Notice this changes
              false))            ; And so does this

;; one-step: world -> world
;; GIVEN: a world
;; RETURNS: a world that has moved one step farther on the line segment
;; Examples omitted
;; Strategy: structural decomposition on world (with slight functional composition)
(define (one-step world)
  (make-world (world-figure world)
              (+ (world-x world) (world-deltax world))  ; Change
              (+ (world-y world) (world-deltay world))  ; Change
              (world-deltax world)
              (world-deltay world)
              (- (world-clicks-to-go world) 1)          ; Change
              false))                                   ; Change, perhaps





