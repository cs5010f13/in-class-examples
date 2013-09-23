;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hike) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define-struct hike (duration length rise))
;; A Hike is (make-hike PosNum PosNum PosNum)
;; Interp:
;; duration is the time it takes to make the hike, in hours
;; length is the total distance, in miles, of the hike
;; rise is the total rise (ignoring descending segments) in feet

;; template
;; hike-fn : Hike -> ??
(define (hike-fn hike)
  (... (hike-duration hike)
       (hike-length hike)
       (hike-rise hike)))

(define-struct segment (length rise))
;; A Segment is (make-segment PosNum Number)
;; Interp:
;; length is the length of the segment, in miles
;; rise is the height difference, in feet, between the end and start
;;   of the segment (positive rise means the end is above the start)

;; template
;; segment-fn : Segment -> ??
(define (segment-fn segment)
  (... (segment-length segment)
       (segment-time segment)))

;; A Step is one of
;; - a Segment
;; - "view"

;; A Path is a list, where each element is a Step

(define-struct partialhike (hike sincepause rising))
;; A PartialHike is (make-partialhike hike sincepause rising)
;; Interp:
;; hike is the Hike already completed
;; sincepause is the time, in hours, since the end of the last pause
;; rising is a boolean, true iff the last segment was uphill

;; template
;; partialhike-fn : partialhike -> ??
(define (partialhike-fn parthike)
  (... (partialhike-hike parthike)
       (partialhike-sincepause parthike)
       (partialhike-rising parthike)))

(define (minutes->hours minutes)
  (/ minutes 60))

(define SPEED-UPHILL 1)   ; miles / hour
(define SPEED-DOWNHILL 3) ; miles / hour
(define VIEW-PAUSE (minutes->hours 10))
(define TIRED-PAUSE (minutes->hours 5))
(define PAUSE-INTERVAL-DOWN 1)
(define PAUSE-INTERVAL-UP (minutes->hours 30))

;; path->hike : Path -> Hike
;; GIVEN: a Path
;; RETURNS: the hike that results if you follow the path
;; Examples:
;; (path->hike (list "view")) = (make-hike VIEW-PAUSE 0 0)
;; (path->hike (list (make-segment 0.5 100))) = 
;;   (make-hike (/ 0.5 SPEED-UPHILL) 0.5 100)
;; (path->hike (list (make-segment 1.5 0))) =
;;   (make-hike (+ (/ 1.5 SPEED-DOWNHILL) TIRED-PAUSE) 1.5 0)
;; Strategy: function composition
(define (path->hike path)
  (partialhike-hike
   (add-path path
	     (make-partialhike (make-hike 0 0 0) 0 false))))

;; add-path : Path PartialHike -> PartialHike
;; GIVEN: a Path and a PartialHike
;; RETURNS: the PartialHike resulting from walking the path
;; Examples:
;; (add-path (list "view") (make-partialhike (make-hike 0 0 0) 0 false)) =
;;   (make-partialhike (make-path VIEW-PAUSE 0 0) VIEW-PAUSE false)
;; ...
;; Strategy: structural decomposition on Path (list of Step)
(define (add-path path parthike)
  (cond [(empty? path) parthike]
	[else (add-path (rest path)
			(add-step (first path) parthike))]))

;; add-step : Step PartialHike -> PartialHike
;; GIVEN: A Step and a PartialHike
;; RETURNS: The PartialHike resulting from taking the Step after
;;   completing the PartialHike
;; Examples:
;; ...
;; Strategy: structural decomposition on Step
(define (add-step step parthike)
  (cond [(segment? step) (add-segment step parthike)]
	[else (add-time VIEW-PAUSE parthike)]))

;; add-time : PosNum PartialHike -> PartialHike
;; GIVEN: a time (in hours) and a PartialHike
;; RETURNS: a PartialHike that adds a rest stop (if needed) to the
;;   PartialHike
;; Examples:
;; ...
;; Strategy: function composition
(define (add-time hours parthike)
  (cond
   [(partialhike-rising parthike) (add-uphill-time hours parthike)]
   [else (add-downhill-time hours parthike)]))

;; add-uphill-time : PosNum PartialHike -> PartialHike
;; GIVEN: a time (in hours) and a PartialHike
;; RETURNS: a PartialHike that includes a rest pause for an uphill
;;   climb if needed
;; Examples:
;; ...
;; Strategy: function composition
(define (add-uphill-time hours parthike)
  (add-time-and-pause (+ hours (partialhike-sincepause parthike))
		      PAUSE-INTERVAL-UP
		      parthike))

;; add-downhill-time : PosNum PartialHike -> PartialHike
;; GIVEN: a time (in hours) and a PartialHike
;; RETURNS: a PartialHike that includes a rest pause for an uphill
;;   climb if needed
;; Examples:
;; ...
;; Strategy: function composition
(define (add-downhill-time hours parthike)
  (add-time-and-pause (+ hours (partialhike-sincepause parthike))
		      PAUSE-INTERVAL-DOWN
		      parthike))

;; add-time-and-pause : PosNum PosNum PartialHike
;; GIVEN: a time (in hours), and interval to a pause (in hours), and a
;;   PartialHike
;; RETURNS: a PartialHike that is time hours longer, includng a pause
;;   if needed
;; Examples:
;; ...
;; Strategy: domain knowledge
(define (add-time-and-pause hours interval parthike)
  (make-partialhike
   (update-hike
    (+ hours (if (> hours interval) TIRED-PAUSE 0))
    (hike-length (partialhike-hike parthike))
    0
    (partialhike-hike parthike))
   (remainder (+ hours (if (> hours interval) TIRED-PAUSE 0))
	      TIRED-PAUSE)
   (partialhike-rising parthike)))

;; update-hike : PosNum PosNum Number Hike -> Hike
;; GIVEN: a time (in hours), a distance (in miles), a boolean, and a Hike
;; RETURNS: a new Hike that takes "time" longer, is "distance" longer,
;;   and has additional rise of "rise" than the given Hike
;; Examples:
;; ...
(define ..hike (make-hike 0 0 0))
(define ..sincepause 0)
(define ..rising false)
(define (update-hike time distance rise hike)
  (make-hike (+ time (hike-duration hike))
	     (+ distance (hike-length hike))
	     (+ (hike-rise hike)
		(if (> rise 0) rise 0))))

;;;; WISH LIST
;; add-segment : Segment PartialHike -> PartialHike
;; GIVEN: a Segment and a PartialHike
;; RETURNS: a PartialHike that reults after walking the original partial
;;   hike followed by the Segment
;; Examples:
;; ...
(define (add-segment segment parthike)
  (make-partialhike ..hike ..sincepause ..rising))
