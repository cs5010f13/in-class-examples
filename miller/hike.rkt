;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hike) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)

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
       (segment-rise segment)))

;; A Path is a list, where each element is a Segment

(define (minutes->hours minutes)
  (/ minutes 60))

(define SPEED-UPHILL 1)   ; miles / hour
(define SPEED-DOWNHILL 3) ; miles / hour

;; rising? : Number -> Boolean
;; GIVEN: A Number, representing a rise in feet
;; RETURNS: true iff the Number indicates a an uphill climb
;; Examples:
;; ...
;; Strategy: domain knowledge
(define (rising? rise)
  (> rise 0))

;; add-time : PosNum PosNum Boolean -> PosNum
;; GIVEN: a distance in feet, time in hours, and whether walk is uphill
;; RETURNS: the original time plus the amount of additional time it
;;   takes to walk the given distance
;; Examples:
;; ...
;; Strategy: domain knowledge
(define (add-time distance time rising?)
  (+ time
     (/ distance 
        (if rising? SPEED-UPHILL SPEED-DOWNHILL))))

;; add-segment : Segment Hike -> Hike
;; GIVEN: A Segment and a Hike
;; RETURNS: The Hike resulting from first walking the Hike and then
;;   walking the segment
;; Examples:
;; ...
;; Strategy: function composition
(define (add-segment segmnt hke)
  (make-hike (add-time (segment-length segmnt)
                       (hike-duration hke)
                       (rising? (segment-rise segmnt)))
             (+ (hike-length hke) (segment-length segmnt))
             (+ (hike-rise hke)
                (segment-uphill-rise (segment-rise segmnt)))))

;; add-path : Path Hike -> PartialHike
;; GIVEN: a Path and a Hike
;; RETURNS: the Hike resulting from first walking the Hike then walking the Path
;; Examples:
;; ...
;; Strategy: structural decomposition on Path (list of Segments)
(define (add-path pth hke)
  (cond [(empty? pth) hke]
        [else (add-path (rest pth)
                        (add-segment (first pth) hke))]))

;; segment-uphill-rise : Number -> PosNum
;; GIVEN: the rise in feet
;; RETURNS: the amount of uphill climb represented by the rise
;; Examples:
;; (segment-uphill-rise (make-segment 1.5 200)) = 200
;; (segment-uphill-rise (make-segment 1.5 0) = 0
;; (segment-uphill-rise (make-segment 1.5 -0.3) = 0
;; Strategy: domain knowledge
(define (segment-uphill-rise rise)
  (if (rising? rise) rise 0))

;; path->hike : Path -> Hike
;; GIVEN: a Path
;; RETURNS: the Hike that results if you follow the Path
;; Examples:
;; (path->hike (list (make-segment 0.5 100))) = 
;;   (make-hike (/ 0.5 SPEED-UPHILL) 0.5 100)
;; (path->hike (list (make-segment 1.5 0))) =
;;   (make-hike (/ 1.5 SPEED-DOWNHILL) 1.5 0)
;; Strategy: function composition
(define (path->hike path)
  (add-path path (make-hike 0 0 0)))
;; Tests
(check-equal? (path->hike (list (make-segment 0.5 100)))
              (make-hike (/ 0.5 SPEED-UPHILL) 0.5 100))
(check-equal? (path->hike (list (make-segment 1.5 0)))
              (make-hike (/ 1.5 SPEED-DOWNHILL) 1.5 0))



