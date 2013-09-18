;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname airport-scanner) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))


;; This is the version from the 600 section, with some notes adding
;; what we did in the 740 section.

(define-struct suitcase (length width height))

;; A Suitcase is a (make-suitcase PosNum PosNum PosNum)
;; Interp:
;; length, width, and height are the dimensions of the suitcase
;; in cm.

;; template
;; suitcase-fn : Suitcase -> ??
(define (suitcase-fn sc)
  (... (suitcase-length sc)
       (suitcase-width sc)
       (suitcase-height sc)))

(define-struct scanner (width height))
;; A Scanner is a (make-scanner PosNum PosNum)
;; Interp:
;; width and height are the width and height of the intake,
;; in cm.

;; template
;; scanner-fn : Scanner -> ??
(define (scanner-fn s)
  (... (scanner-width s)
       (scanner-height s)))


;; suitcase-fits-scanner : Suitcase Scanner -> Boolean
;; GIVEN: a Suitcase and a Scanner
;; RETURNS: true iff the suitcase can fit through the scanner.
;; Examples:
;; (suitcase-fits-scanner (make-suitcase 12 35 24) (make-scanner 15 28))
;; = true
;; (suitcase-fits-scanner (make-suitcase 35 12 24) (make-scanner 15 28))
;; = true
;; (suitcase-fits-scanner (make-suitcase 12 35 24) (make-scanner 11 28))
;; = false
;; (suitcase-fits-scanner (make-suitcase 12 35 24) (make-scanner 15 23))
;; = false
;; Strategy: structural decomposition on sc : Suitcase
(define (suitcase-fits-scanner sc scanner)
  (or
   (face-fits-scanner? (suitcase-length sc) (suitcase-width sc) scanner)
   (face-fits-scanner? (suitcase-length sc) (suitcase-height sc) scanner)
   (face-fits-scanner? (suitcase-width sc) (suitcase-height sc) scanner)))

;; tests omitted.

;;; In the 740 section, we came up with a different solution.  A
;;; student observered that if the suitcase was going to fit at all,
;;; it would fit with the two smallest dimensions facing forward.  So
;;; the "or" is unecessary.  We 

;; STRATEGY: structural decomposition on sc: Suitcase
(define (suitcase-fits-scanner sc scanner)
  (face-fits-scanner?
   (min (suitcase-length sc)
       (suitcase-width sc)
       (suitcase-height sc))
   (second-smallest
    (suitcase-length sc)
    (suitcase-width sc)
    (suitcase-height sc))
   scanner))
  


;; face-fits-scanner? : PosNum PosNum Scanner
;; GIVEN: the dimensions of a face of the suitcase, and a scanner
;; RETURNS: true iff that face will fit through the scanner
;; EXAMPLES:
;; (face-fits-scanner? 12 24 (make-scanner 15 28)) = true
;; (face-fits-scanner? 12 24 (make-scanner 15 23)) = false
;; (face-fits-scanner? 24 12 (make-scanner 15 23)) = false
;; Strategy: structural decomp on s : Scanner
(define (face-fits-scanner? h w s)
  (dimensions-fit? h w (scanner-width s) (scanner-height s)))

;; dimensions-fit? : PosNum PosNum PosNum PosNum -> Boolean
;; GIVEN: height and width and another height width
;; RETURNS: true iff the correspondng rectangles will fit.
;; EXAMPLES: ...
;; STRATEGY: domain knowledge
(define (dimensions-fit? h1 w1 h2 h2)
  (or
   (and (< h1 h2) (< w1 w2))
   (and (< h1 w2) (< w1 h2))))

;; tests....

;; STILL ON THE WISHLIST:

;; second-smallest : Number Number Number -> Number
;; GIVEN: 3 numbers
;; RETURNS: the second smallest of the numbers

