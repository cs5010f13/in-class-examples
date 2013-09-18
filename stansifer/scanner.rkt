;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname scanner-tests) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)

(define-struct suitcase (height width depth))

;; A Suitcase is a
;;   (make-suitcase Number Number Number)
;; interpretation: height, width, depth of the suitcase in cm

;; Template:
;; suitcase-fn : Suitcase -> ??
#;
(define (suitcase-fn sc)
  ... (suitcase-height sc)
  ... (suitcase-width sc)
  ... (suitcase-depth sc) ...)


(define-struct scanner (height width))

;; A Scanner is a
;;   (make-scanner Number Number)
;; interpretation: height and width of the opening in cm

;; Template:
;; scanner-fn : Scanner -> ??
#;
(define (scanner-fn scnr)
  ... (scanner-height scnr) ... (scanner-width scnr) ... )


;; Note: I didn't break this into two functions because of time constraints.

;; face-fits-scanner : Number Number Scanner -> Boolean
;; GIVEN: the height and width of a face and a Scanner
;; RETURNS: true iff the face can git through the scanner
;; (face-fits-scanner 10 20 (make-scanner 11 21)) = true
;; (face-fits-scanner 10 20 (make-scanner 11 20)) = false
;; (face-fits-scanner 10 20 (make-scanner 21 11)) = true
;; (face-fits-scanner 10 20 (make-scanner 40 40)) = true
;; Strategy: structural decomposition on scnr : Scanner
(define (face-fits-scanner height width scnr)
  (or (and (< height (scanner-height scnr))
           (< width (scanner-width scnr)))
      (and (< height (scanner-width scnr))
           (< width (scanner-height scnr)))))

(check-equal? (face-fits-scanner 10 20 (make-scanner 11 21)) true)
(check-equal? (face-fits-scanner 10 20 (make-scanner 11 20)) false)
(check-equal? (face-fits-scanner 10 20 (make-scanner 21 11)) true)
(check-equal? (face-fits-scanner 10 20 (make-scanner 40 40)) true)


;; suitcase-fits-scanner : Suitcase Scanner -> Boolean
;; GIVEN: a Suitcase and a Scanner
;; RETURNS: true iff the suitcase can fit through the scanner.
;; (suitcase-fits-scanner (make-suitcase 2 4 6) (make-scanner 3 5)) = true
;; (suitcase-fits-scanner (make-suitcase 2 4 6) (make-scanner 3 4)) = false
;; (suitcase-fits-scanner (make-suitcase 2 4 6) (make-scanner 5 3)) = true
;; (suitcase-fits-scanner (make-suitcase 4 6 2) (make-scanner 5 3)) = true
;; (suitcase-fits-scanner (make-suitcase 6 4 2) (make-scanner 5 3)) = true
;; Strategy: structural decomposition on sc : Suitcase

(define (suitcase-fits-scanner sc scnr)
  (or (face-fits-scanner (suitcase-height sc) (suitcase-width sc) scnr)
      (face-fits-scanner (suitcase-height sc) (suitcase-depth sc) scnr)
      (face-fits-scanner (suitcase-width sc) (suitcase-depth sc) scnr)))

(check-equal? (suitcase-fits-scanner (make-suitcase 2 4 6)
                                     (make-scanner 3 5))
              true)
(check-equal? (suitcase-fits-scanner (make-suitcase 2 4 6)
                                     (make-scanner 3 4))
              false)
(check-equal? (suitcase-fits-scanner (make-suitcase 2 4 6)
                                     (make-scanner 5 3))
              true)
(check-equal? (suitcase-fits-scanner (make-suitcase 4 6 2)
                                     (make-scanner 5 3))
              true)
(check-equal? (suitcase-fits-scanner (make-suitcase 6 4 2)
                                     (make-scanner 5 3))
              true)
