;;; In the 6pm section, we considered the following posting on
;;; Piazza.  We went through and edited it to conform to the
;;; design recipe.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ORIGINAL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; is the foll use of design recipe corect?

;data definition(also write interpretation) 



;function template for manuplating each kind of data 

;(define (sum_of_2_larger_no num1 num2 num3)...) 

;contract and purpose statement 

;sum_of_2_larger_no:number number nmber -> number 

;Given:3 numbers as parameter's to thefunction 
;Returns:The sum of the largest and the second largest number's 

;Examples 

;1,2,3, = 5 
;12,5,13 = 25 

;Design Strategy 

;Design strategy is function compostion 

;Function definition 

(define (sum_of_2_larger_no num1 num2 num3) 
(cond 
[(and (< num1 num2) (< num1 num3)) (+ num2 num3) ] 
[(and (< num2 num1) (< num2 num3)) (+ num1 num3)] 
[(and (< num3 num2) (< num3 num1)) (+ num2 num1)] 
) 

) 

;Tests 

(check-expect (sum_of_2_larger_no 1 2 3) 5) 
(check-expect (sum_of_2_larger_no 12 5 13) 25)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EDITED VERSION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; is the following use of the design recipe correct?

(require rackunit)

;data definition(also write interpretation) 

;; no data definitions.


;contract and purpose statement 

;sum-of-two-largest: Number Number Number -> Number 
;GIVEN: 3 numbers 
;RETURNS:The sum of the largest and the second largest numbers 

;EXAMPLES: 

;1,2,3 => 5 
;(sum-of-two-largest 12 5 13) = 25 

;Design strategy is domain knowledge

;Function definition 

;; we observed that the original definition didn't work when the three
;; numbers were all equal, or when the two smallest numbers were
;; equal.

;; We came up with three solutions.  The first just turned all of the
;; <'s into <='s:

(define (sum-of-two-largest num1 num2 num3) 
	(cond 
         [(and (<= num1 num2) (<= num1 num3)) (+ num2 num3)] 
         [(and (<= num2 num1) (<= num2 num3)) (+ num1 num3)] 
         [(and (<= num3 num2) (<= num3 num1)) (+ num2 num1)]))

;; The second solution took the sum of all the three numbers, and
;; threw out the smallest:

(define (sum-of-two-largest num1 num2 num3) 
 (- (+ num1 num2 num3)
    (min num1 num2 num3)))

;; The third solution just tried all 3 sums, and took the largest.

(define (sum-of-two-largest num1 num2 num3) 
  (max (+ num1 num2)
       (+ num2 num3)
       (+ num1 num3)))


;; TESTS: 

(check-equal? (sum-of-two-largest 1 2 3) 5) 
(check-equal? (sum-of-two-largest 12 5 13) 25)
(check-equal? (sum-of-two-largest 12 5 2) 17)
; this one broke the original version: 
(check-equal? (sum-of-two-largest 12 12 12) 24)


 
