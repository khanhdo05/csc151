(import test)
; CSC 151 02 (23fa)
; Self-Checks
; Author: Khanh Do
; Date: 2023-09-25
; Acknowledgements: Following the instructions of the reading.

; Documenting your codes
; Check: Oops.
;;; (substring str start end) -> string?
;;;   str : string?
;;;   start : integer?, a 0-based index (<= 0 start (length str))
;;;   end : integer?, a 0-based index (and (<= 0 end (length str)) (<= start))
;;; Returns the substring of `str` denoted by the start index `start` 
;;; (inclusive) and end index `end` (exclusive).

; Unit Testing
; Check 3: Testing bound-grade
; Sketch a set of tests for the bound-grade procedure, 
; which takes a real number as input and outputs
(define bound-grade
  (lambda (n) n))
; That number, if it is between 0 and 100, inclusive.
(test-case "Grade is between 0 and 100, inclusively" (=-eps 0) 51 (lambda () (bound-grade 51)))
(test-case "Grade is between 0 and 100, inclusively" (=-eps 0) 0 (lambda () (bound-grade 0)))
(test-case "Grade is between 0 and 100, inclusively" (=-eps 0) 100 (lambda () (bound-grade 100)))
; Zero, if it is less than 0.
(test-case "Grade is less than 0" (=-eps 0) (bound-grade -5) -5)
; 100, if it is greater than 100.
(test-case "Grade is between 0 and 100, inclusively" (=-eps 0) 5 (lambda () (bound-grade 5)))
(test-case "Grade is between 0 and 100, inclusively" (=-eps 0) 100 (lambda () (bound-grade 100)))

; Hypothesis-driven debugging
; 
(define make-change
  (lambda (n)
    (let ([quarters (quotient n 25)]
          [left-over (remainder n 25)]
          [dimes (quotient n 25)]
          [left-over (remainder n 25)]
          [nickels (quotient n 25)]
          [cents (remainder n 25)])
      (list quarters dimes nickels cents))))
(test-case "change example"
  equal? (list 5 0 0 4) (lambda () (make-change 129)))
; When I run procedure make-change, I received a parser error 
; Cannot use reserved word as identifier name: let* on line 37 
; => change to let

; Run -> Runtime error [40:34-40:42]: Referenced unbound identifier "left-over"
; => Change every "left-over" to "n"