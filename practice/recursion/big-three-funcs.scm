;; CSC 151 (Fall 2023)
;; Lab: Implementing the Big Three
;; Authors: Khanh Do, Paul Lim
;; Date: Oct 11, 2023
;; Acknowledgements: N/A

;; Whew! We've spent a solid week and a half drilling recursive design 
;; techniques over lists and the natural numbers. But this only the beginning!
;; We will deepen our knowledge of recursive design throughout the remainder
;; course as we explore more intricate and complex problems. As a starting
;; point in this conversation, we'll look at the patterns of recursion that
;; we have developed so far and how they relate to the "big three" operations
;; over lists we encountered earlier in the course.
(import lab)
(import test)
;; -------------------------
"Problem 1: Transformations"
;; -------------------------

;; Consider the following pair of recursive functions:

;;; (double lst) -> list?
;;;   lst: list? of numbers
;;; Returns lst but with every element of lst doubled.
(define double
  (lambda (lst)
    (match lst
      [null null]
      [(cons head tail) (cons (* 2 head) (double tail))])))

(test-case "double empty"
           equal?
           null
           (lambda () (double null)))

(test-case "double non-empty"
           equal?
           (list 0 2 4 6 8)
           (lambda () (double (range 5))))

;;; (flip lst) -> list?
;;;   lst: list? of booleans
;;; Returns lst but with every element of lst flipped, i.e., #t becomes #f
;;; and #f becomes true
(define flip
  (lambda (lst)
    (match lst
      [null null]
      [(cons head tail) (cons (not head) (flip tail))])))

(test-case "flip empty"
           equal?
           null
           (lambda () (flip null)))

(test-case "flip non-empty"
           equal?
           (list #t #f #f #t #t #f)
           (lambda () (flip (list #f #t #t #f #f #t))))