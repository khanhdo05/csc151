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
           
;; (Partner A drives!)
;;
;; Follow the style of these two functions to write a similar, third function
;; called digits->nums. (chars->codepoints lst) takes a list of characters as
;; input and returns a list where each character has been turned into its
;; integer codepoint value. For example:
;;
;; (chars->codepoints (list #\a #\0 #\; #\q #\!))
;; > (list 97 48 59 113 33)
;; (chars->codepoints null)
;; > null

;;; (chars->codepoints lst) -> list?
;;;   lst: list?
;;; Returns a list of codepoints for the characters
(define chars->codepoints
  (lambda (lst)
    (match lst
      [null null]
      [(cons head tail) (cons (char->integer head) (chars->codepoints tail))])))

(test-case "correctly returns codepoints" equal? 
                                          (list 97 48 113 33) 
                                          (lambda () (chars->codepoints (list #\a #\0 #\q #\!))))
(test-case "base case" equal? null (lambda () (chars->codepoints null)))