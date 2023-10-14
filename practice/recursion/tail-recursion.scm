;; CSC 151-02 (Fall 2023)
;; Lab: Tail Recursion
;; Authors: Khanh Do, Paul Lim
;; Date: Oct 13, 2023
;; Acknowledgements: N/A

(import test)
; +-------------------------+----------------------------------------
; | Exercise 1: Preparation |
; +-------------------------+

; In today's reading, you wrote a tail-recursive implementation of
; `append`. To begin the lab, review your code with your partner and
; agree upon a solution. In this lab, we'll rewrite several functions
; to be tail-recursive and test them in a standardized way. In
; particular, we shouldn't change the external behavior of a function,
; i.e., what output it produces for a given input. So we can use our
; non-recursive function to test our tail-recursion function!
;
; Below is the standard implementation of append, yet again:

(define append-l
  (lambda (l1 l2)
    (match l1
      [null l2]
      [(cons head tail) (cons head (append tail l2))])))

; Fill in the definition of (a) your tail-recursive helper function
; and (b) your wrapper function for append below. (Hint: you should
; have found that so-far is backwards if you just follow the pattern
; from the reading. If you did not already do so, think about how
; you can use the reverse function to fix the issue.)

;;; (append-helper so-far l1 l2) -> list?
;;;   so-far: list?
;;;   l1: list?
;;;   l2: list?
;;; Append two lists using tail recursion.
(define append-helper
  (lambda (so-far l1 l2)
    (match l1
      [null (append (reverse so-far) l2)]
      [(cons head tail)
       (append-helper (cons head so-far) tail l2)])))

;;; (append-tr l1 l2) -> list?
;;;   l1: list?
;;;   l2: list?
;;; Passing null as so-far.
(define append-tr 
  (lambda (l1 l2)
    (append-helper null l1 l2)))

(test-case "append-tr non-empty" 
           equal?
           (list 1 2 3 4 5)
           (lambda () (append-tr (list 1 2 3) (list 4 5))))

(test-case "append-tr empty" 
           equal?
           (list 4 5)
           (lambda () (append-tr null (list 4 5))))

(append-tr (list 1 2 3) (list 4 5 6))

; Finally, trace through the execution of append-tr on the second
; test case:
;
;     (append-tr (list 1 2 3) (list 4 5 6))
; --> (append-helper null (1 2 3) (4 5 6))
; --> (match (1 2 3) ([() (append (reverse ()) (4 5 6))] [(cons head tail) (append-helper (cons head ()) tail (4 5 6))]))
; --> (append-helper (cons 1 ()) (2 3) (4 5 6))
; --> (append-helper (list 1) (2 3) (4 5 6))
; --> (match (2 3) ([() (append (reverse (1)) (4 5 6))] [(cons head tail) (append-helper (cons head (1)) tail (4 5 6))]))
; --> (append-helper (cons 2 (1)) (3) (4 5 6))
; --> (append-helper (list 2 1) (3) (4 5 6))
; --> (match (3) ([() (append (reverse (2 1)) (4 5 6))] [(cons head tail) (append-helper (cons head (2 1)) tail (4 5 6))]))
; --> (append-helper (cons 3 (2 1)) () (4 5 6))
; --> (append-helper (list 3 2 1) () (4 5 6))
; --> (match () ([() (append (reverse (3 2 1)) (4 5 6))] [(cons head tail) (append-helper (cons head (3 2 1)) tail (4 5 6))]))
; --> (append (reverse (3 2 1)) (4 5 6))
; --> (append (list 1 2 3) (4 5 6))
; --> (list 1 2 3 4 5 6)