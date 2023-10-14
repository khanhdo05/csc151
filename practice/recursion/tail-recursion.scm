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
      [null (append (reverse so-far) l2)] ; Because we append head, and then append head to before head -> so-far need to be reversed to be in correct order.
      [(cons head tail)
       (append-helper (cons head so-far) tail l2)]))) ; Append head to so-far to make a new so-far, and then recursive call to so-far tail as l1 and l2.

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

; In a sentence or two below, describe how you know append-tr is tail
; recursive from your trace.
;
; It cons head to so-far to make a new so-far.

; +---------------------------------------+--------------------------
; | Exercise 2: Tail Recursing Over Lists |
; +---------------------------------------+

; It's a blast from the past! Let's rewrite some of our basic
; recursives over lists. For each function:
;
;   1. Write a tail-recursive version of the function.
;   2. Write a collection of test cases that uses the original
;      version of the function to test the tail-recursive version.
;
; (product l) -> number?
;   l : list? of numbers
; Returns the product of the numbers in l.
(define product
  (lambda (l)
    (match l
      [null 1]
      [(cons head tail) (* head (product tail))])))

; TODO: complete the tail-recursive version of the function below.

;;; (product-helper so-far l) -> number?
;;;   so-far: integer?
;;;   l: list?, a list of numbers
;;; Returns the product of all the numbers in list l.
(define product-helper
  (lambda (so-far l)
    (match l
      [null so-far]
      [(cons head tail) 
       (product-helper (* head so-far) tail)])))

;;; (product-tr l) -> number?
;;;   l: list?, a list of numbers
;;; Passing 1 as so-far.
(define product-tr
  (lambda (l)
    (product-helper 1 l)))

(test-case "product-tr empty" 
           equal?
           (product null)
           (product-tr null))

(test-case "product-tr list of positive numbers" 
           equal?
           (product (list 1 2 3 4))
           (product-tr (list 1 2 3 4)))

(test-case "product-tr list of both positive and negative numbers"
           equal?
           (product (list -1 -2 3 4 -5))
           (product-tr (list -1 -2 3 4 -5)))

(test-case "product-tr list of 1 num"
           equal?
           (product (list 100))
           (product-tr (list 100)))
; ...

; (any l) -> boolean?
;   l : list? of booleans
; Returns #t iff at least one of the booleans in l is #t
(define any
  (lambda (l)
    (match l
      [null #f]
      [(cons head tail) (or head (any tail))])))

; TODO: complete the tail-recursive version of the function below.

(define any-tr
  (lambda (l)
    ; TODO: fill me in!
    #f))

; TODO: fill in suitable test cases for any/any-tr below:

; ...