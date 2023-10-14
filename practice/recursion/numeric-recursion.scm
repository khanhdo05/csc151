
;; CSC 151 (23fa)
;; Lab: Numeric Recursion (numeric-recursion.scm)
;; Authors: Khanh Do, Paul Lim
;; Date: 9 Oct 23
;; Acknowledgements:
;;   Following instructions on webpage.

(import image)
(import test)

;; -------------------
"Problem 1: Replicate"
;; -------------------

;; (Partner A drives!)

;; Implement a recursive function (replicate v n) that takes a value v and
;; natural number n as input and returns a list that contains n copies of v.
;;
;; (replicate "q" 5)
;; > (list "q" "q" "q" "q" "q")
;; (replicate "hello" 0)
;; > (list)
;;
;; Make sure to give a recursive decomposition, docstring, and test suite
;; for the function.
;;
;; (Note that replicate is really another name for make-list, so you can
;; use make-list to easily write test cases for your function).
;;
;; To replicate a value v n times:
;; + If n is zero, return null.
;; + If n is non-zero, cons v to (replicate v (n-1)).

;;; (replicate v n) -> list?
;;;  v : any?
;;;  n : integer?, n >= 0
;;; Returns a list that contains n copies of v.
(define replicate
  (lambda (v n)
    (match n
      [0 null]
      [_ (cons v (replicate v (- n 1)))])))

(test-case "null" equal? null (lambda () (replicate "q" 0)))
(test-case "q 5" equal? (make-list 5 "q") (lambda () (replicate "q" 5)))
(test-case "(list 3 a) 5" equal? (make-list 5 (list 3 "a")) 
                                 (lambda () (replicate (list 3 "a") 5)))

;; -----------------
"Problem 2: Harmony"
;; -----------------

;; (Partner B drives!)

;; Implement a recursive function (harmonic-sequence-sum n) that takes a
;; natural number n as input and returns the sum of the first n terms of the
;; harmonic sequence. The harmonic sequence is defined as follows:
;;
;; 1/1 + 1/2 + 1/3 + 1/4 + 1/5 + ...
;;
;; (harmonic-sequence-sum 5)
;; > 2.283333333333333
;; (harmonic-sequence-sum 100)
;; > 5.187377517639621
;; (harmonic-sequence-sum 0) 
;; > 0
;;
;; Make sure to give a recursive decomposition, docstring, and test suite
;; for the function.
;;
;; The sum of the first n terms of the harmonic-sequence is:
;; + If n is zero... returns 0
;; + If n is non-zero, add up 1/n with (harmonic-sequence-sum (- n 1))

;;; (harmonic-sequence-sum n) -> integer?
;;;   n: integer?
;;; Returns the sum of the first n terms of the harmonic sequence.
(define harmonic-sequence-sum
  (lambda (n)
    (match n
      [0 0]
      [_ (+ (/ 1 n) (harmonic-sequence-sum (- n 1)))])))

(test-case "n=5" (=-eps 0) 2.283333333333333 (lambda () (harmonic-sequence-sum 5)))
(test-case "n=100" (=-eps 0) 5.187377517639621 (lambda () (harmonic-sequence-sum 100)))
(test-case "base case" = 0 (lambda () (harmonic-sequence-sum 0) ))

;; --------------
"Problem 3: Drop"
;; --------------

;; (Partner A drives!)

;; Implement a recursive function (my-drop n l) that takes a list l and natural
;; number n and returns l, but with the first n elements of l removed. If
;; n is greater than the length of l, then null is returned.
;;
;; (my-drop 3 (range 10))
;; > (list 3 4 5 6 7 8 9)
;; (my-drop 0 (range 10))
;; > (list 0 1 2 3 4 5 6 7 8 9)
;; (my-drop 5 null)
;; > null
;;
;; For my-drop, you will need to decompose both n and l! Consequently, write
;; your recursive decomposition in terms of the 4 cases we have based on the
;; recursive definitions for natural numbers and lists. To pattern match on
;; both n and l at the same time, you can use pair them up, e.g., (pair n l)
;; creates a pair of n and l that you can pattern match on with
;; cons as the pattern. It turns out that pair is just an alias for cons!
;;
;; (Note that my-drop is really the drop function provided in the standard
;; library. Feel free to use drop in your test cases!)
;;
;; To drop n elements from l:
;; 1. When n is 0 and l is null : null
;; 2. When n is non-zero and l is null : null
;; 3. When n is 0 and l is not empty : l
;; 4. When n is non-zero and l is not empty : drop head and call (my-drop (- n 1) tail)

;;; (my-drop n l) -> list?
;;;  n : integer?, non-negative
;;;  l : list?
;;; Returns l with the first n elements removed.
(define my-drop
  (lambda (n l)
    (match (pair n l)
      [(pair 0 null) null]
      [(pair n null) null]
      [(pair 0 l) l]
      [(pair n (cons head tail)) (my-drop (- n 1) tail)])))

(test-case "0 null" equal? null (lambda () (my-drop 0 null)))
(test-case "0 (list 1 2 3)" equal? (list 1 2 3) (lambda () (my-drop 0 (list 1 2 3))))
(test-case "5 null" equal? null (lambda () (my-drop 5 null)))
(test-case "3 (range 10)" equal? (list-drop (range 10) 3) (lambda () (my-drop 3 (range 10))))

;; --------------
"Problem 4: Take"
;; --------------

;; (Partner A drives!)

;; Implement a recursive function (my-take n l) that takes a list l and natural
;; number n and returns the first n elements of l as a list. If n is greater
;; than l, then l is returned.
;;
;; (my-take 3 (range 10))
;; > (list 0 1 2)
;; (my-take 0 (range 10))
;; > (list)
;; (my-take 5 null)
;; > null
;;
;; Like my-drop, my-take will need to decompose both n and l. Your recursive
;; decomposition should have 4 cases based on the recursive definitions for
;; natural numbers nad lists.
;;
;; (Note that my-take is really the take function provided in the standard
;; library. Feel free to use take in your test cases!)
;;
;; To take n elements from l:
;; 1. When n is 0 and l is null : null
;; 2. When n is non-zero and l is null : null
;; 3. When n is 0 and l is not empty : null
;; 4. When n is non-zero and l is not empty : cons head to (my-take (- n 1) tail)

;;; (my-take n l) -> list?
;;;   n: integer?
;;;   l: list?
;;; Returns the first n elements of l as a list
(define my-take
  (lambda (n l)
    (match (pair n l)
      [(pair 0 null) null]
      [(pair n null) null]
      [(pair 0 l) null]
      [(pair n (cons head tail)) 
       (cons head (my-take (- n 1) tail))])))
 
(test-case "3 range 10" equal? (list-take (range 10) 3) (lambda () (my-take 3 (range 10))))
(test-case " 0 range 10" equal? (list-take (range 10) 0) (lambda () (my-take 0 (range 10))))
(test-case "base case" equal? null (lambda () (my-take 5 null)))
;; ------------------- 
"Problem 5: Triangles"
;; -------------------

;; Sierpinski triangles are a fractal drawing composed of a collection of
;; triangles nested inside of each other according to the following rules
;; (taken from: https://en.wikipedia.org/wiki/Sierpi%C5%84ski_triangle):
;; 
;; 1. Start with an equilateral triangle.
;; 2. Subdivide it into four smaller congruent equilateral triangles and
;;    remove the central triangle.
;; 3. Repeat step 2 with each of the remaining smaller triangles infinitely.
;;
;; Call each layer of Sierpinski triangles a level. At level 0, we draw no
;; triangles.
;;
;; Examples of Sierpinski Triangles for nesting level n = 1, n = 2, and n = 3
;; can be found on the webpage for this lab.
;;
;; Write a recursive function (sierpinski size color n) that draws n levels
;; of sierpinski triangles in a size × size drawing. Again, proceed by
;; numeric recursion, give a recursive decomposition and appropriate docstring.
;;
;; (Partner A, drive for the recursive decomposition of the function!)
;;
;; To draw n levels of Sierpinski Triangles:
;; + When n = 0: <TODO: fill me in>
;; + When n > 0: <TODO: fill me in>
;;
;; (Partner B, drive for the implementation of the function!)

(define sierpinski
  (lambda (size color n)
    (match n
      [0 (triangle 0 "solid" color)]
      [1 (triangle size "solid" color)]
      [_ (overlay (above (rotate 180 (sierpinski (/ size (+ n 1)) "white" (- n 1)))
                         (rotate 180 (sierpinski (/ size (+ n 1)) "white" (- n 2))))
                  (triangle size "solid" "black"))])))

(sierpinski 100 "black" 0)
(sierpinski 100 "black" 1)
(sierpinski 100 "black" 2)
(sierpinski 100 "black" 3)
;; ------------------------------
"Extra Problem: Fractal Drawings"
;; ------------------------------

;; Sierpinsky triangles are an example of a fractal, an infinitely recursive
;; drawing:
;;
;; https://en.wikipedia.org/wiki/Fractal
;;
;; There are many kinds of fractals out there! A simple example is the
;; Cantor set:
;;
;; https://en.wikipedia.org/wiki/Cantor_set
;;
;; Or the Koch snowflake:
;;
;; https://en.wikipedia.org/wiki/Koch_snowflake
;;
;; Try implementing a function that draws one of these recursive images to
;; an arbitrary level of depth. Or try designing your own fractal drawing using
;; the image library primitives!