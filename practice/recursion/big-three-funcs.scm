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

;; (Partner B drives!)
;;
;; At this point, you should have noticed some redundancy between the
;; three implementations of these functions. In the space below, note which
;; parts of the three functions are shared and what is different between them.
;;
;; Shared: all three functions perform some action on the head of the list, then cons that
;;         to the function of the tail.
;;
;; Different: the types of inputs they take, the actions they perform on the head, etc
;;
;; Before moving on, check your work with a member of the course staff!

;; After you have correctly identified the essential difference between the
;; three functions, let's do exactly what we learned at the beginning of this
;; course: write a function that factors out these differences. It turns out
;; that this function is precisely the map function over lists! Follow your
;; nose and implement the map function over lists by factoring out the
;; essential difference in the implementations above and making it a parameter
;; to your function. You should arrive at precisely the same function signature
;; as the Prelude map function. Give test cases for your implementation of map,
;; list-map, that show how you can use list-map to implement the behavior of
;; the three specialized functions above.

;; (list-map func lst) -> list?
;;  func : procedure?, a function
;;  lst : list?
;; Returns a list with func applied to all elements of lst.
(define list-map
  (lambda (func lst)
    (match lst
      [null null]
      [(cons head tail) (cons (func head) (list-map func tail))])))

(test-case "map double empty"
           equal?
           null
           (lambda () (list-map (lambda (n) (* n 2)) null)))

(test-case "map double non-empty"
           equal?
           (list 0 2 4 6 8)
           (lambda () (list-map (lambda (n) (* n 2)) (range 5))))

(test-case "map flip empty"
           equal?
           null
           (lambda () (list-map (lambda (boole) (not boole)) null)))

(test-case "map flip non-empty"
           equal?
           (list #t #f #f #t #t #f)
           (lambda () (list-map (lambda (boole) (not boole)) (list #f #t #t #f #f #t))))

(test-case "map codepoints empty" 
           equal? 
           null 
           (lambda () (list-map (lambda (cha) (char->integer cha)) null)))

(test-case "map codepoints non-empty" 
           equal? 
           (list 97 48 113 33) 
           (lambda () (list-map (lambda (cha) (char->integer cha)) (list #\a #\0 #\q #\!))))

;; ------------------
"Problem 2: Deletion"
;; ------------------

;; Let's play the same game of observing similarities between functions and
;; factoring out the differences to create a new function! Consider these
;; specialized functions:

;;; (dropzeroes lst) -> list?
;;;   lst: list? of numbers
;;; Returns lst but with every zero removed from lst.
(define dropzeroes
  (lambda (lst)
    (match lst
      [null null]
      [(cons head tail)
       (if (zero? head) (dropzeroes tail)
                        (cons head (dropzeroes tail)))])))

(test-case "dropzeroes empty"
           equal?
           null
           (lambda () (dropzeroes null)))

(test-case "dropzeroes non-empty"
           equal?
           (list 1 1 2 1)
           (lambda () (dropzeroes (list 1 0 0 1 2 0 1 0))))

;;; (length-less-than-five lst) -> list?
;;;   lst: list? of strings
;;; Returns lst but with every element with length greater than or equal to
;;; five removed from the output.
(define length-less-than-five
  (lambda (lst)
    (match lst
      [null null]
      [(cons head tail)
       (if (>= (string-length head) 5) (length-less-than-five tail)
                                       (cons head (length-less-than-five tail)))])))

(test-case "length-less-than-five empty"
           equal?
           null
           (lambda () (length-less-than-five null)))

(test-case "length-less-than-five non-empty"
           equal?
           (list "abba" "doo!")
           (lambda () (length-less-than-five (list "abba" "yabba" "dabba" "doo!"))))

;; (Partner B drives!)
;;
;; Follow the style of these two functions to write a similar, third function
;; called dropfalses. (dropfalses lst) takes a list of booleans as input and
;; and returns a lst but with all the #f values removed from the result. For
;; example:
;;
;; (dropfalses (list #t #t #f #f #f #t #f #t))
;; > (list #t #t #t #t)
;; (dropfalses null)
;; > null

;;; (dropfalses lst) -> list?
;;;   lst: list?, list of boolean values.
;;; Returns lst with every #f removed.
(define dropfalses
  (lambda (lst)
    (match lst
      [null null]
      [(cons head tail)
       (if (equal? head #t) 
           (cons head (dropfalses tail)) 
           (dropfalses tail))])))

(test-case "dropfalses empty" equal? null (lambda () (dropfalses null)))

(test-case "dropfalses non-empty" 
           equal? 
           (list #t #t #t #t) 
           (lambda () (dropfalses (list #t #t #f #f #f #t #f #t))))

;; (Partner A drives!)
;;
;; Like the previous problem, first identify what is shared and different
;; between these three functions:
;;
;; Shared: you compare a predicate to the head, drop it if it does not match the predicate
;;         or keep it if it does, and cons that to the function of the tail.
;;
;; Different: the types of inputs they take, the predicate they compare to the head, etc
;;
;; Check your work with a member of the course staff!
;;
;; Once you know the essential difference between these three functions, create
;; the list-filter function that factors out this redundancy. list-filter
;; should behave indentically to the filter function when you are done!

;;; (list-filter pred lst) -> list?
;;;  pred : procedure?, a predicate
;;;  lst : list?
;;; Returns lst only with elements matching the predicate.
(define list-filter
  (lambda (pred lst)
    (match lst
      [null null]
      [(cons head tail) (if (pred head)
                            (cons head (list-filter pred tail))
                            (list-filter pred tail))])))

(test-case "filter dropzeroes empty"
           equal?
           null
           (lambda () (list-filter (lambda (n) (not (zero? n))) null)))

(test-case "filter dropzeroes non-empty"
           equal?
           (list 1 1 2 1)
           (lambda () (list-filter (lambda (n) (not (zero? n))) (list 1 0 0 1 2 0 1 0))))

(test-case "filter length-less-than-five empty"
           equal?
           null
           (lambda () (list-filter (lambda (str) (< (string-length str) 5)) null)))

(test-case "filter length-less-than-five non-empty"
           equal?
           (list "abba" "doo!")
           (lambda () (list-filter (lambda (str) (< (string-length str) 5))
                                   (list "abba" "yabba" "dabba" "doo!"))))

(test-case "filter dropfalses empty" 
           equal? 
           null 
           (lambda () (list-filter (lambda (boole) (equal? #t boole)) null)))

(test-case "filter dropfalses non-empty" 
           equal? 
           (list #t #t #t #t) 
           (lambda () (list-filter (lambda (boole) (equal? #t boole))
                                   (list #t #t #f #f #f #t #f #t))))