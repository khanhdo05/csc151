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

;; -------------------
"Problem 3: Reduction"
;; -------------------

;; At this point, there's one function left to write from the big three---fold!
;; Let's, again, follow the same procedure: write some specific functions and
;; generalize from there. Here are two examples:

;; (sum-with-init result lst) -> number?
;;   result: number?
;;   lst: list? of numbers
;; Returns the sum of the numbers in lst, starting with result as the initial
;; value.
(define sum-with-init
  (lambda (result lst)
    (match lst
      [null result]
      [(cons head tail)
       (sum-with-init (+ head result) tail)])))

(test-case "sum-with-init empty"
           equal?
           22
           (lambda () (sum-with-init 22 null)))

(test-case "sum-with-init non-empty?"
           equal?
           50
           (lambda () (sum-with-init 11 (list 27 2 10))))

;; (cons-onto-backwards result lst) -> list?
;;   result: lst?
;;   lst: list?
;; Returns the result of consing lst onto the front of result backwards.
(define cons-onto-backwards
  (lambda (result lst)
    (match lst
      [null result]
      [(cons head tail)
       (cons-onto-backwards (cons head result) tail)])))

(test-case "cons-onto-backwards empty"
           equal?
           (list 1 2 3)
           (lambda () (cons-onto-backwards (list 1 2 3) null)))

(test-case "cons-onto-backwards non-empty?"
           equal?
           (list 7 6 5 4 1 2 3)
           (lambda () (cons-onto-backwards (list 1 2 3) (list 4 5 6 7))))

;; (Partner A drives!)
;;
;; Follow the style of these two functions to write a similar, third function
;; called string-append-backwards. (string-append-backwards result lst) takes
;; an initial string value, and a list of strings as input and returns the char->integer
;; strings of the list appended onto the front of the initial string in
;; backwards order. Note that the order of the individual characters in each
;; string is preserved, but they are appended in backwards order. For example:
;;
;; (string-append-backwards "abc" (list "def" "h" "gi"))
;; > "gihdefabc"
;; (string-append-backwards "abc" null)
;; > "abc"

;;; (string-append-backwards result lst) -> string?
;;;   result: string?
;;;   lst: list?, a list of strings
;;; Returns a string of the reversed list of strings with string result appended to the end.
(define string-append-backwards
  (lambda (result lst)
    (match lst
      [null result]
      [(cons head tail)
       (string-append-backwards (string-append head result) tail)])))

(test-case "string-append-backwards empty" 
           equal? 
           "abc" 
           (lambda () (string-append-backwards "abc" null)))

(test-case "string-append-backwards non-empty" 
           equal?
           "gihdefabc"
           (lambda () (string-append-backwards "abc" (list "def" "h" "gi"))))

;; (Partner B drives!)
;;
;; Like the previous problems, first identify what is shared and different
;; between these three functions:
;;
;; Shared: Add/append/wtv head to the initial value, and then add that to the recursion func of tail.
;;
;; Different: the types of inputs they take, the function they perform on the head, etc
;;
;; Again, check your work with a member of the course staff!
;;
;; Once you know the essential difference between these three functions, create
;; the list-foldl function that factors out this redundancy. list-foldl
;; should behave indentically to the foldl function when you are done!

;;; (list-foldl func v lst) -> any
;;;   func: procedure?, a function that takes two inputs
;;;   v: any
;;;   lst: list?
;;; Returns the result of accumulating the result of applying f to each element of l,
;;; starting with initial value v. 
(define list-foldl
  (lambda (func v lst)
    (match lst
      [null v]
      [(cons head tail)
       (list-foldl func (func v head) tail)])))

(test-case "fold sum-with-init empty"
           equal?
           22
           (lambda () (list-foldl (lambda (x y) (+ x y)) 22 null)))

(test-case "fold sum-with-init non-empty?"
           equal?
           50
           (lambda () (list-foldl (lambda (x y) (+ x y)) 11 (list 27 2 10))))

(test-case "fold cons-onto-backwards empty"
           equal?
           (list 1 2 3)
           (lambda () (list-foldl (lambda (x y) (cons y x)) (list 1 2 3) null)))

(test-case "fold cons-onto-backwards non-empty?"
           equal?
           (list 7 6 5 4 1 2 3)
           (lambda () (list-foldl (lambda (x y) (cons y x)) (list 1 2 3) (list 4 5 6 7))))

(test-case "fold string-append-backwards empty" 
           equal? 
           "abc" 
           (lambda () (list-foldl (lambda (x y) (string-append y x)) "abc" null)))

(test-case "fold string-append-backwards non-empty" 
           equal?
           "gihdefabc"
           (lambda () (list-foldl (lambda (x y) (string-append y x)) "abc" (list "def" "h" "gi"))))

;; ----------------------------
"Problem 4: Really, Reductions"
;; ----------------------------

;; (Partner A drives!)
;;
;; In our discussion of list transformations, rather than foldl, we introduced
;; reduce first! Reduce is similar to foldl but instead of providing an initial
;; value, we use the first element of the list as the initial value.
;;
;; Implement list-reduce below in terms of list-foldl. It should be
;; functionality identical to reduce when you are done!

;;; (list-reduce f l) -> any
;;;   f: procedure?, a function
;;;   l: list?
;;; Applying f to each element of l starting with the first element of l.
(define list-reduce
  (lambda (f l)
    (match l
      [null null]
      [(cons head tail)
       (list-reduce (f head) tail)])))

;; TODO: add tests here!

;; With implementations of list-foldl and list-reduce in hand, you should
;; be in a better position to now talk about when you would use foldl versus
;; reduce. Based on your implementation, give 2 reasons when you would choose
;; foldl versus reduce:
;;
;; <TODO: write down your three reasons here>
;; 1. ...
;; 2. ...