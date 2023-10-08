;; CSC-151-02 (Fall 23)
;; Mini-Project 3: Musical Copyright
;; Khanh Do
;; 2023-10-13
;; ACKNOWLEDGEMENTS:
;;   Following instructions on Mini-Project 4 page

(import lab)
(import test)
(import music)

(problem "Part 1: Cartesian Product")

"all-list2"
; To generate all the lists of size two where the first element is v and the second element
; is drawn from a list lst:

; When lst is empty, returns empty list
; When lst is non-empty, match list to pattern (cons head tail), then cons a list of a and the
; head to recursive all-list2 v tail.

;;; (all-list2 v) -> list?
;;;   v: any?
;;; Takes a value v and a list of values and creates a list of all possible lists of size 2
;;; where the first element is v and the second element is a value from lst.
(define all-list2
  (lambda (v lst)
    (match lst
      [null null]
      [(cons head tail) (cons (list v head) (all-list2 v tail))])))

(test-case "v is a string, lst of number" equal? 
                                          (list (list "q" 0) (list "q" 1) (list "q" 2) (list "q" 3) (list "q" 4))
                                          (lambda () (all-list2 "q" (range 5))))
(test-case "v is a character, lst of random" equal? 
                                             (list (list #\? "hello") (list #\? #\V) (list #\? 9))
                                             (lambda () (all-list2 #\? (list "hello" #\V 9))))
(test-case "lst has 2 elements" equal? 
                                (list (list "a" 2) (list "a" 4))
                                (lambda () (all-list2 "a" (list 2 4))))
(test-case "lst has 1 element" equal?
                               (list (list "b" 1))
                               (lambda () (all-list2 "b" (list 1))))
(test-case "base case" equal? null (lambda () (all-list2 "q" null)))

"cartesian-product"
; To form all pairs of values whose first component is drawn from l1
; and second component is drawn from l2:

; When l1 is empty, returns empty list.
; When l2 is non-empty, match pattern of l1 to (cons head tail), then append a list of 
; head and an element from l2 using all-list2, to recursive cartesian-product tail l2.

;;; (cartesian-product l1 l2) -> list?
;;;   l1: list?
;;;   l2: list?
;;; Produces the Cartesian Product of the elements drawn from l1, l2, which is a list that contains
;;; all the possible lists of size 2 (list x y), where x is from l1, y from l2.
(define cartesian-product
  (lambda (l1 l2)
    (match l1
      [null null]
      [(cons head tail) 
       (append (all-list2 head l2) (cartesian-product tail l2))])))

(test-case "correctly append 2 lists" equal?
                                      (list (list 0 "a") (list 0 "b")
                                            (list 1 "a") (list 1 "b")
                                            (list 2 "a") (list 2 "b")) 
                                      (lambda () (cartesian-product (range 3) (list "a" "b"))))
(test-case "list 1 has 1 element" equal?
                                  (list (list 1 0) (list 1 1) (list 1 2))
                                  (lambda () (cartesian-product (list 1) (range 3))))
(test-case "list 2 is null case" equal?
                                 null ; because all-list2 will match list 2 with null
                                 (lambda () (cartesian-product (range 3) null)))
(test-case "base case" equal? null (lambda () (cartesian-product null null)))

"all-two-note-songs"
;;; (all-two-note-songs notes) -> composition?
;;;   notes: list?
;;; Takes a list of notes of MIDI values and produces all the possible two-note songs. 
"all-two-note-songs"
;;; (all-two-note-songs notes) -> composition?
;;;   notes: list?
;;; Takes a list of notes of MIDI values and produces all the possible two-note songs. 
(define all-two-note-songs
  (lambda (notes)
    (|> notes
        (lambda (lst) (map (section note _ qn) lst))
        (lambda (lst) (cartesian-product lst lst))
        (lambda (lst) (map (section apply seq _) lst)))))

"two-note-example"
(define two-note-example 
  (all-two-note-songs (list 60 69)))

(problem "Part 2: All Combinations")

"cons-all"
; To generate a list of lists of x added to the front of every list:

; When lsts is empty, returns empty list.
; When lsts is non-empty, add x to the front of the list and append all the list together.

;;; (cons-all x lsts) -> list?
;;;   x: any?
;;;   lsts: list? 
;;; Takes a single value x and a list of lists, lsts, and returns lsts
;;; but with x added to the front of every list.
(define cons-all
  (lambda (x lsts)
    (match lsts
      [null null]
      [(cons head tail)
       (cons (cons x head) (cons-all x tail))])))

(test-case "cons 0 to list of lists" equal?
                                     (list (list 0 1 2)
                                           (list 0 3 4 5)
                                           (list 0 6 7))
                                     (lambda () (cons-all 0 (list (list 1 2)
                                                                  (list 3 4 5)
                                                                  (list 6 7)))))
                                                                  (test-case "lsts has one null list" equal? 
                                (list (list "a" #\!) (list "a"))
                                (lambda () (cons-all "a" (list (list #\!) (list)))))
(test-case "base case" equal? null (lambda () (cons-all 0 null)))

"combinations"
; To generate a list of lists of x added to the front of every list:

; When lsts is empty, returns empty list.
; When lsts is non-empty, 

;;; (combinations lsts) -> list?
;;;   lsts: list?, list of lists
;;; Returns a list of lists. For each list returned in the result the i-th element of the list 
;;; is drawn from i-th list of lsts. 

(test-case "list of many case" equal?
                               (list 
                                  (list 1 3 6) (list 1 3 7)
                                  (list 1 4 6) (list 1 4 7)
                                  (list 1 5 6) (list 1 5 7)
                                  (list 2 3 6) (list 2 3 7)
                                  (list 2 4 6) (list 2 4 7)
                                  (list 2 5 6) (list 2 5 7))
                               (lambda () (combinations (list (list 1 2)
                                                              (list 3 4 5)
                                                              (list 6 7)))))
(test-case "base case" equal? (list null) (lambda () (combinations null)))
(test-case "list of 2 lists" equal?
                            (list (list 0 "a") (list 0 "b")
                                  (list 1 "a") (list 1 "b")
                                  (list 2 "a") (list 2 "b"))
                            (lambda () (combinations (list (range 3)
                                                           (list "a" "b")))))
(test-case "list of null" equal? null (lambda () (combinations (list null null))))
