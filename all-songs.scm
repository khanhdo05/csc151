;; CSC-151-02 (Fall 23)
;; Mini-Project 3: Musical Copyright
;; Khanh Do
;; 2023-10-13
;; ACKNOWLEDGEMENTS:
;;   Following instructions on Mini-Project 4 page

(import lab)
(import test)

(problem "Part 1: Cartesian Product")

"all-list2"

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
;;; (cartesian-product l1 l2) -> list?
;;;   l1: list?
;;;   l2: list?
;;; Produces the Cartesian Product of the elements drawn from l1, l2, which is a list that contains
;;; all the possible lists of size 2 (list x y), where x is from l1, y from l2.