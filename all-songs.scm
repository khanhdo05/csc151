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

(test-case "v is a string, lst of number" equal? 
                                          (list (list "q" 0) (list "q" 1) (list "q" 2) (list "q" 3) (list "q" 4))
                                          (lambda () (all-list2 "q" (range 5))))
(test-case "base case" equal? null (lambda () (all-list2 "q" null)))