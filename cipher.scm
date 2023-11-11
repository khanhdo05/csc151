;; CSC-151-02 (Fall 23)
;; Mini-Project 6: Cracking a Cipher
;; Khanh Do
;; 2023-11-6
;; ACKNOWLEDGEMENTS: swap!, shuffle-helper!, shuffle! functions are provided on the 
;; Mini-project 6 page.

(import lab)
(import test)

;---------------------------------------------
(part "Part 1. Creating Substitution Ciphers")
;---------------------------------------------

(define example-alphabet (string->list "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ "))

;;; (swap! vec i j) -> void
;;;   vec : vector?
;;;   i : integer?
;;;   j : integer?
;;; Swaps the ith and jth entries in the provided vector vec.
(define swap!
  (lambda (vec i j)
    (let ([temp (vector-ref vec i)])
      (begin
        (vector-set! vec i (vector-ref vec j))
        (vector-set! vec j temp)))))

;;; (shuffle-helper! vec n) -> void
;;;   vec : vector?
;;;   n : integer?
;;; Recursively selects the next random element and swaps it into location n.
(define shuffle-helper!
  (lambda (vec n)
    (if (zero? n)
      void
      (begin
        (swap! vec (random (+ n 1)) n)
        (shuffle-helper! vec (- n 1))
        void))))

;;; (shuffle! vec) -> void
;;;   vec : vector?
;;; Randomly shuffles the elements of the input vector.
(define shuffle!
  (lambda (vec)
    (shuffle-helper! vec ( - (vector-length vec) 1))))

(problem "Create Cipher")

;;; (create-cipher n) -> vector?
;;;   n: integer?
;;; Returns a cipher of length n.
(define create-cipher
  (lambda (n)
    (let ([vec (vector-range n)])
      (begin
        (shuffle! vec)
        vec))))

(description "Testing")
(create-cipher 7)
(create-cipher 2)
(create-cipher 20)

(description "Demo")
(define demo (create-cipher 5))
demo

;---------------------------
(part "Part 2: Enciphering")
;---------------------------

;;; (list-contains lst val) -> boolean?
;;;   lst : list?
;;;   val : any
;;; Returns #t if and only if the list contains the value.
(define list-contains
  (lambda (lst val)
    (match lst
      [null #f]
      [(cons head tail) (or (equal? head val) (list-contains tail val))])))