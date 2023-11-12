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

(define demo (create-cipher 53))

(define example-cipher 
  (vector 46 9 15 16 28 19 14 37 44 11 47 8 6 23 4 40 50 49 2 26 13 7 36 27 31 24 51 35 29 34 42 20 38 45 17 1 5 3 52 30 48 41 10 39 12 18 33 0 25 43 21 22 32))

example-cipher

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

;;; (encipher-single-char ch cipher alphabet) -> character?
;;;   ch: character?
;;;   cipher: vector?
;;;   alphabet: list?
;;; Takes as input a character ch with the cipher and alphabet, and returns the enciphered
;;; character that will replace the original in the enciphered message. If the input 
;;; character is not in the alphabet, leave it unchanged (return the same ch as was input).
(define encipher-single-char
  (lambda (ch cipher alphabet)
    (if (list-contains alphabet ch)
        (list-ref alphabet (vector-ref cipher (index-of alphabet ch)))
        ch)))

(description "Testing")

(define test-alphabet (string->list "abcde"))
(define test-cipher (vector 3 1 0 4 2))

(test-case "a->d"
  equal? 
  #\d
  (lambda () (encipher-single-char #\a 
                                   test-cipher
                                   test-alphabet)))
(test-case "b->b"
  equal? 
  #\b
  (lambda () (encipher-single-char #\b 
                                   test-cipher
                                   test-alphabet)))
(test-case "c->a"
  equal? 
  #\a
  (lambda () (encipher-single-char #\c 
                                   test-cipher
                                   test-alphabet)))
(test-case "d->e"
  equal? 
  #\e
  (lambda () (encipher-single-char #\d 
                                   test-cipher
                                   test-alphabet)))
(test-case "e->c"
  equal? 
  #\c
  (lambda () (encipher-single-char #\e 
                                   test-cipher
                                   test-alphabet)))
(test-case "ot in alphabet"
  equal? 
  #\?
  (lambda () (encipher-single-char #\? 
                                   test-cipher
                                   test-alphabet)))

(problem "Encipher")

;;; (encipher str cipher alphabet) -> string?
;;;   str: string?
;;;   cipher: vector?
;;;   alphabet: list?
;;; Takes a string str as input, along with the cipher and alphabet, and
;;; uses vectors to create and return the enciphered string.
(define encipher
  (lambda (str cipher alphabet)
    (|> str
        (section string->vector _)
        (lambda (vec) 
          (vector-map 
            (section encipher-single-char _ cipher alphabet) 
            vec))
        (section vector->string _))))

(description "Testing")

(test-case "both char in and not in alphabet"
  equal?
  "ahceu"
  (lambda () (encipher "chedu" test-cipher test-alphabet)))

(test-case "all 5 in alphabet"
  equal?
  "ceabd"
  (lambda () (encipher "edcba" test-cipher test-alphabet)))

(test-case "none in alphabet"
  equal?
  "quizz"
  (lambda () (encipher "quizz" test-cipher test-alphabet)))

(test-case "str length != alphabet length"
  equal?
  "hcllo worle!"
  (lambda () (encipher "hello world!" test-cipher test-alphabet)))

(test-case "str=alphabet"
  equal?
  "dbaec"
  (lambda () (encipher "abcde" test-cipher test-alphabet)))

(test-exn "invalid input"
  (lambda () (encipher (vector #\a #\b #\c #\d #\e) test-cipher test-alphabet)))

(description "Demo")

(define message "I found the letters you wrote to him as a child. Just because something is not perfect does not make it any less worthy of love. Your father made you believe otherwise. But he was wrong.")
"message"
message

(define enciphered-message 
  (encipher message example-cipher example-alphabet))
"enciphered-message"
enciphered-message