; Self-Check (‡)

(import test)

; Let’s consider a specialized version of this update functionality in terms
; of our inventory. Write a function (assoc-update-inc-by d k n) that returns
; a new dictionary that is d but updates key-value pair (pair k v) of d to be
; (k (+ v n)). That is, we add n to the value associated with k in d.

(define inventory (list (pair "apples" 5) (pair "bananas" 2) (pair "oranges" 8)))

;;; (assoc-update-inc-by d k n) -> list?
;;;   d: list?
;;;   k: any
;;;   n: any
;;; Returns dictionary d but updates key-value pair (pair k v) of d to be (k (+ v n))
(define assoc-update-inc-by
  (lambda (d k n)
    (match d
      [null (list (cons k n))]
      [(cons (pair key v) tail)
       (if (equal? k key) (assoc-set k (+ v n) d) (assoc-update-inc-by tail k n))])))

(test-case "update 'apples' -2" 
           equal? 
           (list (cons "apples" 3) (cons "bananas" 2) (cons "oranges" 8))
           (lambda () (assoc-update-inc-by inventory "apples" -2)))

(test-case "base case" 
           equal? 
           (list (pair "apples" 3)) 
           (lambda () (assoc-update-inc-by null "apples" 3)))

(test-case "k is not in d should produce wrong output"
           equal?
           (list (pair "mango" 3))
           (lambda () (assoc-update-inc-by inventory "mango" 3)))

(test-exn "wrong input for d" (lambda () (assoc-update-inc-by 4 "hello" "world")))

;; -------------------------------
"Problem 1: Guilty by association"
;; -------------------------------

;; (Partner A drives!)
;;
;; Recall the set up for an association list:
;;
;; + An association list is a set of pairs.
;; + Each pair contains a key and a value.
;; + The key is used to "look up" its associated value in the list.
;;
;; For this first problem, we'll use an association list to maintain a roster
;; of students---mad scientists---and their attendance in class at mad
;; scientist univeristy. Complete the definition roster below as an
;; association list that makes the test cases below pass.

(define example-roster
  (list (pair "Prof. Froth" 5)
        (pair "Dr. Spark" 5)
        (pair "Prof. Afterthought" 2)
        (pair "Dr. Agon" 3)
        (pair "Dr. Putrid" 5)))

; Mad scientist names generated from Fantasy Name Generator:
; https://www.fantasynamegenerators.com/mad-scientist-names.php

(test-case "Prof. Froth is in the class"
  equal? #t (lambda () (assoc-key? "Prof. Froth" example-roster)))
(test-case "Prof. Pride is not in the class"
  equal? #f (lambda () (assoc-key? "Prof. Pride" example-roster)))

(test-case "Dr. Spark's attendance"
  = 5 (lambda () (assoc-ref "Dr. Spark" example-roster)))
(test-case "Prof. Froth's attendance"
  = 5 (lambda () (assoc-ref "Prof. Froth" example-roster)))
(test-case "Prof. Afterthought's attendance"
  = 2 (lambda () (assoc-ref "Prof. Afterthought" example-roster)))
(test-case "Dr. Agon's attendance"
  = 3 (lambda () (assoc-ref "Dr. Agon" example-roster)))
(test-case "Dr. Putrid's attendence"
  = 5 (lambda () (assoc-ref "Dr. Putrid" example-roster)))

;; The test cases demonstrate the use of two of the three association list
;; functions provided in the standard library: assoc-key? and assoc-ref.
;; Frequently, we'll use assoc-key? to first check if a key is in the
;; association list. If so, we know that we can then use assoc-ref to
;; safely pull out its corresponding value.
;;
;; Use these functions to implement the following function and give
;; appropriate test cases for the function using example-roster above.

;; (attended-all? student num-classes roster) -> boolean?
;;   student: string?
;;   num-classes: integer?, a non-negative value
;;   roster: list?, an association list of students and their attendance.
;; Returns #t if and only if student is in the roster and if they have
;; attended all of the classes this semester (represented by num-classes).
(define attended-all?
  (lambda (student num-classes roster)
    (and (assoc-key? student roster) (= (assoc-ref student roster) num-classes))))

(test-case "Dr. Spark's attendance"
  equal? #t (lambda () (attended-all? "Dr. Spark" 5 example-roster)))
(test-case "Prof. Froth's attendance"
  equal? #t (lambda () (attended-all?  "Prof. Froth" 5 example-roster)))
(test-case "Prof. Afterthought's attendance"
  equal? #f (lambda () (attended-all?  "Prof. Afterthought" 5 example-roster)))
(test-case "Dr. Agon's attendance"
  equal? #f (lambda () (attended-all?  "Dr. Agon" 5 example-roster)))
(test-case "Dr. Putrid's attendence"
  equal? #t (lambda () (attended-all?  "Dr. Putrid" 5 example-roster)))

;; ------------------
"Problem 2: Tracking"
;; ------------------

;; (Partner B drives!)

;; Finally, let's look at assoc-set. (assoc-set k v l) returns a new
;; association list that is identical to l, except that key k is now
;; associated with value v. Use assoc-set to implement the following function
;; over our class roster and give appropriate test cases using example-roster
;; from the previous problem.

;; (update-student student roster) -> list?
;;   student: string?
;;   roster: list?, an assoc. list of students and their attendance.
;; Returns a new association list that is roster except updated so that
;; the given student is attending one more class than before. If the student
;; is not yet in the roster, then they should now have an attendance of one.
(define update-student
  (lambda (student roster)
    (if (assoc-key? student roster)
        (assoc-set student (+ 1 (assoc-ref student roster)) roster)
        (assoc-set student 1 roster))))

(test-case "Prof. Afterthought's updated attendance"
  equal? 
  (list (pair "Prof. Froth" 5) 
        (pair "Dr. Spark" 5) 
        (pair "Prof. Afterthought" 3) 
        (pair "Dr. Agon" 3) 
        (pair "Dr. Putrid" 5)) 
  (lambda () (update-student "Prof. Afterthought" example-roster)))

(test-case "Prof. Pride's first attendance"
  equal?
  (list (pair "Prof. Froth" 5) 
        (pair "Dr. Spark" 5) 
        (pair "Prof. Afterthought" 2) 
        (pair "Dr. Agon" 3) 
        (pair "Dr. Putrid" 5)
        (pair "Prof. Pride" 1))  
  (lambda () (update-student "Prof. Pride" example-roster)))

;; --------------------------
"Problem 3: Letter Inventory"
;; --------------------------

;; Now, let's turn our attention towards an application of dictionaries to
;; analyzing written texts: counting letters. While seemingly innoculous,
;; the frequency of letters is not arbitrary---some letters are more common
;; than others! Let's use an association list to be able to compute frequency
;; counts of letters in a text to see how accurate this claim is. In our
;; association list, keys will consist of the 26 lowercase characters and
;; their associated values will be the number of times the character has
;; occurred in the text.

(define empty-inventory
  (let ([make-k (section map integer->char _)]
        [make-pair (section cons _ 0)])
    (map make-pair (make-k (range 97 122)))))

(test-case "No 'a's in the empty inventory"
  = 0 (lambda () (assoc-ref #\a empty-inventory)))

(test-case "No 'm's in the empty inventory, too"
  = 0 (lambda () (assoc-ref #\m empty-inventory)))

;;; (update-inventory inv ch) -> list?
;;;   inv: list?
;;;   ch: char?
;;; Takes an existing letter inventory inv and a single character ch as input. If
;;; ch is a character (either lowercase or uppercase), then returns inv with updated
;;; value. If otherwise, return unmodified inv.
(define update-inventory
  (lambda (inv ch)
    (let ([ch_down (char-downcase ch)])
      (if (assoc-key? ch_down inv)
          (assoc-set ch_down (+ 1 (assoc-ref ch_down inv)) inv)
          inv))))
