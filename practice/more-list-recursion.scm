;; CSC 151 (23fa))
;; Lab: More List Practice
;; Authors: Khanh Do, Sarah Qi
;; Date: 6 Oct
;; Acknowledgements:
;;   Following instruction on web

;; Like the previous lab, we want you to practice writing recursive
;; functions as much as possible! The first two problems are
;; required and the remainder are available for additional practice.
;; We recommend tackling as many of the problems with your partner
;; as possible!
;;
;; For each recursive function that you write, make sure to include
;; an appropriate recursive design, docstring, and test cases.

(import test)

;; ---------------------
"Problem 1: Intersperse"
;; ---------------------

;; (Partner A drives!)

;; In this problem, let's consider the problem of a writing a function
;; called intersperse. (intersperse sep lst) return the list lst but
;; with element sep interspersed between the elements of lst.
;;
;; > (intersperse 0 (list 3 8 7 2 1))
;; (list 3 0 8 0 7 0 2 0 1)
;; > (intersperse #\, (list #\a #\b #\c #\d #\e))
;; (list #\a #\, #\b #\, #\c #\, #\d #\, #\e)
;; > (intersperse 22 null)
;; null
;;
;; While seemingly straightforward, it turns out that our basic
;; recursive decomposition will fail us! Let's investigate where we
;; go wrong in this approach and how we can enhance our recursive
;; skeleton to solve this problem.
;;
;; (a) First, consider the following attempt at implementing
;; intersperse using our current technique:

(define intersperse-bad
  (lambda (sep lst)
    (match lst
      [null null]
      [(cons head tail)
       (cons head (cons sep (intersperse-bad sep tail)))])))

;; Translate this implementation back to a recursive decomposition by
;; filling out this template:
;;
;; To intersperse sep between the elements of lst:
;;
;; + When lst is empty, return an empty list.
;; + When lst is non-empty, pair the head with a pair of the sep and the recursive tail.

;; (b) Next, let's explore what goes wrong with this approach. Write
;; test cases below until you can describe what is wrong with
;; intersperse-bad.

(test-case "correctly intersperse sep in between - number case" equal? (list 3 0 8 0 7 0 2 0 1) (lambda () (intersperse-bad 0 (list 3 8 7 2 1))))
(test-case "correctly intersperse sep in between - character case" equal? (list #\a #\, #\b #\, #\c #\, #\d #\, #\e) (lambda () (intersperse-bad #\, (list #\a #\b #\c #\d #\e))))
(test-case "base case" equal? null (lambda () (intersperse-bad 22 null)))

;; It is adding sep after the last element, which is wrong.

;; (c) Ultimately, the problem you described above occurs because the way
;; that we decomposed the problem is not accurate! To understanding the
;; problem in more detail, trace execution of the following call to
;; intersperse-bad and use the exploration's window to check your work.
;; You may evaluate any given recursive function call directly to the
;; branch that the call would evaluate to in a single step.
;;
(intersperse-bad 0 (list 3 8 7 2 1))
; --> (match (3 8 7 2 1) ([() null] [(cons head tail) (cons head (cons 0 (intersperse-bad 0 tail)))]))
; --> (cons 3 (cons 0 (intersperse-bad 0 (8 7 2 1))))
; --> (cons 3 (cons 0 (match (8 7 2 1) ([() null] [(cons head tail) (cons head (cons 0 (intersperse-bad 0 tail)))]))))
; --> (cons 3 (cons 0 (cons 8 (cons 0 (intersperse-bad 0 (7 2 1))))))
; --> (cons 3 (cons 0 (cons 8 (cons 0 (match (7 2 1) ([() null] [(cons head tail) (cons head (cons 0 (intersperse-bad 0 tail)))]))))))
; --> (cons 3 (cons 0 (cons 8 (cons 0 (cons 7 (cons 0 (intersperse-bad 0 (2 1))))))))
; --> (cons 3 (cons 0 (cons 8 (cons 0 (cons 7 (cons 0 (match (2 1) ([() null] [(cons head tail) (cons head (cons 0 (intersperse-bad 0 tail)))]))))))))
; ...
; --> (cons 3 (list 0 8 0 7 0 2 0 1 0))
; --> (list 3 0 8 0 7 0 2 0 1 0)

;; (d) From your trace, you should observe that the way that
;; intersperse-bad decomposes the interspersed list is as follows:
;;
;; 3 0  8 0  7 0  2 0  1 0
;;
;; Where I have uses spaces the pairs of numbers that are consed onto 
;; the result on each recursive call. Each recursive call follows
;; the pattern of:
;;
;;   (cons head (cons sep ...))
;;
;; Resulting in, e.g., 3 0 being added together in the first recursive
;; call to the function. In this, light the problem is that the final
;; recursive call has an extraneous 0! To fix the problem, we only
;; need to ensure that we don't add an extra 0 in that last case. When
;; does that occur? That occurs when the list has exactly one element!
;;
;; This leads to one additional case to our recursive decomposition.
;; Complete the recursive decomposition for intersperse:
;;
;; To intersperse sep among the elements of lst:
;; + When the lst is empty returns null
;; + When the lst has one element returns lst
;; + When the lst has more than one element pair the head with a pair of the sep and the recursive tail.
;;
;; Observe how that even though we add an extra case, our cases do
;; not overlap---each possible list only applies to one of the
;; cases---and our cases are exhaustive---they cover all possible
;; lists. We can add additional cases if our recursive decomposition
;; demands it as long as our cases have these two properties!

;; (e) Implement intersperse correctly using this new recursive
;; decomposition. Translate the recursive decomposition precisely,
;; adding an additional branch for the new case.

;;; (intersperse sep lst) -> lst?
;;;   sep: any?
;;;   lst: list?
;;; Add sep in between elements of list.
(define intersperse
  (lambda (sep lst)
    (match lst
      [null null]
      [(cons head null) lst]
      [(cons head tail)
       (cons head (cons sep (intersperse sep tail)))])))
(intersperse #\, (list #\a #\b #\c #\d #\e))
(test-case "correctly intersperse sep in between - number case" equal? (list 3 0 8 0 7 0 2 0 1) (lambda () (intersperse 0 (list 3 8 7 2 1))))
(test-case "correctly intersperse sep in between - character case" equal? (list #\a #\, #\b #\, #\c #\, #\d #\, #\e) (lambda () (intersperse #\, (list #\a #\b #\c #\d #\e))))
(test-case "base case" equal? null (lambda () (intersperse 22 null)))

;; --------------------
"Problem 2: Dropzeroes"
;; --------------------

; (Partner B drives!)

;; Implement a function (dropzeroes lst) that takes a list of
;; numbers lst as input and returns lst but with all the zeroes of
;; lst removed.
;;
;; > (dropzeroes (list 1 3 0 8 0 0 1 0))
;; (list 1 3 8 1)
;; > (dropzeroes null)
;; null

;;; (dropzeroes lst) -> list?
;;;   lst: list?
;;; Takes a list of numbers lst as input and returns lst but with all the zeroes of lst removed. 
(define dropzeroes
  (lambda (lst)
    (match lst
      [null null]
      [(cons head tail) 
       (if (not (zero? head)) (cons head (dropzeroes tail)) (dropzeroes tail))])))

(test-case "drop the zero from the list" equal? (list 1 3 8 1) (lambda () (dropzeroes (list 1 3 0 8 0 0 1 0))))
(test-case "base case" equal? null (lambda () (dropzeroes null)))

;; --------------------
"Problem 3: Count True"
;; --------------------

; (Partner A drives!)

;; Implement a function (count-true lst) that takes a list of
;; booleans lst as input and returns the number of occurrences of
;; #t in lst.
;;
;; > (count-true (list #t #f #t #t #f #f)
;; 3
;; > (count-true null)
;; 0

;;; (count-true lst) -> interger?
;;;   lst: list?
;;; Takes a list of booleans lst as input and returns the number of occurrences of
;;; #t in lst.
(define count-true
  (lambda (lst)
    (match lst
      [null 0]
      [(cons head tail) 
       (if (equal? #t head) 
           (+ 1 (count-true tail)) 
           (count-true tail))])))

(test-case "correctly count true" equal? 3 (lambda () (count-true (list #t #f #t #t #f #f))))
(test-case "base case" equal? 0 (lambda () (count-true null)))

;; --------------------
"Problem 4: Telescope"
;; --------------------

; (Partner B drives!)

;; Implement a function (telescope lst) that takes a list of
;; numbers lst as input and returns lst but before each element n of
;; lst is the sequence of numbers from 0 up to n-1. If the number
;; non-positive, no additional numbers precede it in the output.
;;
;; > (telescope (list 3 5 -1 1 2))
;; (list 0 1 2 3 0 1 2 3 4 5 -1 0 1 0 1 2)
;; > (telescope null)
;; null

;;; (telescope lst) -> list?
;;;   lst: list?
;;; Takes a list of numbers and returns lst which add the sequence of numbers from 0 up to n-1 before each element n of the list.
(define telescope
  (lambda (lst)
    (match lst
      [null null]
      [(cons head tail) 
       (if (< head 0)
           (cons head (telescope tail))
           (append (range (+ 1 head)) (telescope tail))))])))

(test-case "correctly add number" equal? (list 0 1 2 3 0 1 2 3 4 5 -1 0 1 0 1 2) (lambda () (telescope (list 3 5 -1 1 2))))
(test-case "base case" equal? null (lambda () (telescope null)))
;; --------------------
"Problem 5: Remove All"
;; --------------------

;; Implement a function (remove-all v lst) that takes a value v
;; and list lst and returns lst but with all occurrences of v removed.
;;
;; > (remove-all 0 (list 1 0 0 8 7 0 9 6 3 0))
;; (list 1 8 7 9 6 3)
;; > (remove-all 0 null)
;; null

;;; (remove-all v lst) -> list?
;;;   v: any?
;;;   lst: list?
;;; Takes a value v and list lst and returns lst but with all occurrences of v removed.
(define remove-all
  (lambda (v lst)
    (match lst
      [null null]
      [(cons head tail) 
       (if (equal? v head)
           (remove-all v tail)
           (cons head (remove-all v tail)))])))

(test-case "remove number" equal? (list 1 8 7 9 6 3) (lambda () (remove-all 0 (list 1 0 0 8 7 0 9 6 3 0))))
(test-case "remove any" equal? (list #\N #\@ #\9 9 2) (lambda () (remove-all #\? (list #\? #\N #\@ #\? #\9 9 2 #\?))))
(test-case "base case" equal? null (lambda () (remove-all 0 null)))
;; --------------------
"Problem 6: Dedup"
;; --------------------

;; Using remove-all, implement a function (dedup lst) that returns
;; list lst but with all duplicate elements of the list removed.
;; The "left-most" copies of each unique value in lst should be
;; retained.
;;
;; > (dedup (list 1 3 1 8 7 4 7 8 0 0 1 2))
;; (list 1 3 8 7 4 0 2)
;; > (dedup null)
;; null

;;; (dedup lst) -> list?
;;;   lst: list?
;;; Returns list lst but with all duplicate elements of the list removed. The "left-most" copies of each unique value in lst should be retained.

(test-case "correctly remove dups" equal? (list 1 3 8 7 4 0 2) (lambda () (dedup (list 1 3 1 8 7 4 7 8 0 0 1 2))))
(test-case "base case" equal? null (lambda () (dedup null)))