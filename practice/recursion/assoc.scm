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
