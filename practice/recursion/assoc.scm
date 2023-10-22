; Self-Check (‡)

(import test)

; Let’s consider a specialized version of this update functionality in terms
; of our inventory. Write a function (assoc-update-inc-by d k n) that returns
; a new dictionary that is d but updates key-value pair (pair k v) of d to be
; (k (+ v n)). That is, we add n to the value associated with k in d.