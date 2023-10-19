;;; (dc-check n mod dc) -> boolean?
;;;   n: integer?
;;;   mod: integer?
;;;   dc: integer?
(define dc-check
  (lambda (n mod dc)
    (let ([roll (+ 1 (random n))]) ; + 1 to random n to correctly represent 1, 2, 3, ..., n
      (>= (+ roll mod) dc))))                   

(dc-check 2 1 3) ; Indeed returns unpredictable output