"2.5 (List Recursion)"

;;; (sanitize lst) -> list?
;;;   lst: list?, of characters
;;; Takes a list of characters from a string and returns a new list containing
;;; the alphabetic characters of the sanitized string.
(define sanitize
  (lambda (lst)
    (match lst
      [null null]
      [(cons head tail)
       (if (char-alphabetic? head)
           (cons (char-downcase head) (sanitize tail))
           (sanitize tail))])))

(list->string (sanitize (string->list "A test for y'all.")))
"atestforyall"

(list->string (sanitize (string->list "Test Case #2: with special chars and numbers too!")))
"testcasewithspecialcharsandnumberstoo"