"2.8 (Lambda-free anonymous procedures)"

;;; (correct-speeds lst) -> list?
;;;   lst: list?, of non-negative numbers
;;; Returns a list of correct kph speeds.
(define correct-speeds
  (section map (o (section * 1.609344 _) (section + 1.2 _)) _))

(correct-speeds (list  45 32 35.7 12 0)) ; Example