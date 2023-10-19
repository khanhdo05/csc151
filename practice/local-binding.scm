;;; (dist p1 p2) -> number?, non-negative
;;;   p1: pair?, of 2 numbers
;;;   p2: pair?, of 2 numbers
;;; Takes in the coordinates of point 1 and point 2 in the form of
;;; (pair x y), and returns the Euclidean distance between them.
(define dist
  (lambda (p1 p2)
    (let ([diff-x (- (car p1) (car p2))]
          [diff-y (- (cdr p1) (cdr p2))]
          [square-diff (section expt _ 2)])
      (sqrt (+ (square-diff diff-x)
               (square-diff diff-y))))))

; Example: Distance between point 1 (10,90) and point 2 (5,9)
(dist (pair 10 90) (pair 5 9)) 