(define menu
  (list (pair (pair "Coffee" "Mocha") 3.5)
        (pair (pair "Chai" "Dirty Chai Latte") 2.5)
        (pair (pair "Milk" "Oat Milk no Sugar") 4)
        (pair (pair "Coffee" "Iced Latte") 3.8)
        (pair (pair "Chai" "Almond Milk Chai") 3.4)
        (pair (pair "Coffee" "Double Shot Cold Brew") 5)))
; Nested Functions
(define coffee-profit-1
  (apply +
    (map cdr 
      (filter (section string=? "Coffee" (car (car _))) menu))))
; Pipelining
(define coffee-profit-2
  (|> menu
    (lambda (lst) (filter (section string=? "Coffee" (car (car _))) lst))
    (lambda (lst) (map cdr lst))
    (lambda (lst) (apply + lst))))
; Compound
(define coffee?
  (section string=? "Coffee" (car (car _))))
(define coffee-profit-3
  (o (section apply + _) 
     (section map cdr _)
     (section filter coffee? _)))
(coffee-profit-3 menu)
; Pipelining without lambda
(define coffee-profit-4
  (|> menu
      (section filter coffee? _)
      (section map cdr _)
      (section apply + _)))