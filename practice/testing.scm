"2.3 (Testing)"

(import test)

(test-case "zip two non-empty lists"
           equal?
           (list (pair 1 4)
                 (pair 2 5)
                 (pair 3 6))
           (lambda () (zip (list 1 2 3) (list 4 5 6))))

(test-case "zip two 1-element lists"
           equal?
           (list (pair 1 2))
           (lambda () (zip (list 1) (list 2))))

(test-case "zip two lists of strings"
           equal?
           (list (pair "hello" "world")
                 (pair "father" "mother")
                 (pair "bother" "sister"))
           (lambda () (zip (list "hello" "father" "brother") 
                           (list "world" "mother" "sister"))))

(test-case "zip two lists of null"
           equal?
           (list (pair null null)) ; (pair null null) is (list null) -> (list (list null))
           (lambda () (zip (list null) (list null)))) ; ith here is index 0

(test-exn "zip two empty lists, expected ERROR"
           (lambda () (zip null null)))

(test-exn "zip two lists of different length, expected ERROR"
          (lambda () (zip (list #/ #R) (list 3 4 5 6))))