;; CSC-151-02 (Fall 23)
;; Mini-Project 2: Working with the basic datatypes
;; Khanh Do
;; 2023-09-14
;; ACKNOWLEDGEMENTS:
;;   Adapting instructions on the Mini Project 2 page.
;;   For part 2: Credit to Marty Stepp and Stuart Reges from the University
;;   of Washington for creating the original version of this assignment.

;; Part 1: String Utilities

;; i.

;;; (leading-space-remove str) -> string?
;;;   str : string?
;;; Returns an empty string if the input string is empty and removes
;;; one leading space of the input string, if it exists.
(define leading-space-remove
  (lambda (str)
    (if (null? (string->list str))
        ""
        (if (char-whitespace? (list-ref (string->list str) 0))
            (substring str 1 (length (string->list str)))
            str))))

;;; (reversed-1 str) -> string?
;;;   str : string?
;;; Returns a string with characters in reversed order.
(define reversed-1
  (o list->string reverse string->list))

;;; (reversed-2 str) -> string?
;;;   str : string?
;;; Returns a reversed string that has its leading space removed.
(define reversed-2 
  (lambda (str)
    (reversed-1 (leading-space-remove str))))

;;; (slight-trim str) -> string?
;;;   str : string?
;;; Returns string str without a single leading space and a single 
;;; trailing space on the ends of str, if they exist.
(define slight-trim 
  (lambda (str)
    (|> str reversed-2 reversed-2)))

;;; Tests
(slight-trim " Hello World! ") ; #"Hello World!"
(slight-trim "Hello World! ")  ; #"Hello World!"
(slight-trim "  Hello")        ; #" Hello"
(slight-trim "  ")             ; #""
(slight-trim "")               ; #""

;; ii.
;;; (starts-with? s1 s2) -> boolean?
;;;   s1 : string?
;;;   s2 : string?
;;; Determines whether string s1 starts with string s2
(define starts-with?
  (lambda (s1 s2)
    (and (not (< (string-length s1) (string-length s2)))
         (equal? 
           (substring s1 0 (string-length s2)) 
           s2))))

;;; Tests
(starts-with? "abcde" "abc")  ; #t
(starts-with? "abcd" "abcd")  ; #t
(starts-with? "abcd" "abcde") ; #f
(starts-with? "hi" "Hi")      ; #f
(starts-with? "" "")          ; #t
(starts-with? "@@@" "@@")     ; #t

;; iii.
;;; (ends-with? s1 s2) -> boolean?
;;;   s1 : string?
;;;   s2 : string?
;;; Determines whether string s1 ends with string s2.
(define ends-with?
  (lambda (s1 s2)
    (starts-with? (reversed-1 s1) (reversed-1 s2))))

;;; Tests
(ends-with? "abcd" "bcd")      ; #t
(ends-with? "abcd" "abcd")     ; #t
(ends-with? "abcd" "eabcd")    ; #f
(ends-with? "0988" "88")       ; #t
(ends-with? "ThI" "hi")        ; #f
(ends-with? "" "")             ; #t
(ends-with? "Hello World!" "") ; #t
(ends-with? "@*@" "*@")        ; #t

;; iv. 
;;; (all-digits? str) -> boolean?
;;;   str : string?
;;; Determines whether the string str contains only digits or not.
(define all-digits?
  (lambda (str)
    (and (not (null? (string->list str)))
         (equal? 
           (filter char-numeric? (string->list str)) 
           (string->list str)))))                    

;;; Tests
(all-digits? "12345454") ; #t
(all-digits? "9")        ; #t
(all-digits? "@12")      ; #f
(all-digits? "")         ; #f
(all-digits? "1 267 3")  ; #f
(all-digits? "C1234")    ; #f