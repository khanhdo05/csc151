;; CSC-151-02 (Fall 23)
;; Mini-Project 2: Working with the basic datatypes
;; Khanh Do
;; 2023-09-14
;; ACKNOWLEDGEMENTS:
;;   Adapting instructions on the Mini Project 2 page.
;;   For part 2: Credit to Marty Stepp and Stuart Reges from the University
;;   of Washington for creating the original version of this assignment.

(import image)

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

;; Part 2. Ehrenstein Illusions

;;; (box length box-color outline-color)
;;;   length : number?
;;;   box-color : string?, either a color name or the form "rgba(r g b a)"
;;;   outline-color : string?, either a color name or the form "rgba(r, g, b, a)"
;;; Creates an image that is a square with side length length, with the given
;;; box-color and outline-color.
(define box
  (lambda (length box-color)
    (square length "solid" box-color))) 

;;; (diamond x) -> drawing?
;;;   x : number?
;;; Creates an image of a diamond.
;;; Code adapted from Eric Autry's example code.
(define diamond
  (lambda (length outline-color) 
    (path length                           ; horizontal image size
          length                           ; vertical image size
          (list (pair (/ length 2) 0)      ; top point
                (pair length (/ length 2)) ; far right point
                (pair (/ length 2) length) ; bottom point
                (pair 0 (/ length 2))      ; far left point
                (pair (/ length 2) 0))     ; top point (need to return)
          "outline"                        ; fill style
          outline-color)))                 ; color

;;; (a-circle radius outline-color circ-color) -> drawing?
;;;   radius : number?
;;;   outline-color : string?, either a color name or the form "rgba(r, g, b, a)"
;;;   circ-color : string?, either a color name or the form "rgba(r g b a)"
;;; Creates a circle with given radius, outline color, and fill color.
(define a-circle 
  (lambda (radius outline-color circ-color) 
    (overlay (circle radius "outline" outline-color) ; outline
             (circle radius "solid" circ-color))))   ; fill

;;; (radius length n) -> list?
;;;   length : number?
;;;   n : number?
;;; Creates a list of numbers representing the corresponding radius of
;;; each circle.
(define radius
  (lambda (length n) 
    (map (section * (/ (/ length 2) n) _) ; because radius = length box / 2
         (range 1 (+ n 1)))))

;;; (circles length n outline-color circ-color) -> drawing?
;;;   length : number?
;;;   n : number?
;;;   outline-color : string?, either a color name or the form "rgba(r, g, b, a)"
;;;   circ-color : string?, either a color name or the form "rgba(r g b a)"
;;; Creates concentric circles of n number of circles with the outer-most
;;; circle of radius half of length, and given outline color and fill color.
(define circles
  (lambda (length n outline-color circ-color)
    (apply 
      overlay                                              
        (map (section a-circle _ outline-color circ-color)  
             (radius length n)))))

;;; (ehrenstein length n box-color circ-color outline-color) -> drawing?
;;;   length : number?
;;;   n : number?
;;;   box-color : string?, either a color name or the form "rgba(r g b a)"
;;;   circ-color : string?, either a color name or the form "rgba(r g b a)"
;;;   outline-color : string?, either a color name or the form "rgba(r, g, b, a)"
;;; Creates an image that contains a single Ehrenstein illusion with side
;;; length length, n circles, with the given box-color and circ-color for 
;;; the box color and circle color, respectively. Outline-color determines
;;; the color of the outline of the circles and the diamond.
(define ehrenstein 
  (lambda (length n box-color circ-color outline-color)
    (overlay (diamond length outline-color)                           
             (circles length n outline-color circ-color) 
             (box length box-color))))

;;; (col m img) -> drawing?
;;;   m : number?
;;;   img : drawing?
;;; Creates an image of m number of the input image above each other.
(define col
  (lambda (m img) 
    (apply above (make-list m img))))

;;; (grid m n img) -> drawing?
;;;   img : drawing?
;;;   m : number?
;;;   n : number?
;;; Creates an image that is a grid of m rows and n columns
;;; of the provided image.
(define grid 
  (lambda (m n img) 
    (apply
      beside
        (make-list n (col m img)))))

;;; ehrenstein-1: a single Ehrenstein illusion of length 200,
;;; 5 circles, a "red" box, "yellow" circles, and "black" outline.
(define ehrenstein-1
  (ehrenstein 200 5 "red" "yellow" "black"))

"ehrenstein-1"
ehrenstein-1

;;; ehrenstein-2: a single Ehrenstein illusion of length 100, 10
;;; circles, an "aqua" box, "orange" circles, and "black" outline.
(define ehrenstein-2
  (ehrenstein 100 10 "aqua" "orange" "black"))

"ehrenstein-2"
ehrenstein-2

;;; ehrenstein-3: : a single Ehrenstein illusion of length 50,
;;; no circles, a "white" box and circle, and "green" outline.
(define ehrenstein-3
  (ehrenstein 50 0 "white" "transparent" "green"))

"ehrenstein-3"
ehrenstein-3

;;; ehrenstein-4: a 3x3 grid of Ehrenstein illusions of length
;;; 100, 10 circles each, and a "red" box, "yellow" circle, and
;;; "orange" outline. 
(define ehrenstein-4
  (grid 3 3 (ehrenstein 100 10 "red" "yellow" "orange")))

"ehrenstein-4"
ehrenstein-4

;;; ehrenstein-5: a 3x2 grid of Ehrenstein illusions of length 
;;; 50, 5 circles each, and a "blue" box, "green" circles, and
;;; "white" outline.
(define ehrenstein-5
  (grid 3 2 (ehrenstein 50 5 "blue" "green" "white")))

"ehrenstein-5"
ehrenstein-5