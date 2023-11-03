;; CSC-151-02 (Fall 23)
;; Mini-Project 5: Visual and Musical Fractals
;; Khanh Do
;; 2023-11-6
;; ACKNOWLEDGEMENTS:
;;   Adapting instructions on the Mini Project 5 page.

(import lab)
(import test)
(import music)
(import image)

(part "Part 1: Visual Fractals")

(problem "Cantor Set")
;;; (cantor-set width height fill color n) -> drawing?
;;;   width: integer?, non-negative
;;;   height: integer?, non-negative
;;;   fill: string?, either "solid" or "outline"
;;;   color: string?
;;;   n: integer?, non-negative
;;; Draws a Cantor Set of depth n with the given visual properties. width 
;;; and height are the dimensions of a single bar.
(define cantor-set
  (lambda (width height fill color n)
    (let ([bar (rectangle width height fill color)])
    (match n
      [0 null]
      [1 bar]
      [_ (above 
           bar
           (rectangle width height "solid" "transparent")
           (beside 
             (cantor-set (/ width 3) height fill color (- n 1))
             (rectangle (/ width 3) height "solid" "transparent")
             (cantor-set (/ width 3) height fill color (- n 1))))]))))

"n = 1"
(cantor-set 100 10 "solid" "green" 1)
"n = 2"
(cantor-set 100 10 "outline" "blue" 2)
"n = 3"
(cantor-set 100 10 "outline" "purple" 3)
"n = 10"
(cantor-set 100 10 "solid" "black" 10)

(problem "Serpiński Carpet")

;;; (serpinski-carpet length fill color n) -> drawing?
;;;   length: integer?, non-negative
;;;   fill: string?, either "solid" or "outline"
;;;   color: string?
;;;   n: integer?, non-negative
;;; Draws a Serpinski Carpet of depth n with the given visual properties.
(define serpinski-carpet
  (lambda (length fill color n)
    (let ([background (square length "solid" "white")]
          [box (square (/ length 3) fill color)])
      (match n
        [0 background]
        [1 (overlay 
             box 
             background)]
        [_ (let* ([small-box (overlay 
                              (serpinski-carpet (/ length 3) fill color (- n 1))
                              (square (/ length 3) "solid" "white"))]
                  [top-bot-row (beside small-box small-box small-box)]
                  [mid-row (beside small-box box small-box)])
             (above top-bot-row mid-row top-bot-row))]))))

"n = 1"
(serpinski-carpet 100 "solid" "blue" 1)
"n = 2"
(serpinski-carpet 100 "outline" "green" 2)
"n = 3"
(serpinski-carpet 100 "solid" "black" 3)
"n = 5"
(serpinski-carpet 300 "solid" "purple" 5)

(problem "My Fractal")

;;; (diamond length color) -> drawing?
;;;   length: integer?, non-negative
;;;   color: string?
;;; Draws a rotated 45 degrees square to make a diamond.
(define diamond
  (lambda (length color) 
    (path length                           ; horizontal image size
          length                           ; vertical image size
          (list (pair (/ length 2) 0)      ; top point
                (pair length (/ length 2)) ; far right point
                (pair (/ length 2) length) ; bottom point
                (pair 0 (/ length 2))      ; far left point
                (pair (/ length 2) 0))     ; top point (need to return)
          "outline"                        ; fill style
          color)))                         ; color

;;; (my-fractal length fill color n) -> drawing?
;;;   length: integer?, non-negative
;;;   color: string?
;;;   n: integer?, non-negative
;;; Draws fractal kolam with the given visual properties.
(define my-fractal
  (lambda (length color n)
    (match n
      [0 null]
      [1 (diamond length color)]
      [_ (let* ([make-diamond (my-fractal (/ length 3) color (- n 1))]
                [space (square (/ length 3) "solid" "white")]
                [top-bot-row (beside space make-diamond space)]
                [mid-row (beside make-diamond make-diamond make-diamond)])
           (above top-bot-row mid-row top-bot-row))])))