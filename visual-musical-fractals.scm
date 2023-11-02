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
        [_ (let* ([make-box (overlay 
                              (serpinski-carpet (/ length 3) fill color (- n 1))
                              (square (/ length 3) "solid" "white"))]
                  [top-bot-row (beside make-box make-box make-box)]
                  [mid-row (beside make-box box make-box)])
             (overlay 
                (above top-bot-row mid-row top-bot-row)
                background))]))))