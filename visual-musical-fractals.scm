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

;--------------------------------
(part "Part 1: Visual Fractals")
;--------------------------------

;_______________________________________________________________
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

;_______________________________________________________________
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

;_______________________________________________________________
(problem "My Fractal")

;;; (my-fractal-helper length color n) -> drawing?
;;;   length: integer?, non-negative
;;;   color: string?
;;;   n: integer?, non-negative
;;; Draws fractal kolam with the given visual properties.
(define my-fractal-helper
  (lambda (length color n)
    (match n
      [0 null]
      [1 (diamond length color)]
      [_ (let* ([make-diamond (my-fractal-helper (/ length 3) color (- n 1))]
                [space (square (/ length 3) "solid" "white")]
                [top-bot-row (beside space make-diamond space)]
                [mid-row (beside make-diamond make-diamond make-diamond)])
           (above top-bot-row mid-row top-bot-row))])))

;;; (my-fractal length color1 fill color2 n) -> drawing?
;;;   length: integer?, non-negative
;;;   color1: string?
;;;   fill: string? either "solid" or "outline"
;;;   color2: string?
;;;   n: integer?, non-negative
;;; Draws fractal image with the given visual properties (kolam on top of serpinski).
(define my-fractal
  (lambda (length color1 fill color2 n)
    (overlay
      (my-fractal-helper length color1 n)
      (serpinski-carpet length fill color2 n))))

"n = 1"
(my-fractal 100 "blue" "solid" "blue" 1)
"n = 2"
(my-fractal 100 "black" "solid" "purple" 2)
"n = 3"
(my-fractal 200 "red" "outline" "yellow" 3)
"n = 4"
(my-fractal 300 "black" "solid" "blue" 4)
"n = 5"
(my-fractal 400 "green" "solid" "transparent" 6)

;--------------------------------
(part "Part 2: Musical Fractal")
;--------------------------------

; Helper func
;;; (div d n) -> duration?
;;;   d: duration?
;;;   n: integer?, non-negative
;;; Takes a duration d and returns that duration but divided by n.
(define div
  (lambda (d n)
    (dur (/ (numerator d) n) (/ (denominator d) n))))

;_______________________________________________________________
(problem "Dominoes")

;;; (dominoes freq d n) -> composition?
;;;   freq: integer?, 0 <= freq <= 4000
;;;   d: duration?
;;;   n: integer?, non-negative
;;; 
(define dominoes
  (lambda (freq d n)
    (match n
      [0 empty]
      [_ (seq 
           (note-freq freq d)
           (dominoes (/ freq 2) (div d 2) (- n 1)))])))

(dominoes 150 wn 3)

;_______________________________________________________________
(problem "Raindrops")

;;; (raindrops lo hi d n) -> composition?
;;;   lo: frequency? 0 <= lo <= 4000, lo < hi
;;;   hi: frequency? 0 <= hi <= 4000, hi > lo
;;;   d: duration?
;;;   n: integer?, non-negative
;;; 
(define raindrops
  (lambda (lo hi d n)
    (let ([new-d (div d 3)])
      (match n
        [0 (seq 
             (note-freq lo new-d)
             (note-freq hi new-d)
             (note-freq (/ (+ hi lo) 2) new-d))]
        [_ (let* ([jump (/ (- 500 100) 4)]
                  [1qrt (+ lo jump)]
                  [mid (+ 1qrt jump)]
                  [3qrt (+ mid jump)])
             (seq
               (raindrops lo 1qrt new-d (- n 1))
               (raindrops 3qrt hi new-d (- n 1))
               (raindrops 1qrt 3qrt new-d (- n 1))))]))))

; Example 
(raindrops 100 500 qn 2)