(import image)

; conttontail.scm
;
; An amazing image of a pastel mountain cottontail I've created.
;
; CSC 151 02 (23fa)
; Mini Project 1, Part 2
; Author: Khanh Do
; Date: 2022-09-04
; Acknowledgements: Following the instructions of the 
; mini-project 1 on the CSC-151 website.

; (...code below here...)  
;

; Color palette:
(define pink
  (color 248 200 220 1))

(define purple
  (color 195 177 225 1))

(define space
  (lambda (width height)
    (rectangle width height "solid" "transparent")))

; Rabbit's ears
(define ear
  (overlay/align "middle" 
                 "bottom" 
                 (ellipse 20 75 "solid" purple) 
                 (ellipse 35 100 "solid" pink)))

(define ears
  (beside ear ear))

; Rabbit's head
(define mouth
  (above (triangle 10 "solid" purple)
         (rectangle 25 1.5 "solid" "black")
         (beside (rectangle 4 4.5 "solid" "white")
                 (space 1.5 4.5)
                 (rectangle 4 4.5 "solid" "white"))))

(define head 
  (overlay (above 
             (above 
               (beside (circle 3.5 "solid" "black")
                       (space 21 1)
                       (circle 3.5 "solid" "black"))
                       (space 1 10))
               mouth)
           (ellipse 100 88 "solid" pink)))

; Rabbit's body
(define hands
  (beside (circle 14 "solid" "white")
          (space 5 1)
          (circle 14 "solid" "white")))

(define belly
  (overlay/align "middle" "bottom" 
    (above (ellipse 60 77 "solid" purple)
           (space 10 10))
           (ellipse 105 145 "solid" pink)))

(define body
  (overlay/offset -22 -24 hands belly))

; Rabbit's legs and tail
(define legs
  (beside (ellipse 50 25 "solid" purple)
          (ellipse 50 25 "solid" purple)))

(define tail
  (circle 18 "solid" pink))

; The whole rabbit
(define rabbit
  (overlay/offset -90 
                  -270 
                  tail 
                  (above ears
                         head
                         body
                         legs)))

; Mountains
(define mountain
  (overlay/offset 9 
                  -17.5 
                  (triangle 20 "solid" "gainsboro")
                  (triangle 40 "solid" "silver")))

; The complete image 
(define my-image
  (beside mountain
          mountain
          rabbit
          mountain
          mountain))

my-image