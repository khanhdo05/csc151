(import image)

; spaceship.scm
;
;
; CSC 151 02 (23fa)
; Mini Project 1, Part 1
; Author: Khanh Do
; Date: 2022-09-04
; Acknowledgements: Following the instructions of the 
; mini-project 1 on the CSC-151 website.

; (...code below here...)  
;
(define stripe
   (lambda (color)
     (rectangle 100 25 "solid" color)))

(define stripe-1 (stripe "red"))
(define stripe-2 (above stripe-1 (stripe "orange")))
(define stripe-3 (above stripe-2 (stripe "yellow")))
(define stripe-4 (above stripe-3 (stripe "green")))
(define stripe-5 (above stripe-4 (stripe "blue")))
(define stripe-6 (above stripe-5 (stripe "violet")))

(define rainbow-spaceship
  (beside stripe-1 
          stripe-2
          stripe-3
          stripe-4
          stripe-5
          stripe-6
          stripe-5
          stripe-4
          stripe-3
          stripe-2
          stripe-1))

rainbow-spaceship