; synthesizer.scm

;; CSC-151-02 (Fall 23)
;; Mini-Project 7: A Simple Synthesizer
;; Khanh Do
;; 2023-11-19
;; ACKNOWLEDGEMENTS: every functions that help define function synthesize-square-note
;; are adpated from asdr lab.

(import lab)
(import test)
(import audio)

(title "Mini-Project 7: A Simple Synthesizer")

;; -------------------
(problem "Part 1: Multi-voice synthesis")
;; -------------------

;;; (samples-per-wave sample-rate frequency) -> number?
;;;   sample-rate: number?, a non-negative integer
;;;   frequency: number?, a non-negative number 
;;; Returns the number of samples per waveform for a given sample rate and
;;; frequncy of wave pattern.
(define samples-per-wave
  (lambda (sample-rate frequency)
    (/ sample-rate frequency)))

;;; (square-helper t T) -> integer?
;;;   t: integer?
;;;   T: integer?
;;; Returns -1 or 1 by comparing t to T.
(define square-helper
  (lambda (t T)
    (if (< t (/ T 2))
        -1
        1)))

;;; (sine-helper t T) -> integer?
;;;   t: integer?
;;;   T: integer?
;;; Returns appropriate sine note.
(define sine-helper
  (lambda (t T)
    (sin (* 2 pi (/ t T)))))

;;; (wave-sample sample-rate frequency duration) -> vector?
;;;   waveform: procedure?
;;;   sample-rate: integer?, non-negative
;;;   frequency: number?, non-negative
;;;   duration: integer?, non-negative
;;; Returns a vector of waveform value.
(define wave-sample
  (lambda (waveform sample-rate frequency duration)
    (let* ([W (* duration frequency)]
           [N (round (samples-per-wave sample-rate frequency))]
           [total-samples (* W N)]
           [T (/ 1 frequency)]
           [dt (/ T N)]
           [vec (vector-range 0 total-samples)])
      (begin
        (vector-map! (section modulo _ N) vec)
        (vector-map! (section * _ dt) vec)
        (vector-map! (section waveform _ T) vec)                      
        vec))))