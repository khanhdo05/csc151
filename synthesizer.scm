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

;;; (simple-envelope-helper total-samples n) -> number?
;;;   total-samples: integer? non-negative
;;;   n: integer?, non-negative
;;; Returns a number that is calculated.
(define simple-envelope-helper
  (lambda (total-samples n)
    (let ([step (/ 1.0 total-samples)])
      (- 1.0 (* step n)))))

;;; (simple-envelope total-samples) -> vector?
;;;   total-samples: integer? non-negative
;;; Takes the total number of samples in the clip total-samples and returns a vector
;;; of linear decay steps from 1.0 to 0.0.
(define simple-envelope
  (lambda (total-samples)
    (|> (vector-range total-samples)
        (lambda (vec) 
          (vector-map (section simple-envelope-helper total-samples _) vec)))))

;;; (apply-envelope clip envelope) -> vector?
;;;   clip: vector? of samples [-1.0, 1.0]
;;;   envelope: vector? of samples in the range [0, 1]
;;; Mutates clip so that the is applied to the clip.
(define apply-envelope
  (lambda (clip envelope)
    (vector-map * clip envelope)))

;;; (generate-wave-note sample-rate frequency duration) -> vector?
;;;   waveform: procedure?
;;;   sample-rate: number?, a non-negative integer
;;;   frequency: number?, a non-negative number
;;;   duration: number?, a non-negative number
;;; Returns a vector of samples representing a single note syntheiszed from
;;; the given parameters.
(define generate-wave-note
  (lambda (waveform sample-rate frequency duration)
    (let* ([W (* duration frequency)]
           [N (round (samples-per-wave sample-rate frequency))]
           [total-samples (* W N)])
    (apply-envelope (wave-sample waveform sample-rate frequency duration) (simple-envelope total-samples)))))
