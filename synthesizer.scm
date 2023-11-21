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