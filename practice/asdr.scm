; asdr.scm

;; CSC 151 (SEMESTER)
;; Lab: ASDR (asdr.scm)
;; Authors: YOUR NAMES HERE
;; Date: THE DATE HERE
;; Acknowledgements:
;;   ACKNOWLEDGEMENTS HERE

(import audio)

;; In today's lab, you will extend your basic synthesized waveforms from the
;; previous lab with the simple ASDR filter from the reading. We'll then use
;; this filter to allow us to create pulses of sound, i.e., notes!
;;
;; (Just like the previous lab, alternate driver-navigator roles with your
;; partner on every function that you write.)

;; -------------------
"Problem 1: In review"
;; -------------------

;; First, we'll need the synthesis functions from the previous lab. Go
;; ahead and copy your definitions of square-sample as well as any other
;; helpers that made this function work in the space below. Take the
;; opportunity to clean up your code a bit as well. You'll need this code
;; for your individual mini-project, so it is worthwhile to ensure everything
;; is in working order before you continue!

(define medium-quality 16000)

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

;;; (square-sample sample-rate frequency duration) -> vector?
;;;   sample-rate: integer?, non-negative
;;;   frequency: number?, non-negative
;;;   duration: integer?, non-negative
;;; Returns a vector of waveform value.
(define square-sample
  (lambda (sample-rate frequency duration)
    (let* ([W (* duration frequency)]
           [N (round (samples-per-wave sample-rate frequency))]
           [total-samples (* W N)]
           [T (/ 1 frequency)]
           [dt (/ T N)]
           [vec (vector-range 0 total-samples)])
      (begin
        (vector-map! (section modulo _ N) vec)
        (vector-map! (section * _ dt) vec)
        (vector-map! (section square-helper _ T) vec)                      
        vec))))

;; Next, we need your definition of simple-envelope from the reading
;; question for the day. You can copy it in the space below. As a
;; reminder, (simple-envelope n) returns a simple, linearly decaying
;; envelope consisting of n samples.

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

;; -------------------------------
"Problem 2: Applying the Envelope"
;; -------------------------------

;; Let's go ahead and apply our envelope now. Recall that to apply an
;; envelope, we just need to multiply each sample in the clip by each sample
;; of the envelope. To implement this function, keep in mind that vector-map!
;; can be passed multiple vectors with a function that takes one value from each
;; vector as its argument.

;;; (apply-envelope clip envelope) -> vector?
;;;   clip: vector? of samples [-1.0, 1.0]
;;;   envelope: vector? of samples in the range [0, 1]
;;; Mutates clip so that the is applied to the clip.
(define apply-envelope
  (lambda (clip envelope)
    (vector-map * clip envelope)))

;; With apply envelope, let's create some functions that play single notes using
;; the waveforms that we defined previously. We'll do this just for a simple
;; square wave, but you hopefully see how you might generalize this function to
;; any of the waveforms!

;;; (synthesize-square-note sample-rate frequency duration) -> vector?
;;;   sample-rate: number?, a non-negative integer
;;;   frequency: number?, a non-negative number
;;;   duration: number?, a non-negative number
;;; Returns a vector of samples representing a single note syntheiszed from
;;; the given parameters.
(define synthesize-square-note
  (lambda (sample-rate frequency duration)
    (let* ([W (* duration frequency)]
           [N (round (samples-per-wave sample-rate frequency))]
           [total-samples (* W N)])
    (apply-envelope (square-sample sample-rate frequency duration) (simple-envelope total-samples)))))

;; Make sure to try out your function with some examples below. At this point
;; You may have noticed that it takes a little bit for your functions to run.
;; This is because we're doing a fair bit of computation! You may find it more
;; productive to try out examples, but comment them out after you are done so
;; that running the file does not take too long. When you are done, make sure
;; to leave one of your samples uncommented so we can try out your code!

(sample-node (synthesize-square-note 16000 440 1)) 
(sample-node (synthesize-square-note 16000 880 2))

;; ---------------------------
"Problem 3 : Beats Aren't Bad"
;; ---------------------------

;; By default, our envelope stretched the entire length of the clip. If we
;; want to create the effect of multiple notes, we can use the same "window"
;; technique we used to generate waveforms to, instead, apply an envelope
;; multiple times throughout the whole clip.
;;
;; Implement the function synthesize-square-notes below that synthesizes a
;; clip that contains many notes, not just one.
;;
;; Note that the functions you have written previous can be leveraged in
;; different ways to implement this function. You may be able to use these
;; functions directly or you may need to refactor them so that they are usable
;; in synthesize-square-notes. In the worst case, you will need to duplicate
;; their behavior within synthesize-square-notes, but try to avoid this
;; whenever possible.
;;
;; As a starting point, you may find it useful to create an envelope with
;; multiple peaks aka the functions we used to create multiple waveforms
;; within a single clip. Additionally, you may find yourself needing to map
;; over a list while also knowing the index of that element in the list.
;; You can obtain this effect by using vector-map! with multiple vectors
;; where one of the vectors is the result of vector-range.

;;; (synthesize-square-notes waveform sample-rate frequency duration n) -> vector?
;;;   waveform: string?
;;;   sample-rate: number?, a non-negative integer
;;;   frequency: number?, a non-negative number
;;;   duration: number?, a non-negative number
;;;   n: number?, a non-negative integer
;;; Returns a vector of samples representing n notes synthesized from the given
;;; parameters.
(define synthesize-square-notes
  (lambda (waveform sample-rate frequency duration n)
    (let* ([single-note (synthesize-square-note sample-rate frequency duration)]
           [single-note-length (vector-length single-note)]
           [total-length (* single-note-length n)]
           [vec (make-vector total-length 0)]
           [indexes (vector-range total-length)])
      (begin
        
        ))))

(synthesize-square-notes "sawtooth" 1000 880 1 2)

;; Like with synthesize-notes, try out your function here. Make sure
;; only leave at most one example uncommented to avoid making the file
;; too time-consuming to run!