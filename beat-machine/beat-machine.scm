;; CSC-151-03 (23 Fall)
;; Mini-Project 3: Beat Machine
;; Khanh Do
;; 2023-10-03
;; ACKNOWLEDGEMENTS:
;;   Following the instructions on web.
;;   CS tutor help (10/4) from: Simon Taye 

(import music)
(import lab)

(problem "Part 1. Percussive Dynamics")

"accent"
;;; (accent midi-note dur) -> composition?
;;;   midi-note: composition?
;;;   dur: dur?
;;; Makes the given note played at louder volume.
(define accent 
  (lambda (midi-note dur)
    (mod (dynamics 100) (note midi-note dur))))

(accent 40 wn)

"ghost"
;;; (ghost midi-note dur) -> composition?
;;;   midi-note: composition?
;;;   dur: dur?
;;; Makes the given note played at smaller volume.
(define ghost 
  (lambda (midi-note dur)
    (mod (dynamics 23) (note midi-note dur))))

(ghost 40 wn)

"strokes"
;;; Two bars of snare drum quarter notes. Each of these bars
;;; consists of four snare drum quarter notes: 
;;; an accented note, two ghost notes, and a regular note.
(define strokes
  (mod percussion (repeat 2 (seq (accent 38 qn) 
                                 (repeat 2 (ghost 38 qn))   
                                 (note 38 qn)))))

strokes 

"tremolo"
;;; (tremolo slashes midi-note d) -> composition?
;;;   slashes: integer?
;;;   midi-note: composition?
;;;   d: dur?
;;; Returns a sequence of evenly spaced notes from midi-note 
;;; value that fit into duration dur.
(define tremolo 
  (lambda (slashes midi-note d)
    (let* ([num (numerator d)]
           [den (denominator d)]
           [rep (expt 2 slashes)])
      (mod percussion (repeat rep (note midi-note (dur num (* den rep))))))))

(tremolo 3 40 wn)

"roll"
;;; (roll midi-note dur) -> composition?
;;;   midi-note -> composition?
;;;   dur -> dur?
;;; Creates a roll of the given duration dur by subdividing dur
;;; into four equally-spaced notes.
(define roll 
  (lambda (midi-note dur) 
    (mod percussion (tremolo 2 midi-note dur))))

(roll 40 wn)

"flam"
;;; (flam midi-note d) -> composition?
;;;   midi-note: composition?
;;;   d: dur?
;;; Creates a flam which is a grace note of half the duration 
;;; played before the given note. The given note is also played
;;; with an accent.
(define flam 
  (lambda (midi-note d)   
    (mod percussion 
      (pickup (tremolo 1 midi-note d) 
              (accent midi-note d)))))

(flam 40 qn)

"single-drag-tap"
;;; (single-drag-tap midi-note) -> composition?
;;;   midi-note: composition?
;;; Creates a single drag tap, which is a pair of sixteenth-note grace notes, 
;;; followed by a regular eighth note, and then finally an accented eighth note.
(define single-drag-tap 
  (lambda (midi-note)
    (let* ([grace-note (note midi-note sn)]
           [accent-note (accent midi-note en)]
           [reg-note (note midi-note en)])
      (mod percussion 
        (seq (pickup (repeat 2 grace-note) reg-note) accent-note)))))

(single-drag-tap 40)

(problem "Part 2: An Example Groove")

;;; Drum kit:

;;; A quarter note hi-hat
(define hi-hat-4 
  (note 42 qn))

;;; A quarter note snare
(define snare-4 
  (note 38 qn))

;;; A quarter note bass
(define bass-4 
  (note 35 qn))

"horizontal-simple-rock-beat"
(define horizontal-simple-rock-beat
  (let ([top (repeat 4 hi-hat-4)]
        [mid (repeat 2 (seq (rest qn) snare-4))]
        [low (repeat 2 (seq bass-4 (rest qn)))])
    (mod percussion (par top mid low))))

horizontal-simple-rock-beat

"vertical-simple-rock-beat"
(define vertical-simple-rock-beat 
  (let* ([pulse-1 (par hi-hat-4 bass-4)]
         [pulse-2 (par hi-hat-4 snare-4)])
    (mod percussion (seq pulse-1 pulse-2 pulse-1 pulse-2))))

vertical-simple-rock-beat

(problem "Part 3: Slicing up the Beats")

"horizontal-beat-machine"
;;; (horizontal-beat-machine voices) -> composition?
;;;   voices: list?
;;; Takes a list of compositions as input that represent each of the voices
;;; of the pattern and composes.
(define horizontal-beat-machine
  (lambda (voices)
    (let ([top (list-ref voices 0)]
          [mid (list-ref voices 1)]
          [low (list-ref voices 2)])
      (mod percussion (par top mid low)))))

(horizontal-beat-machine 
  (list (repeat 4 hi-hat-4) 
        (repeat 2 (seq (rest qn) snare-4)) 
        (repeat 2 (seq bass-4 (rest qn)))))

;; Helper Func:

;;; (1-note lst duration) -> composition?
;;;   lst: list?
;;;   duration: dur?
;;; Takes in a list of durations and turn them into a list of notes.
(define 1-note
  (lambda (lst duration) (map (lambda (n) (note n duration)) lst)))

;;; (pulse lst duration) -> composition?
;;;   lst: list?
;;;   duration: dur?
;;; Takes in a list of compositions and par the notes together. Creates a pause
;;; if the element of the list is null.
(define pulse
  (lambda (lst duration) 
    (if (null? lst)
        (rest duration)
        (apply par (1-note lst duration)))))

"vertical-beat-machine"
;;; (vertical-beat-machine pulses duration) -> composition?
;;;   pulses: list?
;;;   duration: dur?
;;; Takes a list of pulses as input. Each “pulse” itself is a list of MIDI values
;;; corresponding to the voices played on that pulse. duration is the length of 
;;; each pulse specified as a dur value.
(define vertical-beat-machine 
  (lambda (pulses duration) 
      (mod percussion 
        (apply seq (map (section pulse _ duration) pulses)))))

(vertical-beat-machine (list (list 42 35) (list 42 38) (list 42 35) (list 42 38)) qn)

;;; Tools kit:

;;; An eighth note hi-hat
(define hi-hat-8 
  (note 42 en))

;;; An eighth note snare
(define snare-8 
  (note 38 en))

;;; An eighth note bass
(define bass-8 
  (note 35 en))

;;; (accent-note comp) -> composition?
;;;   comp: composition?
;;; Creates a new composition with an accent.
(define accent-note 
  (lambda (comp)
    (mod (dynamics 96) comp)))

;;; (ghost-note comp) -> composition?
;;;   comp: composition?
;;; Creates a new composition with ghost effect.
(define ghost-note
  (lambda (comp)
    (mod (dynamics 32) comp)))

;;; Creates an eighth note snare with ghost effect
(define ghost-snare-8
  (ghost-note snare-8))

;;; Creates an eighth note snare with accent effect
(define accent-snare-8
  (accent-note snare-8))

"horizontally-elaborate-rock-beat"
(define horizontally-elaborate-rock-beat
  (let* ([top (repeat 8 hi-hat-8)]
         [2-ghost-snare (repeat 2 ghost-snare-8)]
         [mid (seq (repeat 2 (seq 2-ghost-snare accent-snare-8)) 2-ghost-snare)]
         [low (seq (repeat 2 bass-8) (rest en) bass-8 (rest (dur 3 8)) bass-8)])
    (horizontal-beat-machine (list top mid low))))

horizontally-elaborate-rock-beat

"vertically-elaborate-rock-beat"
(define vertically-elaborate-rock-beat
  (vertical-beat-machine (list (list 42 38 35)
                               (list 42 38 35)
                               (list 42 38) 
                               (list 42 38 35) 
                               (list 42 38) 
                               (list 42 38) 
                               (list 42 38) 
                               (list 42 38 35)) 
                         en))

vertically-elaborate-rock-beat

(problem "Part 4: Exploring Grooves")

"horizontal-latin-beat"
(define horizontal-latin-beat 
    (let* ([side-stick-8 (note 37 en)]
           [top (repeat 8 hi-hat-8)]
           [mid (seq (repeat 2 (seq (rest qn) side-stick-8)) (rest qn))]
           [low (repeat 2 (seq bass-4 (rest en) bass-8))])
      (horizontal-beat-machine (list top mid low))))

horizontal-latin-beat

"vertical-latin-beat"
(define vertical-latin-beat
  (vertical-beat-machine (list (list 42 35) 
                               (list 42)
                               (list 42 37)
                               (list 42 35)
                               (list 42 35)
                               (list 42 37)
                               (list 42) 
                               (list 42 35))
                         en))

vertical-latin-beat

"horizontal-swing-beat"
(define horizontal-swing-beat
  (let* ([hi-hat-triplet (note 42 (dur 1 12))]
         [top (repeat 2 (seq hi-hat-4 hi-hat-triplet (rest (dur 1 12)) hi-hat-triplet))]
         [mid (repeat 2 (seq (rest qn) snare-4))]
         [low (repeat 4 bass-4)])
    (horizontal-beat-machine (list top mid low))))

horizontal-swing-beat

"vertical-swing-beat"
(define vertical-swing-beat
  (vertical-beat-machine (list (list 35 42) 
                               null 
                               null
                               (list 35 42 38) 
                               null
                               (list 42)
                               (list 35 42)
                               null 
                               null
                               (list 35 38 42)
                               null
                               (list 42))
                         (dur 1 12)))

vertical-swing-beat

"horizontal-funk-beat"
(define horizontal-funk-beat
  (let* ([top (repeat 8 (note 42 en))]
         [ghost-snare-16 (ghost-note (note 38 sn))]
         [of-mid (seq accent-snare-8 (rest sn) ghost-snare-16)]
         [mid (seq (rest qn) of-mid (rest sn) (repeat 2 ghost-snare-16) (rest sn) of-mid)]
         [bass-16 (note 35 sn)]
         [low (seq (repeat 2 (note 35 en)) (rest qn) (rest (dur 3 16)) bass-16 (rest en) (note 35 en))])
    (horizontal-beat-machine (list top mid low))))

horizontal-funk-beat

"vertical-funk-beat"
(define vertical-funk-beat
  (vertical-beat-machine (list (list 42 35)
                               null
                               (list 42 35)
                               null
                               (list 42 38 35)
                               null
                               (list 42)
                               (list 38)
                               (list 42)
                               (list 38)
                               (list 42 38)
                               (list 35)
                               (list 42 38 35)
                               null
                               (list 42 35)
                               (list 38))
                         sn))

vertical-funk-beat

"horizontal-garba-beat"
(define horizontal-garba-beat
  (let* ([snare-dur (section note 38 _)]
         [top (seq (rest (dur 3 4)) (repeat 3 (note 48 en)))]
         [mid (seq (repeat 2 (seq (rest en) (snare-dur en))) 
                   (rest (dur 3 32)) 
                   (snare-dur tn) 
                   (rest sn)
                   (snare-dur sn)
                   (rest (dur 3 8))
                   (snare-dur en))]
         [low (seq (repeat 2 (seq (note 35 en) (rest en))) (note 35 tn))])
    (horizontal-beat-machine (list top mid low))))

horizontal-garba-beat

;; Attempt vertical-garba-beat
; (define vertical-funk-beat
;   (vertical-beat-machine (list (list 35)
;                                null
;                                (list 38)
;                                null
;                                (list 35)
;                                null
;                                (list 38)
;                                null
;                                (list 38)
;                                null
;                                null
;                                (list 35)
;                                (list 42 38 35)
;                                null
;                                (list 42 35)
;                                (list 38))
;                           sn))

"horizontal-my-beat"
(define horizontal-my-beat
  (let* ([3-hi-hat-16 (repeat 3 (note 42 sn))]
         [2-ghost-snare (repeat 2 (ghost 38 qn))]
         [of-top (seq (note 42 en) (repeat 2 (note 42 sn)))]
         [top (repeat 4 of-top)]
         [mid (seq (rest sn) 2-ghost-snare (rest en) 2-ghost-snare)]
         [accent-bass (accent 35 sn)]
         [of-low (seq (note 35 en) accent-bass (note 37 sn))]
         [low (mod (tempo qn 140) (repeat 4 of-low))])
    (repeat 4 (horizontal-beat-machine (list top mid low)))))

horizontal-my-beat

(problem "Part 5: Reflection")

;; a. Overall, which implementation of beat machine did you find easier to
;; use to implement the various grooves in this mini-project? 
;; Which implementation of beat machine did you choose for your final groove?
;; Explain why! 

;; > I find it easier to use the horizontal-beat-machine when I need ghost or accent note,
;; and easier to use the vertical-beat-machine when I can easily read the sheet and divide
;; the beats up equally. I chose to implement horizontal-beat-machine even though it does not 
;; look as clean as vertical, I can easily manipulate notes, instead of having to only able to
;; pass in midi value number. 

;; I could not vertically composes the garba-beat because:
;; + The snare drum on the third beat is broken up into eight thirty-second notes, which will be 
;; hard to implement using vertical machine because our vertical machine operates on pulses of a 
;; fixed duration.
;; + Same problem for the triplet hi-hat, it is hard to compose such variations using our vertical
;; machine.
;; + Using horizontal beat machine is easier because we can map out each note in time sequence.

;; b. Look back at the Roland TR-909 emulator linked in part 3. Additionally, look at this drum
;; machine implementation by @berkcebi. Which way do these drum machines that are emulators of 
;; or inspired by physical drum machines slice up a groove? Based on your experience, why do you 
;; think physical drum machines are set up this way? 

;; > They both slice up a groove vertically (16 pulses). They are set up this way because as a digital
;; user, it is easier to modify the rhythm, change and choose voices, especially par-ing them on top
;; of each other, by columns by columns, corresponding to 16 steps, representing 4/4 measure which is
;; popular. 

;; c. Finally, recall that our vertical-beat-machine ignored modifications of notes, e.g., ghosts
;; and accents! Describe briefly how you might fix vertical-beat-machine to allow for ghosts and 
;; accents and comment on the negatives of your approach.

;; > Similar to how I add (rest duration) to the beat-machine, I can create new conditions that do something
;; like if an element of the elements of voices is a note, then allow to pass ghost-note/accent-note
;; to modify that specific one. The negatives of my approach is that I would have to have another, to go
;; to the farthest nested functions and check the type of each elements, and also throw that thing out. It
;; is not as easy like if (null? lst) because we have to check inner-er than that.