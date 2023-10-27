;; CSC 151 (Fall 2023)
;; Lab: Vectors (vectors.scm)
;; Authors: Khanh Do, Paul Lim
;; Date: 10.25.2023
;; Acknowledgements: N/A

;; In today's lab, we'll explore how we can create and manipulate vectors in
;; Scheme. For each problem, alternative driver/navigator with your partner.
;; ------------------------------
"Problem: Binding and Sequencing"
;; ------------------------------

(import test)

;; Whenever possible, we will want to avoid using top-level definitions in
;; favor of local bindings and parameter passing. However, it is relatively
;; easy syntactically to define a top-level vector and mutate it at the top
;; level. But doing so locally requires a particular combination of binding
;; and sequencing with the begin expression that we should get used to.

;; First, let's review how the begin expression works. Consider the following
;; code snippet that attempts to mutate the first, third, and fifth slots of
;; the vector using function chaining:

; (define example-vector (vector 0 1 2 3 4))

; (vector-set!
;   (vector-set!
;     (vector-set! example-vector 0 "zero")
;     2 "two")
;   4 "four")

;; Uncomment the code (highlight the block, then press ctrl-/) and run it.
;; You should receive an error! In a sentence or two, describe the error and
;; why it occurs:
;;
;; void-set! returns void, not a new vector. You can't vector-set! void.
;;

;; (begin ...) allows us to execute a sequence of side effects. Now, let's
;; combine this concept with a local binding to:
;;
;; (a) Create a new vector (as a let-binding)
;; (b) Mutate the elements of the vector (using vector-set!)
;;