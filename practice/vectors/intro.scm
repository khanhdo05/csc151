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
;; Complete the definition of the function below that creates a new vector
;; of five elements, initially the values 0 through 4. The function then
;; mutates the 1st, 3rd, and 5th elements to be "zero", "two", and "four."
;; Finally, the function returns that vector as output. (Note that the
;; value of the final expression is produced by a begin expression!)

(define make-and-mutate-vector
  (lambda ()
    (let ([original-vec (vector 0 1 2 3 4)])
      (begin
        (vector-set! original-vec 0 "zero")
        (vector-set! original-vec 2 "two")
        (vector-set! original-vec 4 "four")
        original-vec))))

(test-case "example" 
           equal? 
           (list "zero" 1 "two" 3 "four")
           (lambda () (vector->list (make-and-mutate-vector))))

(make-and-mutate-vector)

;; ----------------
"Problem: Swapping"
;; ----------------

;; Complete the following function definition using vector-set! that
;; swaps the elements found at two locations in a vector. For this function,
;; make sure to check that i and j are valid indices into the vector. If
;; not, your function should raise an error with (error ...).
;; (swap! v i j) -> void?
;;   v: vector?
;;   i: integer?, a valid index into v
;;   j: integer?, a valid index into v
;; Swaps the elements at indices i and j of v.
(define swap!
  (lambda (vec i j)
    (if (and (< i (vector-length vec)) (< j (vector-length vec)))
        (let ([i-j (vector-ref vec j)]
              [j-i (vector-ref vec i)])
          (begin
            (vector-set! vec i i-j)
            (vector-set! vec j j-i)))
        "Error")))

(test-case "example"
           equal?
           (list 1 2 5 4 3)
           (lambda () 
             (vector->list 
               (let ([v (vector 1 2 3 4 5)])
                 (begin (swap! v 2 4)
                        v)))))
(test-case "error"
           equal?
           "Error"
           (lambda () (swap! (vector 1 2 3) 10 4)))

(test-case "returns void"
           equal?
           void
           (lambda () (swap! (vector 1 2 3) 0 1)))

;; --------------------------------
"Problem: Incrementing in two ways"
;; --------------------------------

;; A common "stateful" operation we might consider is incrementing the
;; value at a particular location in a vector. We may also want the value
;; at that location either _before_ the increment occurs or _after_ the
;; increment occurs.
;;
;; Implement the following pair of functions:
;;
;; + (pre-inc vec i) takes a vector vec of numbers and a valid index i into
;;   vec as input. The function increments the ith element of vec as a side-
;;   effect and returns the value of the ith element _before_ the increment.
;; + (post-inc vec i) takes a vector vec of numbers and a valid index i into
;;   vec as input. The function increments the ith element of vec as a side-
;;   effect and returns the value of the ith element _after_ the increment.

(define pre-inc
  (lambda (vec i)
    (let ([i-before (vector-ref vec i)])
      (begin
        (vector-set! vec i (+ (vector-ref vec i) 1))
        i-before))))

(test-case "pre-inc"
           equal?
           3
          (lambda () (pre-inc (vector 1 2 3) 2)))

(define post-inc
  (lambda (vec i)
    (begin
      (vector-set! vec i (+ (vector-ref vec i) 1))
      (vector-ref vec i))))

(test-case "post-inc"
           equal?
           4
           (lambda () (post-inc (vector 1 2 3) 2)))