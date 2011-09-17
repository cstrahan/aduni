;; ga-search.scm
;;

;;(load "timing")

;;;; Requires a global *alphabet* vector be set up,
;;;; This is a vector of legal values for each position on the genome, each with
;;;; a preference value.
;;;;
;;;; The 'population' thus consists of vectors of "letters" that
;;;; may have preference values.
;;;; Note that the order of the pairs is the same as the order of their
;;;; respective entries in *alphabet*, for simplicity.
;;;; The population would consist of a number of such sets.

(define *verbose* #f)                        ; control printing

;;; The choices for each position, together with their "preference"
(define *alphabet* #f)

;;; Set of best-valued genomes sets is left in *result-set*.
(define *result-set* '())

(define *total-mutations* 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; THE SEARCH
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initial-population:
;;  Constructs a list of (valid) genomes

(define (initial-population size)
  (do ((i 0 (+ i 1))
       (assignments '()))
      ((or (= i 5000.)
           (= size (length assignments)))
       (display* "\n\nInitial population size = " (length assignments))
       assignments)
    (let ((new (list->vector
                (map (lambda (candidates)
                       (random-member candidates))
                     (vector->list *alphabet*)))))
      (if (valid? new)
          (set! assignments (cons new assignments))))))


;;; A little bit of sugar.
;; n-times:  the number of times to try the GA. Note that each time is a
;;           completely different run, and will end up with a different set
;;           of results (and may suceed or fail).
;; pop-size: the number of genomes to have in each generation.
;; steps:    the number of generations without a change in fitness
;;           to wait before giving up, in each trial.
(define (run-ga n-times pop-size steps)
  (cond ((not *alphabet*)  ;; must define alphabet first!
         (display* "Problem not initialized (no alphabet)."))
        (else
         (do ((i 0 (+ i 1))    ;; repeat the GA run for n-times trials.
              (sum 0.0)
              (termination-count 0))
             ((= i n-times)
              (display* "\n\nAverage number of mutations " (/ sum n-times))
              (display* "(" termination-count " trials terminated successfully.)")
              (/ sum n-times))

           (set! *result-set* '())                ; the answers will be here
           (set! *total-mutations* 0)        ; initialize

           ;; Run the code and time it - not available on all releases
           ;;(time-func
           ;; `(ga-driver (initial-population ,pop-size) ,steps))

           (ga-driver  (initial-population pop-size) steps)

           (set! sum (+ sum *total-mutations*))

           ;; report results
           (display* "\n" (length *result-set*) " Assignment Sets with best fitness:\n")
           (pp *result-set*)
           ;; assumes terminating conditon is present in results:
           (if (termination? *result-set*)
               (set! termination-count (1+ termination-count)))))
        ))

(define (termination? pop) #f)                ; the default

;; ga-driver:
;; Runs the genetic algorithm.
;;   requires: *alphabet* must be set up.
;;  initial-pop:          a list of genome vectors - the initial
;;                        population
;;  quiesence-patience:   number of rounds we will wait without change in the
;;                        best fitness in the population.
;; effects: returns the best fitness found. All genome vectors discovered
;;          with that fitness are stored in the global list *result-set*.
(define (ga-driver initial-pop quiesence-patience)
  (define (ga-driver-helper
           cur-pop quiet-rounds up-quiet-rounds last-best total-best)
    (cond
     ((termination? cur-pop)                ; special termination?
      (display* "\n            Success!  Total mutations = " *total-mutations*)
      total-best)
     ((> quiet-rounds quiesence-patience)
      ;; Nothing doing, give up
      (display* "\nGiving up. Nothing is changing.  Total mutations = "
                *total-mutations*)
      total-best)
     ((> up-quiet-rounds (* 4 quiesence-patience)) ; no improvement
      (display* "\nGiving up. Nothing is improving.  Total mutations = "
                *total-mutations*)
      total-best)
     (else
      ;; construct new population and update the progress variables.
      (let* ((new-pop (reproduce cur-pop)) ; sorted by fitness
             (new-best (gvec-value (car new-pop)))
             (new-quiet-rounds
              (cond ((= new-best last-best)
                     (if (zero? quiet-rounds)
                         (display*-c "\n \tcurrent value: " new-best ";\t "
                                     "Quiet for round " (1+ quiet-rounds))
                         (if (zero? (remainder (1+ quiet-rounds) 10))
                             (display*-c " " (1+ quiet-rounds))
                             (display*-c ".")))
                     (1+ quiet-rounds))
                    (else
                     (if (not (zero? quiet-rounds))
                         (display*))
                     0)))
             (new-up-quiet-rounds
              (if (>= new-best total-best)
                  (1+ up-quiet-rounds)
                  0))
             (new-total-best
              (cond ((< new-best total-best)
                     ;; Found a new best.
                     (display* "\nNew best genome found. Value: " new-best)
                     (display* (car new-pop))
                     (set! *result-set* '())
                     new-best)
                    (else
                     total-best))))
        ;; Increment the count of total mutations
        (set! *total-mutations* (+ 1 *total-mutations*))
        ;; Update bests
        (add-new-bests new-pop new-total-best)
        ;; Loop.
        (ga-driver-helper
         new-pop new-quiet-rounds new-up-quiet-rounds new-best new-total-best)))))
  ;; Initial call to loop.

  ;; This was changed 11/13, if the initial population includes the answer,
  ;; this can lead to failure when looking at (empty) *result-set*.  TLP
  ;;(ga-driver-helper (sort initial-pop gvec<?)
  ;;                  0 0 999999999 999999999)

  (let* ((sorted-initial-pop (sort-func initial-pop gvec<?))
         (best-value (gvec-value (first sorted-initial-pop))))
    (add-new-bests sorted-initial-pop best-value)
    (ga-driver-helper sorted-initial-pop
                      0 0 best-value best-value)))

;; add-new-bests:
;;   Given a list of genome vectors and a best fitness (preference) value,
;;   adds to *result-set* all vectors from the list that have the top fitness.
;;  gvec-list: genome vector list; must be sorted in ascending order by
;;             fitness.
;;  best:      the best fitness so far.

(define (add-new-bests gvec-list best)
  (if (or (null? gvec-list) (> (gvec-value (car gvec-list)) best))
      'done
      (begin (if (not (member (car gvec-list) *result-set*))
                 (set! *result-set* (cons (car gvec-list) *result-set*)))
             (add-new-bests (cdr gvec-list) best))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; THE OPERATORS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Mutate some position
(define (mutate course-genome)
  (let* ((new-genome (vector-copy course-genome))
         ;; pick location for mutation
         (n (random (vector-length new-genome)))
         ;; pick value for mutation
         (new-value
          (random-member (vector-ref *alphabet* n))))
    (if *verbose*
        (display* "Replacing " (vector-ref new-genome n) " with " new-value))
    (vector-set! new-genome n new-value)
    new-genome))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; THE REPLACEMENT STRATEGY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (replace-random new pop)
  (cons new (delq (random-member (cdr pop)) pop)))

(define (replace-last new pop)
  (cons new (except-last-pair pop)))

(define replace replace-last)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; THE FITNESS PROBABILITY STRATEGY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The probability of being picked is fitness/(sum of fitnesses)
(define (pick-entry-roulette pop)
  (let* ((fitness (map gvec-value pop))
         (sum-fit (reduce + 0 fitness))
         (probs (map (lambda (fit) (/ fit sum-fit)) fitness)))
    (define (loop i po pr)
      (cond ((null? po) (first pop))        ; did not pick one, so do best one
            ((>= (first pr) (random 1.0))
             (if *verbose*
                 (display* " Selected " i))
             (first po))
            (else
             (loop (+ i 1) (cdr po) (cdr pr)))))
    (loop 0 pop probs)))

;;; The probability of being picked is rank/(sum of ranks)
(define (pick-entry-ranks pop)
  (let* ((ranks (ranks pop))
         (max-rank (reduce max 0 ranks))
         (sum-rank (reduce + 0 ranks))
         ;; convert low rank to larger probs
         (probs (map (lambda (rank) (/ (1+ (- max-rank rank)) sum-rank)) ranks)))
    (define (loop i po pr)
      (cond ((null? po) (first pop))        ; did not pick one, so do best one
            ((>= (first pr) (random 1.0))
             (if *verbose*
                 (display* " Selected " i))
             (first po))
            (else
             (loop (+ i 1) (cdr po) (cdr pr)))))
    (loop 0 pop probs)))

;;; Construct a list of the ranks, including ties.
;;; A good candidate for "most obtuse code"
(define (ranks list)
  (let ((groups '())
        (next list)
        (rank 1))
    (define (next-group l val)
      (cond ((null? l)
             (set! next l)
             '())
            ((= (gvec-value (car l)) val)
             (cons (car l) (next-group (cdr l) val)))
            (else
             (set! next l)
             '())))
    (define (loop)
      (cond ((null? next) (reverse groups))
            (else
             (let ((g (next-group next (gvec-value (car next)))))
               (set! groups (cons (cons rank g) groups))
               (set! rank (+ 1 rank)))
             (loop))))
    (apply append
           (map (lambda (g)
                  (map (lambda (x) (car g)) (cdr g)))
                (loop)))
    ))

(define pick-entry pick-entry-ranks)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; THE REPRODUCTION STRATEGY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (reproduce pop)
  (if *verbose*
      (display* (map gvec-value pop)))
  (let* ((new (mutate (pick-entry pop))))
    (if *verbose*
        (display* "New kid on the block " (gvec-value new)))
    (sort-func
     ;; replace some kid with new kid
     (replace new pop)
     gvec<?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; THE FITNESS OF A GENOME
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; gvec-value:
(define (gvec-value cv)
  (let ((cvl (vector->list cv)))
    (if (valid? cvl)
        (apply + (map gpair-value cvl))
        1000.)))

(define (valid? x) #t)                        ; default

;; gpair-value:
;;   Return the preference heuristic value from a course-ta genome pair.
(define (gpair-value cp)
  (cadr cp))

;; random-member:
;;   Return a random element of a list.
(define (random-member els)
  (list-ref els (random (length els))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; STRINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The 26 letters of the alphabet
(define *letters* "abcdefghijklmnopqrstuvwxyz")

;;; Defines the *alphabet* used by GA
(define (set-string-alphabet target)
  (set! *alphabet* (make-vector (string-length target)))
  (do ((i 0 (+ i 1)))
      ((= i (string-length target)))
    (vector-set! *alphabet*
                 i
                 (map (lambda (let)
                        (if (equal? let (string-ref target i))
                            (list let 0)
                            (list let 1)))
                      (string->list *letters*))))
  )

(define (strings-valid? x) #t)                        ; no constraints

(define (strings-termination? pop)                ; if we get to 0, we've got a match
  (= 0 (gvec-value (first pop))))

(define (init-strings word)
  (set! valid? strings-valid?)
  (set! termination? strings-termination?)
  (set-string-alphabet word)
  'done)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COURSES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *candidate-course-assignments*
'((c6nnn (((c6nnn ben) 5) ((c6nnn louis) 5)))
 (c6mmm
  (((c6mmm julius) 4) ((c6mmm buck) 5)
                      ((c6mmm george) 5)
                      ((c6mmm ima) 5)
                      ((c6mmm isaac) 5)
                      ((c6mmm ben) 5)
                      ((c6mmm electra) 5)
                      ((c6mmm jimmy) 5)
                      ((c6mmm patty) 5)
                      ((c6mmm melisa) 3)
                      ((c6mmm alyssa) 5)
                      ((c6mmm jack) 5)
                      ((c6mmm john) 5)))
 (c6lll
  (((c6lll julius) 4) ((c6lll buck) 5)
                      ((c6lll george) 5)
                      ((c6lll ima) 5)
                      ((c6lll isaac) 5)
                      ((c6lll ben) 5)
                      ((c6lll electra) 5)
                      ((c6lll thorbear) 5)
                      ((c6lll jimmy) 5)
                      ((c6lll patty) 5)
                      ((c6lll melisa) 5)
                      ((c6lll alyssa) 5)
                      ((c6lll jack) 5)
                      ((c6lll john) 5)
                      ((c6lll louis) 5)))
 (c6iii (((c6iii jimmy) 5) ((c6iii louis) 6)))
 (c6eee
  (((c6eee julius) 5) ((c6eee buck) 5)
                      ((c6eee george) 5)
                      ((c6eee ima) 5)
                      ((c6eee isaac) 5)
                      ((c6eee ben) 5)
                      ((c6eee electra) 5)
                      ((c6eee thorbear) 4)
                      ((c6eee jimmy) 5)
                      ((c6eee patty) 5)
                      ((c6eee melisa) 5)
                      ((c6eee alyssa) 5)
                      ((c6eee jack) 5)
                      ((c6eee john) 5)
                      ((c6eee louis) 5)))
 (c6ddd (((c6ddd george) 5) ((c6ddd ima) 5) ((c6ddd patty) 5)))
 (c6ccc (((c6ccc george) 4) ((c6ccc ima) 4)))
 (c6bbb
  (((c6bbb george) 5) ((c6bbb ima) 5)
                      ((c6bbb ben) 5)
                      ((c6bbb thorbear) 5)
                      ((c6bbb jimmy) 5)
                      ((c6bbb patty) 5)
                      ((c6bbb melisa) 5)
                      ((c6bbb alyssa) 5)
                      ((c6bbb john) 5)
                      ((c6bbb louis) 5)))
 (c6aaa (((c6aaa melisa) 5) ((c6aaa alyssa) 5)))
 (c6034 (((c6034 thorbear) 4) ((c6034 john) 3)))))

(define *candidate-course-assignments-vec*
  (list->vector *candidate-course-assignments*))

(define (set-courses-alphabet)
  (set! *alphabet*
        (list->vector
         (map cadr (vector->list *candidate-course-assignments-vec*)))
        ))

;;; This is very specific to this test case
(define (courses-termination? pop)                ; if we get to 43
  (= 43 (gvec-value (first pop))))

;; Course specific stuff.

;; gpair-ta:
;;   Return the TA from a course-ta gene pair.
(define (gpair-ta cp)
  (cadar cp))

;; gpair-course:
;;   Return the course from a course-ta gene pair.
(define (gpair-course cp)
  (caar cp))

;; gpair-course-ta:
;;   Return the course and TA from a course-ta gene pair.
(define (gpair-course-ta cp)
  (car cp))

;; Is the same TA used twice?
(define member-ta
  (member-procedure
   (lambda (x y) (eq? (gpair-ta x) (gpair-ta y)))))

;; A genome is not valid if a TA is used twice.
(define (courses-valid? ca)
  (if (vector? ca)
      (valid? (vector->list ca))
      (if (null? ca) #t
          (if (member-ta (car ca) (cdr ca))
              #f
              (valid? (cdr ca))))))

(define (init-courses)                        ; to do courses example
  (set! valid? courses-valid?)
  (set! termination? courses-termination?)
  (set-courses-alphabet)
  'done)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UTILITIES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; random-new-member:
;;   Return a random element of a list that is not the same as 'old'.
(define (random-new-member els old)
  (define (skip-index i)
    (if (member old (list-head els (1+ i)))
        (1+ i)
        i))
  (if (= 1 (length els))
      #f
      (list-ref els (skip-index (random (-1+ (length els)))))))

;; vector-search:
;;   Return an element of vec if 'pred' is true for it or #f otherwise.
(define (vector-search vec pred)
  (list-search-positive (vector->list vec) pred))

;; vector-index-of:
;;   Return the index of 'item' in 'vec', or #f if it does not exist.
(define (vector-index-of vec item)
  (define (v-i-helper index)
    (if (>= index (vector-length vec))
        #f
        (if (equal? item (vector-ref vec index))
            index
            (v-i-helper (1+ index)))))
  (v-i-helper 0))

;; gvec<?:
;;   Check if one genome vector is less than another based on their
;;   fitness values.
(define (gvec<? cv1 cv2)
  (if (not cv2)
      #t
      (if (not cv1)
          #f
          (< (gvec-value cv1) (gvec-value cv2)))))

;;; DISPLAY

(define (display* . args)
  (if (not *t:silent*)
      (begin
        (for-each display args)
        (newline))))

(define (display*-c . args)
  (if (not *t:silent*)
      (for-each display args)))

(define *t:silent* #f)

(define (scheme-eval x)
  ;; In MIT Scheme, eval requires a specified environment
  (eval x (the-environment)))


;;;; Sort routine to be consistent across platforms:
(define (merge-sort-fixed obj pred)
  (if (vector? obj)
      (merge-sort-fixed! (vector-copy obj) pred)
      (vector->list (merge-sort-fixed! (list->vector obj) pred))))

(define (merge-sort-fixed! v pred)
  (if (not (vector? v))
      (error:wrong-type-argument v "vector" 'merge-sort-fixed!))
  (let sort-subvector ((v v) (temp (vector-copy v)) (low 0) (high (vector-length v)))
    (if (fix:> (fix:- high low) 1)
        (let ((middle (fix:quotient (fix:+ low high) 2)))
          (sort-subvector temp v low middle)
          (sort-subvector temp v middle high)
          (let merge ((p low) (p1 low) (p2 middle))
            (if (fix:< p high)
                (if (and (fix:< p1 middle)
                         (or (eq? p2 high)
                             (pred (vector-ref temp p1) (vector-ref temp p2))))
                    (begin (vector-set! v p (vector-ref temp p1))
                           (merge (fix:+ p 1) (fix:+ p1 1) p2))
                    (begin (vector-set! v p (vector-ref temp p2))
                           (merge (fix:+ p 1) p1 (fix:+ p2 1)))))))))
  v)

;;(define sort-func sort)
;;(define sort-func quick-sort)
(define sort-func merge-sort-fixed)


;;;; From problem set:

;; This mutate operator attempts to propagate changes so that the course
;; gene vector stays valid.
;; It performs well on the given test of a population of size 10 and
;; quiescence threshold of 1000 - as stated, it always succeeds, with
;; 140 mutations per run on average.
;;
;; A common solution is to keep mutating until the mutated gene vector
;; is both different and valid. While this shows an understanding of the
;; problem, it is non-optimal. There is a lot of work to be done for 
;; that, and it can potential go on for a long time. Perhaps more
;; importantly, it will only allow point mutations - mutations in one
;; gene at a time. That will limit the distance you can move in the
;; genotype space at each generation, and may even make some gene
;; vectors unreachable (such as when two TAs need to be swapped to
;; stay valid). The solution here will allow large moves in 'valid'
;; directions.

;; Tries to get a consistent mutation or just returns the parent
;(define (mutate course-assignment)
;  (or (smart-mutate course-assignment)
;      course-assignment))

;; smart-mutate:
;;   Randomly change one course gene to another valid gene for that
;;   course. Propagate the changes by making new random genes to avoid
;;   any conflicts created. If this fixing-up fails, returns original genome; 
;;   otherwise, returns the new course gene vector.
;;  course-genes: the original course gene vector
;;  max-spread-arg:     an optional argument giving the maximum number of
;;                      conflict-resolving propogations to attempt. Defaults
;;                      to the length of the gene vector.
(define (smart-mutate course-genes . max-spread-arg)
  (let ((max-spread (if (null? max-spread-arg)
                        (vector-length course-genes)
                        (car max-spread-arg))))
    (let* ((zap-course (random (vector-length course-genes)))
           (possible-genes (vector-ref *alphabet* zap-course)))
      (if (= 1 (length possible-genes))	; no choices for this course
          (smart-mutate course-genes max-spread) ; try again
          (let ((old-gene (vector-ref course-genes zap-course))
		;; pick a new ta for some course
                (new-gene (list-ref possible-genes
				    (random (length possible-genes)))))
            (if (equal? new-gene old-gene) ; got the same answer back
                (smart-mutate course-genes max-spread) ; try again
                (swap-gene course-genes zap-course new-gene 0 max-spread)))))))

;; swap-gene:
;;   Change one gene to another choice.
;;   Helper function for (smart-mutate). Attempts to recursively resolve
;;   conflicts by picking new genes. Returns #f if it can't do so within the
;;   maximum depth.
;;  course-genes: the original course genes vector
;;  course-num:          the index of the course to be modified
;;  new-gene:     the gene pair to replaced the current one
;;  depth:              the current number of gene propagations
;;  max-depth:          maximum number of gene propagations
(define (swap-gene course-genes course-num new-gene depth max-depth)
  (if (> depth max-depth)
      #f				; done
      (let ((old-gene (vector-ref course-genes course-num))
            (new-course-genes (vector-copy course-genes)))
        (vector-set! new-course-genes course-num new-gene)
        (let ((affected
	       ;; find a conflicting assignment (same ta)
	       (vector-search 
		course-genes
		(lambda (a) 
		  (eq? (gpair-ta a) (gpair-ta new-gene))))))
          (if (not affected)
              new-course-genes		; done
	      ;; try to fix the conflict
              (let* ((affected-index (vector-index-of course-genes affected))
                     (affected-others (vector-ref *alphabet* affected-index)))
                (if (= 1 (length affected-others))
                    #f
                    (swap-gene new-course-genes affected-index
			       (random-new-member affected-others affected)
			       (1+ depth) max-depth))))))))
