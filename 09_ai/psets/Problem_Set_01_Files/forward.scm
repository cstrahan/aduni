;;;; -*- mode:Scheme -*- ;;;;
;;
;; forward.scm
;;
;; A simple (and inefficient) forward-chaining rule interpreter for 6.034.
;; Derives from an implementation by Patrick Winston.  Extensively
;; modified by Tomas Lozano-Perez.

;; Ignore this; it aids compilation but affects nothing else.
(declare (usual-integrations))

;;;; FORWARD CHAINING

(define *trace* #t)

;; Top-level chainer that uses try-first-rule to generate list of triggered
;; rule instances.  Returns number of steps in chaining.

(define (chain . max-step)
  (chain-internal try-first-rule
                  choose-first-instance
                  1
                  (if (null? max-step) #f (car max-step))))

(define (chain-internal
         triggered-rule-instances-fn choose-rule-instance-fn  step max-step)
  (cond ((and max-step (> step max-step))
         (display* "Steps = " step " exceeds max-step = " max-step))
        ((member '(STOP) (get-assertions))
         (display* "\nSTOP asserted."))
        (else
         ;; Initiate forward chaining.
         (let* ((triggered-rule-instances (triggered-rule-instances-fn (get-rules)))
                (rule-instance (choose-rule-instance-fn triggered-rule-instances)))
           (if *trace*
               (display* "Step " step ": Triggered rules: "
                         (and triggered-rule-instances
                              (map rule-name triggered-rule-instances))))
           (cond (rule-instance
                  (fire-rule-instance rule-instance)
                  (chain-internal
                   triggered-rule-instances-fn choose-rule-instance-fn
                   (+ 1 step) max-step)
                  )
                 ;; No more triggerable rules...
                 (else (display* "No rule can assert anything new.")))
           )))
  step)

(define (fire-rule-instance rule-instance)
  ;; Do the actions specified by the rule.  The value is irrelevant.
  (do-additions rule-instance)
  (do-deletions rule-instance)
  (do-evals rule-instance)
  (do-sayings rule-instance))

(define (do-additions rule-instance)
  (for-each
   (lambda (x)
     (if *trace* (display* "Rule " (rule-name rule-instance) " adds " x))
     (remember-assertion x))
   (append (rule-thens rule-instance) (rule-adds rule-instance))))

(define (do-deletions rule-instance)
  (for-each
   (lambda (x)
     (if *trace* (display* "Rule " (rule-name rule-instance) " deletes " x))
     (delete-assertion x))
   (rule-deletes rule-instance)))

(define (do-evals rule-instance)
  (for-each scheme-eval
            (rule-evals rule-instance)))

(define (do-sayings rule-instance)
  (for-each (lambda (x) (apply display* x))
            (rule-sayings rule-instance)))

;; CONFLICT RESOLUTION: rule-order (fire first triggered rule-instance)

(define (choose-first-instance rule-instances)
  ;; Select first triggered rule instance.
  (if (pair? rule-instances)
      (first rule-instances)
      ;; no rule instance to fire.
      #f))

(define (try-first-rule rules)
  ;; Check rules and find the first that triggers, returns all the rule
  ;; instances for that rule.
  (there-exists? rules
                 (lambda (rule)
                   (let ((instances (build-rule-instances rule)))
                     (if (null? instances)
                         #f
                         instances)))))

;; A note on terminology.
;; BINDINGS refers to a list of pairs of variable names and values,
;; e.g. ((x 1) (y 2)), i.e. the assignments to a set of variables to get a
;; match between a rule pattern and an assertion.
;; BINDINGS-LIST refers to a list of bindings, i.e. alternative assignments to
;; the same set of vaariables that all yield a match,
;; e.g. ( ((x 1) (y 2))  ((x 3) (y 4)) ...)

(define (build-rule-instances rule)
  ;; For each of the valid bindings, check that the instantiated rule is
  ;; triggered, that is, it adds or deletes something from the assertions (and
  ;; satisfies AND-IF).  Returns a list of triggered rule instances.
  (filter-map
   (lambda (binding) (triggered-rule-instance rule binding))
   ;; List of the bindings determined from the assertions that match all the
   ;; patterns in the IF part of the rule.
   (bindings-list-for-all-patterns
    (rule-ifs rule) (list (empty-bindings)))))

(define (bindings-list-for-all-patterns patterns bindings-list)
  ;; Tries to match all patterns to all assertions using all binding lists.
  (if (null? patterns)
      bindings-list
      (bindings-list-for-all-patterns
       (rest patterns)
       (bindings-list-for-one-pattern (first patterns)
                                      bindings-list))))

(define (triggered-rule-instance rule bindings)
  ;; Pull out the different components of the rule and instantiate them. that is
  ;; replace variables with the matching entry in bindings.  Returns a rule
  ;; instance or #f.
  (let ((i-rule (instantiate-variables rule bindings)))
    ;; Check the additional conditions for firing (beyond matching the
    ;; antecedent to the assertions).  That is,
    ;; (a) The rule adds a new assertion (via THEN or ADD) or
    ;; (b) The rule deletes some asertion (via DELETE)
    ;; and any AND-IF conditions evaluate to true.
    (if (and (or (there-exists?
                  (append (rule-thens i-rule) (rule-adds i-rule))
                  (lambda (x) (not (member x (get-assertions)))))
                 (there-exists?
                  (rule-deletes i-rule)
                  (lambda (x) (member x (get-assertions)))))
             (for-all? (rule-and-ifs i-rule) scheme-eval))
        ;; Passed the test, return the instantiated rule.
        i-rule
        ;; Failed.
        #f)))

(define (bindings-list-for-one-pattern pattern bindings-list)
  ;; Tries to match one pattern to all assertions using all binding lists.
  (append-map
   (lambda (bindings)
     (match-pattern-to-assertions pattern bindings))
   bindings-list))

(define (match-pattern-to-assertions pattern bindings)
  ;; Tries to match one pattern to all assertions using one binding list,
  ;; returns a list of succesful bindings, i.e. a bindings-list
  (filter-map
   (lambda (assertion) (do-match pattern assertion bindings))
   (get-assertions)))

;;;; ASSERTION UTILITIES

(define (delete-assertion assertion)
  ;; Keep assertions that DON'T match specified assertion.
  (set! *assertions*
        (filter (lambda (assert) (not (match assertion assert)))
                *assertions*)
        ))

(define (count-assertions pattern)
  ;; Counts assertions that match specified pattern.
  (let ((count 0))
    (for-each
     (lambda (x)
       (if (match pattern x)
           (set! count (+ count 1))))
     (get-assertions))
    count))

;;;; UTILITIES

(define (append-map fn list)
  ;; Append the result of the map of FN over list.
  (apply append (map fn list)))

(define (filter-map fn l)
  ;; Map fn for each element of l but keep only non-false results.
  (if (null? l)
      '()
      (let ((value (fn (car l))))
        (if value
            (cons value (filter-map fn (cdr l)))
            (filter-map fn (cdr l))))))

