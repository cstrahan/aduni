;;;; -*- mode:Scheme -*- ;;;;
;;
;; rule-utils.scm
;;
;; This file contains infrastructure for rule-based systems
;; Derives from an implementation by Patrick Winston.  Extensively
;; modified by Tomas Lozano-Perez.

;; Ignore this; it aids compilation but affects nothing else.
(declare (usual-integrations))
(declare (integrate rest))

;;; COMPATIBILITY

;; Uncomment the appropriate line for your Scheme.
(define (scheme-eval x)
  ;; In MIT Scheme, eval requires a specified environment
  (eval x (the-environment)))
  ;; In SCM, the environment argument is not required
  ;;(eval x))

;; Not every version of Scheme has these, let's make sure they are there.

(define first car)
(define rest cdr)
(define second cadr)
(define third caddr)

;; Returns the first non-false value returned by applying fn to the elements of l
(define (there-exists? l fn)
  (if (null? l)
      #f
      (or (fn (car l))
          (there-exists? (cdr l) fn))))

;; Returns non-false only if fn is non-false when applied to every element of l
(define (for-all? l fn)
  (if (null? l)
      #t
      (and (fn (car l))
           (for-all? (cdr l) fn))))

;;; Keep only elements of l for which pred is not false.
(define (filter pred l)
  (if (null? l)
      '()
      (if (pred (car l))
          (cons (car l) (filter pred (cdr l)))
          (filter pred (cdr l)))))

;; BASIC RULE OPERATIONS

;; The top-level list of rules.
(define *rules* '())

;; Removes all current rules.
(define (clear-rules)
  (set! *rules* '()))

;; Adds a new rule to *rules* or updates an existing rule of the same name.
(define (remember-rule irule)
  (let* ((rule (process-vars irule))
         (name (rule-name rule))
         (prev-rule (assoc name *rules*)))
    (if prev-rule
        ;; existing rule, update it.
        (update-rule prev-rule rule)
        ;; Adds a single (new) rule to (end of) *rules*
        (if (member rule *rules*)
            *rules*
            (set! *rules* (append *rules* (list rule)))
            ))))

;; replaces old rule with new - assuming same name
(define (update-rule old new)
  (if (equal? (rule-name old) (rule-name new))
      ;; relies on the fact that name is car of list and body is the rest.
      (set-cdr! old (rest new))
      (error "Can only update rules of the same name" old new)
      ))

;; Hide implementation
(define (get-rules) *rules*)

;;;; ACCESS FUNCTIONS FOR RULE ELEMENTS

;; A rule is stored as a list.  Its first element is an arbitrary
;; name, the rest are for the form (... Marker a b c Marker...)  where
;; the markers are IF, THEN, etc.  These functions return a list of
;; all the entries after a specified marker.
(define (extract-from-rule marker rule)
  ;; Construct a list of elements in the rule following the specified marker.
  (let ((fragment (member marker rule)))
    ;; (member x l) returns a sublist of l starting with x or #f.
    (if fragment
        (extract-expressions (rest fragment))
        '())))

;; This returns a list of the elements following a marker (such as IF,
;; THEN ...)  It relies on the fact that markers will fail the list?
;; test.
(define (extract-expressions rule)
  (cond ((null? rule) '())
        ((list? (first rule))
         (cons (first rule) (extract-expressions (rest rule))))
        (else '())))

;; Accessors for rule components - most of these only for forward-chaining.
(define (rule-name rule) (first rule))
(define (rule-body rule) (rest rule))
(define (rule-ifs rule) (extract-from-rule 'if rule))
(define (rule-thens rule) (extract-from-rule 'then rule))
(define (rule-then rule) (first (rule-thens rule)))
(define (rule-adds rule) (extract-from-rule 'add rule))
(define (rule-deletes rule) (extract-from-rule 'delete rule))
(define (rule-and-ifs rule) (extract-from-rule 'and-if rule))
(define (rule-evals rule) (extract-from-rule 'evaluating rule))
(define (rule-sayings rule) (extract-from-rule 'saying rule))

;; Scheme note: the use of . in the argument list allows a function
;; to be called with a variable number of arguments.  A list of any
;; remaining arguments "left over" after binding required arguments
;; are bound to the variable after the dot.  So, remember-rules can be
;; called with any number of rules (see examples, e.g. zoo.scm).
(define (remember-rules . rules)
  ;; Adds a list of rules to *rules*
  (for-each remember-rule rules))

;; Like remember-rules but given an explicit list argument.
(define (remember-rules-list rules)
  ;; Adds a list of rules to *rules*
  (for-each remember-rule rules))

;; BASIC ASSERTION OPERATIONS

;; The top-level list of assertions.
(define *assertions* '())

;; Removes all current assertions.
(define (clear-assertions)
  (set! *assertions* '()))

;; Adds a single assertions to (end of) *assertions*, if it is distinct.
;; This is done by a side-effect to the *assertions* list.
(define (remember-assertion assertion)
  ;; loop is only called when l has at least one entry.
  (define (loop l)
    (or (equal? assertion (first l))        ; already there, stop.
        (if (null? (rest l))
            ;; last assertion, modify last cell to point to the new assertion.
            (set-cdr! l (list assertion))
            (loop (rest l)))))
  (if (null? (variables-in-thing assertion))
      (if (null? *assertions*)
          (set! *assertions* (list assertion))
          (loop *assertions*))
      (error "Assertion has variables in it,
probably produced by a rule with a variable in the consequent
that is not present in the antecedent!" assertion)))

;; Analogous to remember-rules.
(define (remember-assertions . assertions)
  ;; Adds a list of assertions to *assertions*, maintains the order in input.
  (for-each remember-assertion assertions))

;; Like remember-assertions but given an explicit list argument.
(define (remember-assertions-list assertions)
  ;; Adds a list of assertions to *assertions*, maintains the order in input.
  (for-each remember-assertion assertions))

;; Print the assertions, all those in *assertions* when no arguemt.
(define (display-assertions . arg)
  (for-each (lambda (assertion) (display* assertion))
            (if (null? arg)
                (get-assertions)
                (first arg))))

;; Hide the implementation
(define (get-assertions) *assertions*)

;; VARIABLES

;; A test for a (? x) type of variable.
(define (simple-variable? x)
  (and (pair? x) (eq? '? (first x)) (symbol? (second x))))

;; A test for a (* x) type of variable.
(define (segment-variable? x)
  (and (pair? x) (eq? '* (first x)) (symbol? (second x))))

;; A test for a (? _) or (* _) type of variable.
(define (nameless-variable? x)
  (and (pair? x) (memq (first x) '(? *)) (eq? '_ (second x))))

;; Get the name for a variable, given (? var)
(define (variable-name x) (second x))

;; Constructors
(define (make-simple-variable name) (list '? name))

(define (make-segment-variable name) (list '* name))

;; Find all the variables in a tree structure.
(define (variables-in-thing thing)
  (let ((answer '()))
    (define (do-it piece)
      (cond ((or (simple-variable? piece)
                 (segment-variable? piece))
             (set! answer (cons-new piece answer)))
            ((pair? piece)
             (do-it (first piece))
             (do-it (rest piece)))))
    (do-it thing)
    answer))

;; Convert ?x into (? x) anywhere in input list
(define (process-vars l)
  (cond ((null? l) '())
        ((symbol? l)
         (let ((name (symbol->string l)))
           (if (and (> (string-length name) 1)
                    (or (char=? #\? (string-ref name 0))
                        (char=? #\* (string-ref name 0))))
               (list (string->symbol (substring name 0 1))
                     (string->symbol (substring name 1 (string-length name))))
               l)))
        ((pair? l) (cons (process-vars (car l)) (process-vars (cdr l))))
        (else l)))

;;;; BINDINGS ABSTRACTION

;; initial value of bindings
(define *null-bindings* '(bindings))

;; Hide the implementation
(define (empty-bindings) *null-bindings*)

;; Test
(define (bindings? x)
  (and (pair? x) (eq? (first x) 'bindings)))

;; a single binding is (key value)
(define make-binding list)

;; Accessors
(define (binding-key binding) (first binding))

(define (binding-value binding) (second binding))

;; adds a new binding for a variable to a bindings list.
;; a variable expression is of the form (? x) or (* x)
(define (add-binding variable-expression datum bindings)
 (if (nameless-variable? variable-expression)
     ;; If it is a nameless variable, it matches anything but does not add bindings
     bindings
     ;; add a new binding, remembering that the car of the list is the symbol 'bindings
     (cons (first bindings)
           (cons (list (variable-name variable-expression) datum)
                 (rest bindings)))))

;; returns the binding for a variable (or #f if none)
;; a variable expression is of the form (? x) or (* x)
(define (find-binding variable-expression bindings)
  (if (and bindings
           (not (nameless-variable? variable-expression)))
    ;; If it's a nameless variable, don't look for binding.  Otherwise, use
    ;; assoc to find the binding.  (assoc x y) looks for a list in y whose car
    ;; is x and returns that, which is just what we need.
    (assoc (variable-name variable-expression)
           ;; Skip the symbol at the beggining of bindings list.
           (rest bindings))
    #f))

;; Given two bindings list, return a merged one.
(define (merge-bindings b1 b2)
  (if (and (bindings? b1) (bindings? b2))
      (cons 'bindings (append (rest b1) (rest b2)))
      (error "Merge-bindings requires two bindings lists:" b1 b2)))

;; INSTANTIATE VARIABLES

(define (instantiate-variables pattern bindings)
  ;; Replaces variables by their values specified in bindings.
  (cond ((not (pair? pattern)) pattern)
        ((nameless-variable? pattern)        ; special case
         pattern)
        ((simple-variable? pattern)
         (let ((binding (find-binding pattern bindings)))
           (if binding
               (binding-value binding)
               ;; If variable has no bindings, leave it alone.
               pattern
               )))
        (else (cons (instantiate-variables (first pattern) bindings)
                    (instantiate-variables (rest pattern) bindings)))))

;; KNOWLEDGE FILES
;; Read in an initialize rules, assertions and test functions.
;; A typical file would look like:
;; assertions
;; (parent ...)
;; (parent ...)
;; rules
;; (r1 if ... then ...)
;; code
;; (define (test-parent) ...)

(define (read-k-file filename)
  (with-input-from-file filename
    (lambda ()
      (let ((rules '())
            (assertions '())
            (mode #f))
        (do ((input (read) (read)))
            ((eof-object? input)
             (clear-rules)
             (apply remember-rules (reverse rules))
             (clear-assertions)
             (apply remember-assertions (reverse assertions)))
          (display input) (newline)
          (cond ((memq input '(rules assertions code))
                 (set! mode input))
                ((eq? mode 'rules)
                 (set! rules (cons input rules)))
                ((eq? mode 'assertions)
                 (set! assertions (cons input assertions)))
                ((eq? mode 'code)
                 (scheme-eval input))
                (else
                 (error "Unknown mode: " mode)))
          )))))

;; PRINTING

(define *t:silent* #f)                        ; if we want no output, set to #f

(define (display* . l)
  ;; Print the list of arguments
  (cond (*t:silent* #f)
        (else
         (for-each display l)
         (newline))))


;; Simple printing utilities.

;; Used to indent printouts.
(define *indent* 0)

(define (indent)
  (define (spaces n)
    (if (= n 0)
        ""
        (string-append " " (spaces (- n 1)))))
  (spaces *indent*))

;; Simple utilities.

(define (cons-new x list)
  (if (member x list)
      list
      (cons x list)))



