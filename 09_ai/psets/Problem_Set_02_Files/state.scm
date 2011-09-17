;;;; state.scm

;; Functions to easily load and save program state (ie, sets of variables).
;; There are three different ways to call the save and load functions,
;; although the output file is the same in each case (and they can be
;; interchanged).
;; The functions 'state-save-set' and 'state-load-set' are the simplest;
;; 'state-load-list' offers more flexibility than the other load functions,
;; though (it returns a list of the stored data, while the other two set
;; global variables). See the usage examples at the end of this file.


;; Saves any number of objects to the file 'filename'.
;; Can be called with, for example:
;;   (save-state "myfunc.state" *samples* *constants* status)
;; or whatever is appropriate.

(define (state-save filename . objects)
  (let ((out (open-output-file filename)))
    (for-each (lambda (obj)
		(write-line obj out))
	      objects)
    (close-port out)
    'saved.))


;; Takes a list of objects to write, instead of multiple arguments.
;; May be less convinient, but a closer parallel to the state-load-list function.
;; The above example would be called with:
;;   (save-state-list "myfunc.state" (list *samples* *constants* status))
;; for the same effect.

(define (state-save-list filename object-list)
  (apply state-save (cons filename object-list)))


;; Takes a list of *names* objects to write, instead of multiple arguments.
;; Analogous to the state-load-set function.
;; The above example would be called with:
;;   (save-state-set "myfunc.state" '(*samples* *constants* status))
;; for the same effect. (See below for examples).

(define (state-save-set filename object-name-list)
  (apply state-save (cons filename (map scheme-eval object-name-list))))



;; Loads the objects stored in 'filename', and returns a list containing
;; them (in order).

(define (state-load-list filename)
  (letrec ((in (open-input-file filename))
	   (read-next (lambda (objects-so-far)
			(let ((next (read in)))
			  (if (eof-object? next)
			      (begin (close-port in)
				     (reverse! objects-so-far))
			      (read-next (cons next objects-so-far)))
			  )) ))
    (read-next '()) ))


;; Takes arguments specifying the names (as symbols) of the global variables
;; to be loaded. To load the data specified about, the call would be:
;;   (state-load "myfunc.state" '*samples* '*constants* 'status)
;; Not as clean and scheme-like, but may seem simpler to you. Make sure all
;; of the variables are globally available (and defined) so that they can be set.
;; (Also, with no arguments, acts as a call to 'state-load-list'.)

(define (state-load filename . object-names)
  (if (null? object-names)
      (state-load-list filename)
      (let ((object-values (state-load-list filename)))
	(if (not (= (length object-values) (length object-names)))
	    (error "Different number of variable names and stored values." 
		   (length object-names) (length object-values))
	    (begin (for-each (lambda (name val)
			       ;(display* "Restoring " name " to " val ".")
			       (scheme-eval (list 'set! name (list 'quote val))))
			     object-names object-values)
		   'loaded.)))))


;; Takes an argument that is a list of the names of the variables to be loaded
;; (as symbols). The example call would be:
;;   (state-set "myfunc.state" '(*samples* *constants* status))

(define (state-load-set filename object-name-list)
  (apply state-load (cons filename object-name-list)))



;; Uncomment the appropriate line for your Scheme.

(define (scheme-eval x)
  ;; In MIT Scheme, eval requires a specified environment
  (eval x (the-environment)))
  ;; In SCM, the environment argument is not required
  ;;(eval x))



;;;; Examples of use:

;;  

;(define foo #f) (define bar #f) (define baz #f) (define goo #f)
;
;(define (test-set) (begin
;		     (set! foo 7)
;		     (set! bar "hi there")
;		     (set! baz 'hello)
;		     (set! goo '(1 2 'three "four"))
;                    'ok))
;
;(define (test-show) (begin
;		      (pp foo)
;		      (pp bar)
;		      (pp baz)
;		      (pp goo)))
;
;(define (test-clear) (begin
;		       (set! foo #f)
;		       (set! bar #f)
;		       (set! baz #f)
;		       (set! goo #f)
;                      'ok))

;;;; First Method:

;(test-set)
;(test-show)
;(state-save "test.state" foo bar baz goo)
;
;(test-clear)
;(test-show)
;(state-load "test.state" 'foo 'bar 'baz 'goo)
;
;(test-show)


;;;; Second Method, for the same effect:

;(test-set)
;(test-show)
;(state-save-list "test.state" (list foo bar baz goo))
;
;(test-clear)
;(test-show)
;(let ((state (state-load-list "test.state")))
;  (set! foo (list-ref state 0))
;  (set! bar (list-ref state 1))
;  (set! baz (list-ref state 2))
;  (set! goo (list-ref state 3)) )
;	     
;(test-show)

;; Note that this is the most flexible, since the variables do not
;; have to be global and the loaded list can be processed as desired.


;;;; Third Method, for the same effect: (Perhaps the easiest)

;  (define *state-variables* '(foo bar baz goo))
;(test-set)
;(test-show)
;(state-save-set "test.state" *state-variables*)
;
;(test-clear)
;(test-show)
;(state-load-set "test.state" *state-variables*)
;	     
;(test-show)


