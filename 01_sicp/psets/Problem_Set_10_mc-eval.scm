;;; mc-eval-ps10.scm
;;;
;;; Metacircular evaluator and analyzing evaluator from section 4.1 of
;;; SICP mc-eval and mc-apply are the heart of the evaluator

;;; NOTE!
;;;
;;; This code supersedes the code handed out in lecture yesterday.
;;; It includes the special form and, which was not included in 
;;; yesterday's notes.  There are also minor modifications to correct
;;; some errors and to add some primitive procedures.

(define (mc-eval exp env)
  (cond ((self-evaluating? exp)
	 exp)
        ((variable? exp)
	 (lookup-variable-value exp env))
        ((quoted? exp)
	 (text-of-quotation exp))
        ((assignment? exp)
	 (eval-assignment exp env))
        ((definition? exp)
	 (eval-definition exp env))
        ((if? exp)
	 (eval-if exp env))
	((and? exp)
	 (eval-and exp env))
        ((lambda? exp)
	 (make-procedure (lambda-parameters exp) (lambda-body exp) env))
        ((begin? exp)
	 (eval-sequence (begin-actions exp) env))
        ((cond? exp)
	 (mc-eval (cond->if exp) env))
        ((application? exp)
	 (mc-apply (mc-eval (operator exp) env)
		   (list-of-values (operands exp) env)))
        (else (error "Unknown expression type -- MC-EVAL"))))


(define (mc-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
	  (extend-environment (procedure-parameters procedure)
			      arguments
			      (procedure-environment procedure))))
        (else (error "Unknown procedure type -- MC-APPLY" procedure))))


;;; LIST-OF-VALUES
;;;
;;; Used to produce the list of arguments to which
;;; a procedure is to be applied.

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (mc-eval (first-operand exps) env)
	    (list-of-values (rest-operands exps) env))))


;;; EVAL-IF
;;;
;;; Evaluates the predicate part of an if expression in the given
;;; environment.

(define (eval-if exp env)
  (if (eq-adu-true? (mc-eval (if-predicate exp) env))
      (mc-eval (if-consequent exp) env)
      (mc-eval (if-alternative exp) env)))


;;; EVAL-SEQUENCE
;;;
;;; Used by mc-apply to evaluate the sequence of expressions in a
;;; procedure body and by eval to evaluate the sequence of expressions
;;; in a begin expression.

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (mc-eval (first-exp exps) env))
        (else (mc-eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))


;;; EVAL-ASSIGNMENT
;;;
;;; Handles assignment to variables.

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (mc-eval (assignment-value exp) env)
                       env)
  'ok)


;;; EVAL-DEFINITION 
;;;
;;; handles definition of variables

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (mc-eval (definition-value exp) env)
                    env)
  'ok)


;;; Following are the procedures that define the syntax of our language We
;;; preface the usual scheme commands with "adu:" to allow us to differentiate
;;; when we are using the metacircular evaluator to evaluate expressions (it
;;; will only be used for "adu:" expressions) and when scheme code is being
;;; evaluated by the usual scheme interpreter


;;; SELF-EVALUATING?
;;;
;;; Self-evaluating entities: numbers and strings

(define (self-evaluating? exp)
  (cond ((number? exp) true)
	((string? exp) true)
	(else false)))


;;; VARIABLE?
;;;
;;; Variables

(define (variable? exp) (symbol? exp))


;;; QUOTED?, TEXT-OF-QUOTATION
;;;
;;; Quotations

(define (quoted? exp) 
  (tagged-list? exp 'adu:quote))

(define (text-of-quotation exp) (cadr exp))


;;; TAGGED-LIST?
;;;
;;; Special forms (in general)

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))


;;; ASSIGNMENT?, ASSIGNMENT-selectors
;;;
;;; Assignment--- set!

(define (assignment? exp) 
  (tagged-list? exp 'adu:set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))


;;; DEFINITION?, DEFINITION-selectors
;;;
;;; Definitions.

(define (definition? exp)
  (tagged-list? exp 'adu:define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)		;formal parameters
		   (cddr exp))))        ;body

;;; LAMBDA?, LAMBDA-selectors, MAKE-LAMBDA
;;;
;;; Lambda expressions: test, selectors and constructor.

(define (lambda? exp) (tagged-list? exp 'adu:lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'adu:lambda (cons parameters body)))


;;; IF, IF-selectors
;;;
;;; If conditionals: test, selectors, and constructor.  The constructor for IF
;;; expressions to be used for COND->IF to transform COND expressions into IF
;;; expressions.

(define (if? exp) (tagged-list? exp 'adu:if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'adu:if predicate consequent alternative))



;;; EVAL-AND, AND? AND-CLAUSES
;;;
;;; Procedures to implement adu:and as a special form in our new language

(define (eval-and exp env)
  (define (iter clauses)
    (if (null? clauses)
	#t
	(if (eq-adu-false? (mc-eval (car clauses) env))
	    #f
	    (iter (cdr clauses)))))
  (iter (and-clauses exp)))

(define (and? exp) (tagged-list? exp 'adu:and))

(define (and-clauses exp) (cdr exp))

;;; BEGIN?, BEGIN-ACTIONS, LAST-EXP?, FIRST-EXP, REST-EXPS
;;;
;;; Begin expressions (a.k.a. sequences).

(define (begin? exp) (tagged-list? exp 'adu:begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))


;;; SEQUENCE->EXP, MAKE-BEGIN
;;;
;;; Constructor sequence->exp for use by cond->if that transforms a sequence
;;; into a single expression, using begin if necessary.

(define (sequence->exp seq)
  (cond ((null? seq) seq)
	((last-exp? seq) (first-exp seq))
	(else (make-begin seq))))

(define (make-begin seq) (cons 'adu:begin seq))


;;; APPLICATION?, procedure selectors
;;;
;;; Procedure applications, operator and operand selectors. 

(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? args) (null? args))

(define (first-operand args) (car args))

(define (rest-operands args) (cdr args))


;;; COND?, COND-selectors, COND->IF, EXPAND-CLAUSES
;;;
;;; cond conditionals: a derived expression

(define (cond? exp) (tagged-list? exp 'adu:cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'adu:else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
	    (rest (cdr clauses)))
	(if (cond-else-clause? first)
	    (if (null? rest)
		(sequence->exp (cond-actions first))
		(error "ELSE clause isn't last -- COND->IF"
		       clauses))
	    (make-if (cond-predicate first)
		     (sequence->exp (cond-actions first))
		     (expand-clauses rest))))))

;;; DATA STRUCTURES FOR OUR EVALUATOR
;;;

;;; EQ-ADU-TRUE?, EQ-ADU-FALSE?
;;;
;;; For conditionals, we accept anything that isn't the explicit false object
;;; to be true.

(define (eq-adu-true? x)
  (not (eq? x false)))

(define (eq-adu-false? x)
  (eq? x false))


;;; MAKE-PROCEDURE, COMPOUND-PROCEDURE?, PROCEDURE-selectors
;;;
;;; Constructors and selectors for representing procedures.

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))


;;; OPERATIONS ON ENVIRONMENTS
;;; 

;;; ENCLOSING-ENVIRONMENT, FIRST-FRAME, THE-EMPTY-ENVIRONMENT
;;;
;;; An environment is represented as a list of frames.

(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())


;;; MAKE-FRAME, FRAME-VARIABLES, FRAME-VALUES, ADD-BINDING-TO-FRAME!
;;;
;;; Each frame of an environment is represented as a pair of lists:
;;; a list of the variables bound in that frame and a list of the 
;;; associated values.

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))

(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

;;; EXTEND-ENVIRONMENT
;;; 
;;; To extend an environment by a new frame that associates variables with
;;; values, we make a frame consisting of the list of variables and the list
;;; of values, and we adjoin this to the environment -- signal an error if the
;;; number of variables does not match the number of values

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
	  (error "Too many arguments supplied" vars vals)
	  (error "Too few arguments supplied" vars vals))))

;;; LOOKUP-VARIABLE-VALUE
;;; 
;;; To look up a variable in an environment, we scan the list of variables in
;;; the first frame.  If we find the desired variable, we return the
;;; corresponding element in the list of values.  If we do not find the
;;; variable in the current environment, we search the enclosing environment,
;;; and so on.  If we reach the empty environment, signal an "unbound
;;; variable" error.

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
	     (env-loop (enclosing-environment env)))
	    ((eq? var (car vars))
	     (car vals))
	    (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable" var)
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame)
		(frame-values frame)))))
  (env-loop env))


;;; SET-VARIABLE-VALUE!
;;; 
;;; To set a variable to a new value in a specified environment, we scan for
;;; the variable, just as in lookup-variable value, and change the
;;; corresponding value when we find it

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
	     (env-loop (enclosing-environment env)))
	    ((eq? var (car vals))
	     (set-car! vals val))
	    (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable -- SET!" var)
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame)
		(frame-values frame)))))
  (env-loop env))


;;; DEFINE-VARIABLE!
;;; 
;;; To define a variable, we search the first frame for a binding for the
;;; variable, and change the binding if it exists (just as in
;;; set-variable-value!).  If no such binding exists, we adjoin one to the
;;; first frame.

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
	     (add-binding-to-frame! var val frame))
	    ((eq? var (car vars))
	     (set-car! vals val))
	    (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
	  (frame-values frame))))


;;; The following functions are used to allow the metacircular evaluator to be
;;; run

(define (setup-environment)
  (let ((initial-env
	 (extend-environment (primitive-procedure-names)
			     (primitive-procedure-objects)
			     the-empty-environment)))
    (define-variable! 'true  true  initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'adu:car car)
	(list 'adu:cdr cdr)
	(list 'adu:cons cons)
	(list 'adu:null? null?)
	(list 'adu:+ +)
	(list 'adu:- -)
	(list 'adu:* *)
	(list 'adu:/ /)
	(list 'adu:= =)
	(list 'adu:> >)
	(list 'adu:< <)))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define apply-in-underlying-scheme apply)

(define mc-input-prompt ";;; MC-Eval input: ")
(define mc-output-prompt "\n;;; MC-Eval value: ")

(define analyze-input-prompt ";;; A-Eval input: ")
(define analyze-output-prompt "\n;;; A-Eval value: ")

(define current-evaluator     mc-eval)
(define current-input-prompt  mc-input-prompt)
(define current-output-prompt mc-output-prompt)

(define (driver-loop)
  (prompt-for-input current-input-prompt)
  (let ((input (read)))
    (let ((output (current-evaluator input the-global-environment)))
      (announce-output current-output-prompt)
      (user-print output)))
  (driver-loop))

(define (start-analyze)
  (set! current-evaluator      (lambda (exp env) ((analyze exp) env)))
  (set! current-input-prompt   analyze-input-prompt)
  (set! current-output-prompt  analyze-output-prompt)
  (set! the-global-environment (setup-environment))
  (driver-loop))

(define (start-mc-eval)
  (set! current-evaluator mc-eval)
  (set! current-input-prompt mc-input-prompt)
  (set! current-output-prompt mc-output-prompt)
  (set! the-global-environment (setup-environment))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string))

(define (announce-output string)
  (display string))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure 
		     (procedure-parameters object)
		     (procedure-body object)
		     '<procedure-env>))
      (display object)))

(define the-global-environment (setup-environment))

(define *unparser-list-depth-limit* 7)
(define *unparser-list-breadth-limit* 10)

;;; DISPLAY
;;;
;;; Supplants the version in the system and actually HAS breadth and depth limits.
;;; (pz, 10/2000)

(load-option 'format)

(define (display obj)
  (let ((breadth-limit (or *unparser-list-breadth-limit* 100))
	(depth-limit   (or *unparser-list-depth-limit* 100)))
    (define (nd obj depth breadth)
      (cond ((< breadth-limit breadth)
	     (format #t "..."))
	    ((< depth-limit depth)
	     (format #t "..."))
	    ((not (pair? obj))
	     (format #t "~A" obj))
	    ((pair? (car obj))
	     (format #t "(")
	     (nd (car obj) (inc depth) breadth)
	     (format #t ")")
	     (if (not (null? (cdr obj)))
		 (begin
		   (format #t " ")
		   (nd (cdr obj) depth (inc breadth)))))
	    (else
	     (if (null? (cdr obj))
		 (format #t "~A" (car obj))
		 (begin
		   (format #t "~A" (car obj))
		   (format #t " ")
		   (nd (cdr obj) depth (inc breadth)))))))
    (cond ((pair? obj)
	   (format #t "(")
	   (nd obj 1 1)
	   (format #t ")"))
	  (else
	   (format #t "~A" obj)))))


;;;
;;; end.

