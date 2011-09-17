;;;; -*- mode:scheme -*- ;;;;

;;; Neural Net Training.  Follows terminology in Winston Chap. 22.

;;; Test Data

(define *xor-samples*  '(((1 0) (1.0))
			 ((0 1) (1.0))
			 ((0 0) (0.0))
			 ((1 1) (0.0))))

;;; Rate Constant
(define *rate* 8.0)

;;; Weights used during training
(define *weights* '())

(define *initial-weight-magnitude* 1.0)

;;; Train

;;;  Purpose:	Train the net
;;;  Arguments:	Maximum number of epochs, samples, RMS to stop
(define (train epoch-limit samples initial-weights min-rms)
  (newline)
  (set! *weights* initial-weights)	; will be changed during training
  (let ((sample-count (length samples)))
    (do ((step 0 (+ 1 step))
	 (limit (* epoch-limit sample-count)))
	((cond ((= step limit) #t)
	       ((zero? (modulo step (* 25 sample-count)))
		(let ((rms-error (compute-rms-error samples)))
		  (print-average-error (/ step sample-count) rms-error)
		  (if (< rms-error min-rms) #t #f)))
	       (#t #f)))
      (let* ((sample (nth (modulo step sample-count) samples))
	     (sample-inputs (first sample))
	     (desired-outputs (second sample)))
	(single-step step sample-inputs desired-outputs)))))


;;;  Purpose:	Perform a single training step
;;;  Arguments:	Step number
;;;  		Sample inputs
;;;  		Desired outputs for those inputs
;;;  Remarks:	Weights changed by side effect
(define (single-step step input desired-outputs)
  (let* ((outputs (forward-propagate-output input *weights*))
	 (final-outputs (compute-final-outputs outputs))
	 (output-betas (output-layer-betas final-outputs
					   desired-outputs))
	 (betas (reverse (backward-propagate-beta (reverse outputs)
						  output-betas
						  (reverse *weights*))))
	 (partials (invert-partials (compute-partials betas (cons input outputs)))))
    (set! *weights* (add-by-layers *weights* 
				   (multiply-by-layers *rate* partials)))))

;;; Forward Propagation

;;;  Purpose:	Propagate values from inputs to outputs
;;;  Arguments:	Inputs to net
;;;  Returns:	Outputs of net
(define (forward-propagate-output inputs-to-k remaining-weights)
  (if (null? remaining-weights)
      '()
    (let* ((weights-to-k (first remaining-weights))
	   (output-k (forward-propagate-output-one-layer
		      (cons -1.0 inputs-to-k)
		      weights-to-k)))
      (cons output-k
	    (forward-propagate-output
	     output-k
	     (rest remaining-weights))))))

;;;  Arguments:	Outputs of leftward layer, weights to this layer
;;;  Returns:	Outputs of this layer
(define (forward-propagate-output-one-layer outputs-from-j weights-to-k)
  (map (lambda (weights-j-to-k)
	      (sigmoid (vector-dot-product outputs-from-j weights-j-to-k)))
	  weights-to-k))

;;;  Purpose:	Fetch final outputs from layer-by-layer outputs
;;;  Arguments:	Layer-by-layer outputs
;;;  Returns:	Final outputs
(define (compute-final-outputs outputs)
  (first (last-pair outputs)))

;;; Backward Propagation

;;;  Arguments:	Obvious from names
;;;  Returns:	Betas associated with output layer
(define (output-layer-betas actual-outputs desired-outputs)
  (vector-difference desired-outputs actual-outputs))

;;;  Arguments:	Weights to rightward layer
;;;  		Outputs of rightward layer
;;;   		Betas of rightward layer
;;;  Returns:	Betas of this layer
(define (backward-propagate-beta-one-layer weights-to-k outputs-k betas-k)
  (map (lambda (weights)
	      (reduce + 0.0 (map * 
				  weights
				  (map derivative-of-sigmoid outputs-k)
				  betas-k)))
	  (transform-weights weights-to-k)))

;;;  Purpose:	Propagate betas backward from outputs to inputs
;;;  Arguments:	Outputs of each layer, from outputs to first layer
;;;  		Betas for each layer
;;;  		Weights of each layer, from outputs to inputs
;;;  Returns:	All betas
(define (backward-propagate-beta reversed-outputs betas-k reversed-weights)
  (if (null? reversed-weights)
      '()
    (let* ((outputs-k (first reversed-outputs))
	   (weights-k (first reversed-weights))
	   (betas-j (backward-propagate-beta-one-layer weights-k
						       outputs-k
						       betas-k)))
      (cons betas-k
	    (backward-propagate-beta (rest reversed-outputs)
				     (rest betas-j)
				     (rest reversed-weights))))))
   
;;;  Purpose:	Convert left-to-right weight description to right-to-left
;;;  Arguments:	Left-to-right weight description
;;;  Returns:	Right-to-left weight description
;;;  Remarks:	Transforms a single layer
(define (transform-weights weights)
  (let ((result '()) (upper-bound (length (first weights))))
    (do ((n 0 (+ 1 n)))
	((= n upper-bound) (reverse result))
      (set! result (cons (map (lambda (x) (nth n x)) weights) result)))))


;;;  Purpose:	Convert right-to-left partials description to left-to-right
;;;  Arguments:	Right-to-left partials description
;;;  Returns:	Left-to-right partials description
;;;  Remarks:	Inverts all layers
(define (invert-partials layers)
  (map transform-weights layers))

;;; Compute Partials

;;;  Arguments:	Betas for this layer, outputs from this layer
;;;  Returns:	partials for all weights in this layer
(define (compute-partials-one-layer betas outputs)
  (let ((outputs-i (first outputs))
	(outputs-j (second outputs))
	(betas-j (first betas)))
    (map (lambda (oi)
		(map (lambda (oj bj)
			    (* oi (derivative-of-sigmoid oj) bj))
			    outputs-j
			    betas-j))
            (cons -1 outputs-i))))

;;;  Purpose:	Compute partials for all weights
;;;  Arguments:	All betas, all outputs
;;;  Returns:	All partials
(define (compute-partials betas outputs)
  (if (null? betas)
      '()
    (cons 
     (compute-partials-one-layer betas outputs)
     (compute-partials (rest betas) (rest outputs)))))

;;; Error Computation

;;;  Purpose:	Computs average error for all samples, 
;;;  		where error for one sample is rms error over all outputs
;;;  Arguments:	All samples
;;;  Returns:	Average rms error
(define (compute-rms-error samples)
  (/ (reduce + 0.0
	     (map
	      (lambda (sample) 
		  (vector-rms-differences
		   (compute-final-outputs
		     (forward-propagate-output (first sample) *weights*))
		   (second sample)))
		  samples))
     (length samples)))

;;; Manipulate Weights

;;;  Purpose:	Takes two weight descriptions and adds corresponding elements
;;;  Arguments:	Typically, the current weights and changes
;;;  Returns:	Typically, new weights
(define (add-by-layers weights partials)
  (if (null? weights)
      '()
    (if (number? weights)
	(+ weights partials)
      (cons (add-by-layers (first weights) (first partials))
	    (add-by-layers (rest weights) (rest partials))))))

;;;  Purpose:	Mulitplies each weight by a multiplier
;;;  Arguments:	Typically, a rate and an expression containing partials
;;;  Returns:	Typically, changes to be made
(define (multiply-by-layers multiplier weights)
  (if (null? weights)
      '()
    (if (number? weights)
	(* multiplier weights)
      (cons (multiply-by-layers multiplier (first weights))
	    (multiply-by-layers multiplier (rest weights))))))

;;; Inform User

;;;  Purpose:	Generate formated progress information
;;;  Arguments:	Current epoch number, set of samples
(define (print-average-error epoch rms-error)
  (display* "Epochs: " epoch
	    " Average rms error: " rms-error))

;;; Initial Weight Computation

;;;  Purpose:	Produce initialized weights
;;;  Arguments:	A list of layer sizes, from inputs to outputs
;;;  Returns:	Initialized weights
(define (initialize-weights dimensions)
  (define (make-layer from-count to-count)
    (define (another-random-number counter)
      (if (zero? counter)
	  '()
	  (cons (- (random (* *initial-weight-magnitude* 2)) 
		   *initial-weight-magnitude*)
		(another-random-number (- counter 1)))))
    (if (zero? to-count)
	'()
	(cons (another-random-number (+ 1 from-count))
	      (make-layer from-count (- to-count 1)))))
  (if (null? (rest dimensions))
      '()
      (cons
       (make-layer (first dimensions) (second dimensions))
       (initialize-weights (rest dimensions)))))

;;; Vector Operations

(define (vector-dot-product v1 v2)
  (reduce + 0.0 (map * v1 v2)))

(define (vector-difference v1 v2)
  (map - v1 v2))

(define (vector-squared-differences v1 v2)
  (let ((v (vector-difference v1 v2)))
    (vector-dot-product v v)))

(define (vector-rms-differences v1 v2)
  (let ((v (vector-difference v1 v2)))
    (sqrt (/ (vector-dot-product v v)
	     (length v)))))

;;; Sigmoid Computations

;;;  Arguments:	Input of sigma function
;;;  Returns:	Output of sigmoid function
(define (sigmoid input)
  (if (> input 50.0)
      1.0
      (if (< input -50.0)
	  0.0
	  (/ 1.0 (+ 1 (exp (- input)))))))

;;;  Arguments:	Output of sigmoid function
;;;  Returns:	Derivitive of output of sigmoid function wrt output variable
(define (derivative-of-sigmoid output)
  (* output (- 1 output)))

;;; UTILITIES

;;(define (first x) (car x))
;;(define (second x) (cadr x))
;;(define (third x) (caddr x))
(define (rest x) (cdr x))

;;;  Purpose:	   Use function to combine list items with an initial value
;;;  Example:         (reduce + 1 '(2 3 4))
;;;  Value:           10
(define (reduce function initial-value items)
  (if (null? items)
      initial-value
      (reduce function
              (function initial-value (first items)) ; New initial value
              (rest items))))			     ; New list of items

(define (nth n list-of-elements) (list-ref list-of-elements n))

(define *t:silent* #f)
(define (display* . l)
  ;; Print the list of arguments
  (cond (*t:silent* #f)
	(else
	 (for-each display l)
	 (newline))))
