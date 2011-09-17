;;; -*- mode:Scheme -*- ;;;;

;;; To build a tree:
;;; (read-training-data "file")
;;; (make-tree *training-data*)
;;; (show-tree)
;;; To test a tree (created as above)
;;; (read-test-data "file")
;;; (test *test-data* identify)

;;;; GLOBAL VARIABLES

(define *tree* '())             ; Assigned to an identification tree
(define *verbose* #f)

;;;; INTERMEDIATE NODE CONSTRUCTOR AND ACCESS FUNCTIONS

(define (make-branching-node dimension
			     threshold
			     left-branch
			     right-branch)
  (list 'branching-node dimension threshold left-branch right-branch))

(define (branching-node? x)
  (and (list? x) (eq? 'branching-node (first x))))

(define (branching-node-dimension x)
  (if (branching-node? x) (list-ref x 1) (error 'accessor "Error in accessor")))

(define (branching-node-threshold x)
  (if (branching-node? x) (list-ref x 2) (error 'accessor "Error in accessor")))

(define (branching-node-left-branch x)
  (if (branching-node? x) (list-ref x 3) (error 'accessor "Error in accessor")))

(define (branching-node-right-branch x)
  (if (branching-node? x) (list-ref x 4) (error 'accessor "Error in accessor")))

;;;; TERMINAL NODE CONSTRUCTOR AND ACCESS FUNCTIONS

(define (make-terminal-node samples identity)
  (list 'terminal-node samples identity))

(define (terminal-node? x)
  (and (list? x) (eq? 'terminal-node (first x))))

(define (terminal-node-samples x)
  (if (terminal-node? x) (list-ref x 1) (error 'accessor "Error in accessor")))

(define (terminal-node-identity x)
  (if (terminal-node? x) (list-ref x 2) (error 'accessor "Error in accessor")))

;;;; TREE CONSTRUCTOR

(define *maximum-depth* 0)		; A global used by MAKE-TREE and
					; MAKE-TREE-AUX

;;;  Purpose:	Supplies arguments to MAKE-TREE-AUX
;;;             Assigns value to *tree*.
;;;             Initializes *maximum-depth*.
(define (make-tree data)
  (set! *maximum-depth* 0)
  (cond ((and (list? data) (not (null? data)))
	 (set! *tree* (make-tree-aux data 0))
	 (display* "\nThe maximum depth of the tree is " *maximum-depth*))
	(else
	 (display* "\nNo training data available.")))
  #t)

;;;  Purpose:	Construct a identification tree from samples.
;;;  Returns:	A branching node.
(define (make-tree-aux samples level)
  ;; Keep track of the maximum depth of recursion, which equals the tree depth:
  (set! *maximum-depth* (max *maximum-depth* level))
  (let ((dimension-count (length (data-point-features (first samples))))
	(disorder 10000) ; I.e., infinity
	(dimension 0)
	(threshold 0)
	(left-samples '())
	(right-samples '()))
    ;; Find the dimension and threshold that produces the minimum disorder.
    ;; First, try each dimension:
    (for-each
     (lambda (d)
       (display* "Trying thresholds in dimension " d)
       ;; During which, try each threshold:
       (for-each
	(lambda (t)
	  (let* ((split (score-threshold t d samples))
		 (score (split-score split)))
	    (if *verbose*
		(display* "  Threshold = " t ", disorder = " score))
	    ;; If the current score is lower than the best so far,
	    ;; then reset variables accordingly:
	    (cond ((< score disorder)
		   (display* "Best disorder --> " score)
		   (set! disorder score)
		   (set! dimension d)
		   (set! threshold t)
		   (set! left-samples (split-left-samples split))
		   (set! right-samples (split-right-samples split))))))
	;; Supply a list of thresholds:
	(compute-thresholds-in-dimension d samples)))
     ;; Supply a list of dimensions:
     (list-dimensions dimension-count))
    ;; Check for still-'infinite' disorder
    (if (= disorder 10000)
	(begin 
	  (display* "\nFailing to split - cannot reduce disorder."
		    " Leaving node with disorder " 
		    (disorder-of-set (length samples) (map data-point-class samples)))
	  (make-terminal-node samples (most-common-class samples)))
	(begin
	  ;; At this point, announce the disorder minimizing dimension and threshold:
	  (display* "\nSplitting in dimension " dimension
		    " at threshold " threshold
		    " with disorder " disorder)
	  ;; Make a new node, using the discovered dimension and threshold:
	  (make-branching-node
	   dimension
	   threshold
	   ;; Also, if the remaining samples in the left branch suggest multiple
	   ;; predictions ...
	   (if (test-for-variation left-samples)
	       ;; then make another branching node on the left side,
	       (make-tree-aux left-samples (+ 1 level))
	       ;; otherwise, make a terminal node:
	       (make-terminal-node left-samples
				   ;; Allows for mixed class terminals
				   (most-common-class left-samples)))
	   ;; Repeat for the right branch:
	   (if (test-for-variation right-samples)
	       (make-tree-aux right-samples (+ 1 level))
	       (make-terminal-node right-samples
				   ;; Allows for mixed class terminals
				   (most-common-class right-samples))))))))

;;;  Purpose:	See if the samples vary:
;;;  Returns:	#t or #f
(define (test-for-variation samples)
  (if (null? samples)
      #f    ;; No variation if no samples!!
     (let ((reference (data-point-class (first samples))))
       ;; Define a testing loop:
       (define (loop others)
         (if (null? others)
	     ;; If no more samples to test, there is no variation:
	     #f
	     ;; If there is at least one, compare with reference;
	     ;; If same, keep going; if not, there is variation:
	     (if (equal? reference (data-point-class (first others)))
	         (loop (rest others))
	         #t)))
       ;; If there is just one sample ...
       (if (null? (rest samples))
	   ;; there is no variation:
	   #f
	   ;; otherwise, launch the testing loop:
	   (loop (rest samples))))))

;;;  Purpose:         Given a number of dimensions, return a list:
;;;  Sample argument: 4
;;;  Sample value:    (0 1 2 3)
(define (list-dimensions count)
  (define (loop n)
    (if (= n count)
	'()
	(cons n (loop (+ 1 n)))))
  (loop 0))

;;;  Purpose:	   Given a list of numbers, return all intermediate numbers:
;;;  Sample argument: (1 2 2 3 3 4 6)
;;;  Sample value:	   (1.5 2.5 3.5 5.0)
(define (compute-thresholds numbers)
  (let ((numbers (remove-duplicates (sort numbers <))))
    ;; Compute average
    (map (lambda (x y) (* 0.5 (+ x y)))     
	 (reverse (rest (reverse numbers)))
	 (rest numbers))))

;;;  Purpose:	Find potential thresholds
;;;  Returns:      A list of thresholds
;;;  Remark:       COMPUTE-THRESHOLDS does the work
(define (compute-thresholds-in-dimension dimension samples)
  (compute-thresholds
   (map (lambda (x) (list-ref (data-point-features x) dimension))
	samples)))

;;;  Purpose:	Compute disorder score for given dimension and threshold.
;;;  Returns:	A list of the score and two lists into which the samples
;;;             are divided by the threshold in the dimension
(define (score-threshold threshold dimension samples)
  (let ((left-samples '())
	(right-samples '()))
    ;; Loop over the samples:
    (for-each
     (lambda (x)
       ;; If the appropriate attribute value is less than the threshold ...
       (if (< (list-ref (data-point-features x) dimension)
	      threshold)
	   ;; Add to variable describing the left branch:
	   (set! left-samples (cons x left-samples))
	   ;; Otherwise, add to variable describing the right branch:
	   (set! right-samples (cons x right-samples))))
     samples)
    ;; Given the way the threshold divides the samples ...
    (make-split
     ;; Return the disorder score:
     (disorder-of-threshold left-samples right-samples)
     ;; Along with the way the samples were divided:
     left-samples
     right-samples)))

;;; SPLITS

(define make-split list)		; Constructor

(define split-score first)		; Getters
(define split-left-samples second)
(define split-right-samples third)

;;;; DISORDER CALCULATIONS

;;; Computes disorder associated with a particular threshold.
;;;; From problem set:
(define (disorder-of-threshold
         left-samples
         right-samples)
  (let* ((left-count (length left-samples))
         (right-count (length right-samples))
         (total-count (+ left-count right-count)))
    (if (> total-count 0)
      (+ (* (/ left-count total-count)
            (disorder-of-set
              left-count
              (map data-point-class left-samples)))
         (* (/ right-count total-count)
            (disorder-of-set
              right-count
              (map data-point-class right-samples))))
      (error "No data in disorder-of-threshold."))))



;;;  Purpose: Compute disorder of a set
;;;  Returns: Disorder value
(define (disorder-of-set size set)
  (let* ((elts				; distinct labels
	  (remove-duplicates set))
	 (counts			; count of each label
	  (map (lambda (x) (count x set)) elts))
	 (disorder 
	  (apply + (map (lambda (x) (compute-disorder-term x size))
			counts))))
    (if *verbose*
	(display* "   " (map cons elts counts) " -> " disorder))
    disorder
    ))

(define (compute-disorder-term x total)
  (let ((ratio (/ x total)))
    (- (* ratio (log2 ratio)))))

(define (log2 x) (/ (log x) (log 2)))

;;;; DISPLAY TREE

;;;   Purpose:	Supply arguments to show-tree-aux.
(define (show-tree)
  (show-tree-aux *tree* 0 #f)
  (newline)
  'done)

;;;  Purpose:	Display a tree using indentation to indicate level
;;;  Arguments:	The root node of the tree, the level, and the direction

(define (show-tree-aux node level branch)
  (cond ((branching-node? node)
	 ;; It's a branching node; describe it:
	 (display* (indent level)
		   ;; Usually BRANCH's value is not #f, but not
		   ;; when SHOW-TREE-AUX is called by SHOW-TREE:
		   (if branch branch "Top")
		   " a split on dimension "
		   (branching-node-dimension node)
		   " [at " (branching-node-threshold node) "]")
	 ;; Recurse left:
	 (show-tree-aux
	  (branching-node-left-branch node) (+ 1 level) "Left branch")
	 ;; Recurse right:
	 (show-tree-aux
	  (branching-node-right-branch node) (+ 1 level) "Right branch"))
	(else
	 ;; It is a terminal node; describe it:
	 (let ((count (length (terminal-node-samples node))))
	   (display* (indent level)
		     "Answer: " (terminal-node-identity node)
		     " [" count " sample"
		     ;; Get the english correct:
		     (if (> count 1) "s" "")
		     "]"
		     ))))
  #t)

;;;   Purpose:	Indent by as many spaces as node is deep.
(define (indent depth)
  (make-string depth #\space))

;;;; IDENTIFICATION

;;;  Purpose:	To perform a prediction using an identification tree.
;;;  Argument:	A list of a prediction and various attribute values
;;;  Returns:	The best guess for an unknown's prediction.

(define (identify unknown)
 (let* ((result (find-answer unknown *tree* 0))
	(prediction (answer-prediction result))
	(supporter-count (answer-support result)))
      (display* "The winner is " prediction
	      " according to " supporter-count 
	      " example" (if (= 1 supporter-count) "" "s")
	      ".  Correct is " (data-point-class unknown)
	      "."
	      )
      prediction))

;;;   Purpose:	Find answer using identification tree.
(define (find-answer unknown tree level)
  (let* ((dimension (branching-node-dimension tree))
	 (threshold (branching-node-threshold tree))
	 (projection (list-ref (data-point-features unknown) dimension))
	 (next-node #f))
    ;; Decide which branch wins:
    (if (> projection threshold)
	(set! next-node (branching-node-right-branch tree))
	(set! next-node (branching-node-left-branch tree)))
    ;;If the winning branch is an branching node ...
    (if (branching-node? next-node)
	;; Then find the closest neighbor by calling FIND-ANSWER recursively:
	(find-answer unknown next-node (+ 1 level))
	;; Otherwise the winning branch is a terminal node:
	(make-answer next-node))))

;;; ANSWERS

(define (make-answer node)			;Constructor
  (list (terminal-node-identity node)
	(length (terminal-node-samples node))))

(define answer-prediction first)		;Getters
(define answer-support second)

