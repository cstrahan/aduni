;;;; nnet-train.scm

(load "learn-utils")
(load "backprop")

;; Samples in the backprop code are all numbers and the class is at the end.
;; This converts from the data sets we saw in PS 7.
(define (convert-sample x)
  (list (data-point-features x)
	(list (if (eq? (data-point-class x) 'high)
		  1.0 0.0))))

;; Read in the data set
(read-training-data "nnet.data")

;; Normalize it
(set! *training-data* 
      (normalize-training-data *training-data*))

;; Convert them and randomize the order.
(define *samples*
  (random-reorder 
   (map convert-sample *training-data*)))

;; Classifies a ps-7 style datapoint using a trained net.  For use with (test...)
(define (neural-classify data-point)
  (let* ((converted (convert-sample data-point))
	 (out (first
	       (compute-final-outputs 
		(forward-propagate-output 
		 (first converted)
		 *weights*))))
	 (prediction (if (>= out 0.5) 'high 'normal)))
    (display* "The prediction is " prediction "(" out ")"
	      ".  Correct is " (data-point-class data-point)
	      "."
	      )
    prediction))

;; Initial weights should be small
(set! *initial-weight-magnitude* 0.001)

;; A reasonable choice of rate for this (small) problem.  
(set! *rate* 1.0)
(define *start-rate* 1.0)

;; Do the training...
;; You might want to do each one a couple of times.
     
(define (train-net n-hidden)
  (display* "\nStarting training with " n-hidden " units.")
  (set! *rate* *start-rate*)
  (set! *weights* (initialize-weights `(2 ,n-hidden 1)))
  ;; train for 6000 epochs or until rms error drops to 0.001
  (train 6000 *samples* *weights* 0.001)
  ;; test the results on the training data, so we can see misclassified points
  (test *training-data* neural-classify))