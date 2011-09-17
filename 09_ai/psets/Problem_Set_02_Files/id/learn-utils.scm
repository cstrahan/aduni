
(define *training-data* '()) 	; Assigned to a list of training records
(define *test-data* '())        ; Assigned to a list of test records

;;;; TEST PROCEDURE

;; Try all test unknowns, comparing prediction with truth--- produce
;; success-indicating ratios.
(define (test test-data classify)
  (let* ((classes (remove-duplicates (map data-point-class test-data)))
	 (class-counts (map (lambda (x) (cons x 0)) classes))
	 (class-correct-counts (map (lambda (x) (cons x 0)) classes)))
    (do ((tests test-data (rest tests))
	 (count 1 (+ 1 count))
	 (correct-count 0))
	((null? tests)
	 (display* "Tests = " (- count 1)
		   " Correct = " correct-count 
		   " Pct = " (/ correct-count (- count 1)))
	 )
      (let* ((truth (data-point-class (first tests)))
	     (class-count (assoc truth class-counts))
	     (class-correct-count (assoc truth class-correct-counts))
	     (result (classify (first tests))))
	(set-cdr! class-count (+ 1 (cdr class-count)))
	(cond ((equal? truth result)
	       (set! correct-count (+ 1 correct-count))
	       (set-cdr! class-correct-count (+ 1 (cdr class-correct-count)))
	       ))))
    (for-each
     (lambda (class class-count class-correct-count)
       (display* class ": "
		 (cdr class-correct-count) "/" (cdr class-count) 
		 " = " (/ (cdr class-correct-count) (cdr class-count))))
     classes
     class-counts
     class-correct-counts))
  )

;;; CROSS-VALIDATION

;;; Do an N-way cross-validation, return predicted accuracy of the classify
;;; function, which is the average of the N individual tests.
(define (cross-validate n data train classify)
  (define (loop i cv-data)
    (display* "Fold " i " of " n " way cross-validation.")
    (if (= i n)
	'()
	;; do the ith test and cons to list of results.
	(cons (prediction-accuracy (cv-test-set cv-data i n)
				   (cv-training-set cv-data i n)
				   train
				   classify)
	      (loop (+ i 1) cv-data))))	
  ;; return the averange of all the tests.
  (average (loop 0 (random-reorder data))))

;; (Average can take a list)
;;  (apply average (loop 0 (random-reorder data))))

;;; Estimate prediction accuracy through cross-validation.
(define (prediction-accuracy test-set training-set train classify)

  (display* "Training with " (length training-set) " data points")
  (train training-set)

  (display* "Testing " (length test-set) " data points with classes " 
	    (map data-point-class test-set))
  (let ((result
	 (/ (count #t 
		   (map (lambda (x) (equal? (data-point-class x)
					    (classify x)))
			test-set))
	    (length test-set))))
    (display* "Accuracy is " result)
    result))

;;; Reorder the data set randomly, prior to splitting it.
(define (random-reorder data)
  (do ((i 0 (+ i 1))
       (n (length data))
       (new-data '()))
      ((= i n) new-data)
    (let ((entry (list-ref data (random (- n i)))))
      (set! data (remove-equal entry data))
      (set! new-data (cons entry new-data)))))

;;; Generate the ith test-set (out of a total of n) for cross-validation.
(define (cv-test-set data i n)
  (let* ((lg (length data))		; total number of entries
	 (inc (/ lg n))			; how many entries in set
	 (start (* i (/ lg n)))		; index of first entry
	 )
    (if (< start lg)			; check that it has not overrun
	(list-subset data start (min (+ start inc) lg))
	'())))

;;; Generate the ith training set, which is the complement of the ith test-set
(define (cv-training-set data i n)
  (let* ((lg (length data))
	 (inc (/ lg n))
	 (start (* i inc)))
    (append 
     (list-subset data 0 start)		; from 0 to start
     (list-subset data (min (+ start inc) lg) lg) ; from start+inc to end
     )))

;;; The elements of the list whose indices are >= start and < end.
(define (list-subset l start end)
  (define (loop ll i)
    (cond ((null? ll) '())
	  ((< i start) (loop (cdr ll) (+ i 1)))
	  ((>= i end) '())
	  (else
	   (cons (car ll) (loop (cdr ll) (+ i 1))))))
  (loop l 0))

;;; CLASS COUNTING

(define (class-counts data)
  (let* ((classes (remove-duplicates (map data-point-class data)))
	 (counts (map (lambda (x) (cons x 0)) classes)))
    (do ((dl data (rest dl)))
	((null? dl))
      (let* ((class (data-point-class (first dl)))
	     (class-count (assoc class counts)))
	(set-cdr! class-count (+ 1 (cdr class-count)))))
    (sort counts (lambda (x y) (> (cdr x) (cdr y))))))

(define (most-common-class data)
  (if (null? data)
      #f
      (caar (class-counts data))))

;;; NORMALIZATION

(define *averages* '())
(define *standard-deviations* '())

(define (square x) (* x x))

;;; Use: (set! *training-data* (normalize-training-data *training-data*))
;;; Sets *averages* and *standard-deviations* which can be used to normalize
;;; test-set, e.g. (map normalize-data-point *test-set*)

(define (normalize-training-data data)
  (let ((nf (length (data-point-features (first data))))
	(n (length data)))
    (define (avg i)
      (do ((d data (cdr d))
	   (sum 0))
	  ((null? d) (/ sum n))
	(set! sum (+ (list-ref (data-point-features (car d)) i) sum))))
    (define (sd i avs)
      (do ((d data (cdr d))
	   (av (list-ref avs i))
	   (sum 0))
	  ((null? d) (sqrt (/ sum n)))
	(set! sum (+ (square (- (list-ref (data-point-features (car d)) i) av)) sum))))
    (define (nf-list i fn)
      (if (>= i nf) '()
	  (cons (fn i) (nf-list (+ i 1) fn))))
    (let* ((avg-list (nf-list 0 avg))
	   (sd-list (nf-list 0 (lambda (i) (sd i avg-list)))))
      ;; Remember
      (set! *averages* avg-list)
      (pretty-print avg-list)
      (set! *standard-deviations* sd-list)
      (pretty-print sd-list)
      (map normalize-data-point data))))

(define (normalize-data-point pt)
  (cons (data-point-label pt)
	(map (lambda (f av sd) (if (= sd 0) f (/ (- f av) sd)))
	     (data-point-features pt)
	     *averages*
	     *standard-deviations*)))

;;;; DATA READERS

;;;   Purpose:	Read training data.  Assign *training-data*.
(define (read-training-data file)
  (set! *training-data* (read-data file))
  (length *training-data*))

;;;  Purpose:	Read test data.  Assign *test-data*.
(define (read-test-data file)
  (set! *test-data* (read-data file))
  (length *test-data*))

;;;  Purpose:	Read list of expressions from a file.
;;;  Remark:    Complains if expressions vary in length.
;;;  Returns:	A list of expressions.
(define (read-data file)
  (define (read-expression record-length count)
    (let ((e (read)))
      ;; Keep reading until end of file:
      (cond ((eof-object? e)
	     '())
	    (else
	     ;; Set record length or compare with previous length
	     (if (zero? record-length)
		 (set! record-length (length e))
		 (if (not (= (length e) record-length))
		     (display* "Record " count " is wrong length")
		     ))
	     (cons e (read-expression record-length (+ count 1) ))))))
  (with-input-from-file file 
    (lambda () (read-expression 0 0))))

;;; CONSTRUCTOR FOR DATA-POINT

(define (make-data-point label preferences)
    (cons label preferences))

;;;; ACCESS FUNCTIONS FOR TRAINING AND TEST POINTS

(define (data-point-label dp)		; returns label (class index), e.g. (yes 2)
  (car dp))

(define (data-point-class dp)		; returns class, e.g. yes
  (car (data-point-label dp)))

(define *selected-features* #f)		; when #f, all are selected

(define (data-point-features dp)	; returns list of features
  (if *selected-features*
      (map (lambda (i) (list-ref (cdr dp) i)) *selected-features*)
      (cdr dp)))

;;; DISPLAY

(define *t:silent* #f)
(define (display* . l)
  ;; Print the list of arguments
  (cond (*t:silent* #f)
	(else
	 (for-each display l)
	 (newline))))

;;;; UTILITIES

(define (rest x) (cdr x))

;;; Count how many times elt appears in l, uses equal? to test.
(define (count elt l)
  (define (loop ll ct)
    (cond ((null? ll) ct)
	  ((equal? elt (car ll)) (loop (cdr ll) (+ ct 1)))
	  (else (loop (cdr ll) ct))))
  (loop l 0))

;;;  Purpose:	   Remove duplicates in a list.
;;;  Sample argument: (a b c b d)
;;;  Sample value:    (a b c d)
(define (remove-duplicates l)
  (cond ((null? l) '())
	((member (first l) (rest l)) (remove-duplicates (rest l)))
	(else (cons (first l) (remove-duplicates (rest l))))))

;;; Returns a list that does not contain anything that is equal? to x
(define (remove-equal x l)
  (cond ((null? l) '())
	((equal? x (first l))
	 (remove-equal x (rest l)))
	(else
	 (cons (first l) (remove-equal x (rest l))))))



;;;;;;;;

;;; Average of a list of numbers, allows some of the vals to be #f.
;;; Takes either multiple arguments for the numbers, or a single list.
(define (average . vals)
  (let ((values (if (pair? (car vals))  ;; argument was a list, not multiple args...
		    (car vals)
		    vals)))
    (do ((v values (rest v))
	 (sum 0.0)
	 (num 0))
	((null? v) (if (= num 0) #f (/ sum num)))
      (cond ((first v)
	     (set! sum (+ sum (first v)))
	     (set! num (+ num 1)))))))

