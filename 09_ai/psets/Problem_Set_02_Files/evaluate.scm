;;;; evaluate.scm

;; These 'declare' statements only make compilation more efficient under
;; MIT Scheme. If you are using another scheme, either comment them out or
;; try something like (define (declare . x) #f) .
(declare (usual-integrations))

;;; Evaluate the file of predictions against the correct answers.

(define (read-all file)
  (define (loop)
    (do ((input (read) (read))
	 (lst '() (cons input lst)))
	((eof-object? input) (reverse lst))
      ;;(display input) (newline)
      ))
  (with-input-from-file file loop))

(define (summary doc)
  (list (car doc) (caadr doc)))

(define (read-all-summary file)
  (define (loop)
    (do ((input (read) (read))
	 (lst '() (cons (summary input) lst)))
	((eof-object? input) (reverse lst))
      (display (summary input)) (newline)
      ))
  (with-input-from-file file loop))

;; modified 12/09 by rjr - give percentages as decimals...

(define (evaluate-results output-file nominal-output-file)
  (let* ((actual-results (read-all output-file))
	 (nominal-results (read-all nominal-output-file))
	 (classes (remove-duplicates (map cadr nominal-results)))
	 (class-counts (map (lambda (x) (cons x 0)) classes))
	 (class-correct-counts (map (lambda (x) (cons x 0)) classes)))
	(if (null? actual-results)
	    (error "No data in output-file."))
	(if (null? nominal-results)
	    (error "No data in nominal-output-file."))
	(if (> (length actual-results) (length nominal-results))
	    (display* "\nMore elements in output-file (" (length actual-results) ") than "
	              "nominal-output-file (" (length nominal-results) ")."))
	(if (< (length actual-results) (length nominal-results))
	    (display* "\nMore elements in nominal-output-file (" (length nominal-results) ") than "
	              "output-file (" (length actual-results) ")."))
    (do ((nominals nominal-results (cdr nominals))
	 (actuals actual-results (cdr actuals))
	 (count 1 (+ 1 count))
	 (correct-count 0))
	((or (null? nominals) (null? actuals))
	 (if (not (null? nominals))
	     (display* "\nDid not complete all tests!!! Ran out of output data!!!\n"
	               "Results so far:"))
	 (display* "\nTests = " (- count 1)
		   " Correct = " correct-count 
		   " Pct = " (truncate-float (/ correct-count (- count 1))))
	 (if (not (null? nominals))
	     (display* "\nNOTE: Did not complete all tests!!! Ran out of output data!!!\n"
	               "Performance counting failures:\n"
	               "\nTests = " (length nominal-results)
                   " Correct = " correct-count 
                   " Pct = " (truncate-float (/ correct-count (length nominal-results))) ))
	 )
      (let* ((truth (cadr (first nominals)))
	     (class-count (assoc truth class-counts))
	     (class-correct-count (assoc truth class-correct-counts))
	     (actual (cadr (car actuals))))
	(set-cdr! class-count (+ 1 (cdr class-count)))
	(cond ((equal? truth actual)
	       (set! correct-count (+ 1 correct-count))
	       (set-cdr! class-correct-count (+ 1 (cdr class-correct-count)))
	       ))))
    (for-each
     (lambda (class class-count class-correct-count)
       (display* class ": "
		 (cdr class-correct-count) "/" (cdr class-count) 
		 " = " (truncate-float (/ (cdr class-correct-count) (cdr class-count)))))
     classes
     class-counts
     class-correct-counts))
  )


;; Added 12/06:
;; Our old helper functions.

(define *t:silent* #f)
(define (display* . l)
  ;; Print the list of arguments
  (cond (*t:silent* #f)
	(else
	 (for-each display l)
	 (newline))))

;;;  Purpose:	   Remove duplicates in a list.
;;;  Sample argument: (a b c b d)
;;;  Sample value:    (a b c d)
(define (remove-duplicates l)
  (cond ((null? l) '())
	((member (first l) (cdr l)) (remove-duplicates (cdr l)))
	(else (cons (first l) (remove-duplicates (cdr l))))))


(define (truncate-float f . places)
  (let ((p (if (null? places)
               2
               (car places))))
    (let ((mult (expt 10 p)))
      (/ (round (* (exact->inexact f) mult)) mult))))

