;;; To test data (using 1-nearest-neighbors and compare as comparison function)
;;; (test-nn *training-data* *test-data* 1 compare)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NEAREST NEIGHBOR CLASSIFICATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Infers the class of a set of preferences, uses voting among the classes
;;; of the K nearest neighbors.
(define (infer-class data unknown k comparison)
  (let ((prediction
	 (most-common-class 
	  (k-nearest-neighbors data (data-point-features unknown)
			       k comparison))))
    (display* "The winner is " prediction
	      ".  Correct is " (data-point-class unknown)
	      "."
	      )
    prediction))

;;; Returns a copy of the database sorted so that records which are closer to 
;;; PREF are earlier in the list.  Of course, this is hopelessly inefficient
;;; for large data bases.  This is here just to illustrate the idea in its
;;; simplest form.

(define (sort-database-by-nearness database pref comparison)
  (define (closer? ex1 ex2) 
    (< (comparison pref (data-point-features ex1))
       (comparison pref (data-point-features ex2))))
  (sort database closer?))

;;; Returns the K nearest members to PREF in DATABASE.
(define (k-nearest-neighbors database pref k comparison)
  (first-n k (sort-database-by-nearness database pref comparison)))

;;; Returns list of averages given a list of preferences, a bit tricky...
;;; If l were of length 2, we want (map average l1 l2), if length = 3, we 
;;; want (map average l1 l2 l3) etc.  Recall (apply f l)=(eval (cons f l)).
(define (map-average l)
  (apply map (cons average l)))

;;; Square 'Euclidean' distance among a set of preferences.  It ignores
;;; any #f fields.
(define (compare pref1 pref2)
  (if (and (not (null? pref1)) (not (null? pref2)) )
      (+ (if (and (first pref1) (first pref2))
	     (square (- (first pref1) (first pref2)))
	     0.0)
	 (compare (rest pref1) (rest pref2)))
      0.0))

;;; Cross-validate nearest-neighbor using n test-sets drawn from
;;; data as the source, use k nearest neighbors and the comparison
;;; function given.

;;; (Changed 12/6 *merely* to make the unrelatedness of the 'data'
;;; argument of 'cross-validate-nn' and the 'data' argument of
;;; the first lambda-generated function (now 'd').)

(define (cross-validate-nn n data k comparison)
  (let ((cv-training-set #f))
    (cross-validate 
     n data
     (lambda (d)			; training function
       (set! cv-training-set d))
     (lambda (x)			; classify
       (infer-class cv-training-set x k comparison)))))

(define (test-nn train-data test-data k comparison)
  (test
   test-data
   (lambda (x)				; classify
     (infer-class train-data x k comparison))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SOME UTILITY FUNCTIONS.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (square x)
    (* x x))

;;; Returns a list which contains the first N elements of LIST.
(define (first-n n list)
    (if (or (= n 0) (null? list))
	'()
	(cons (first list) (first-n (- n 1) (rest list)))))

;;; Average of a list of numbers, allows some of the vals to be #f.
(define (average . vals)
  (do ((v vals (rest v))
       (sum 0.0)
       (num 0))
      ((null? v) (if (= num 0) #f (/ sum num)))
    (cond ((first v)
	   (set! sum (+ sum (first v)))
	   (set! num (+ num 1))))))


;;;; From problem set:

(define (predict-preferences database pref k comparison)
  (map-average
   (map data-point-features
	(k-nearest-neighbors database pref k comparison)))) 

;;; Computes the AVERAGE square difference among common entries.
 (define (compare-missing pref1 pref2)
  (define (loop pr1 pr2 dist n)
    (cond ((and (not (null? pr1)) (not (null? pr2)))
	   (if (and (car pr1) (car pr2)) ; both entries present
	       (loop (cdr pr1) (cdr pr2)
                     ;; increment distance
		     (+ dist (square (- (car pr1) (car pr2))))
                     ;; increment count
		     (+ n 1))
               ;; at least one entry missing, ignore it.
	       (loop (cdr pr1) (cdr pr2) dist n)))
	  (else
	   (if (> n 0) 
	       ;; average square difference
	       (/ dist n)
	       ;; nothing in common, very bad...
	       100.))))
  (loop pref1 pref2 0 0)) 


