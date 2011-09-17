
;; a-list (frequency, aj, ai)
(define a-list
  (list '(1 1 1)
	'(3 2 1)
	'(3 3 1)
	'(3 4 1)
	'(3 5 1)
	'(3 6 1)
	'(4 2 2)
	'(9 3 2)
	'(9 4 2)
	'(9 5 2)
	'(9 6 2)
	'(7 3 3)
	'(15 4 3)
	'(15 5 3)
	'(15 6 3)
	'(10 4 4)
	'(21 5 4)
	'(21 6 4)
	'(13 5 5)
	'(27 6 5)
	'(16 6 6)))

;; d2-list (frequency dj di)
(define d2-list
  (list '(1 1 1)
	'(2 2 1)
	'(2 3 1)
	'(2 4 1)
	'(2 5 1)
	'(2 6 1)
	'(1 2 2)
	'(2 3 2)
	'(2 4 2)
	'(2 5 2)
	'(2 6 2)
	'(1 3 3)
	'(2 4 3)
	'(2 5 3)
	'(2 6 3)
	'(1 4 4)
	'(2 5 4)
	'(2 6 4)
	'(1 5 5)
	'(2 6 5)
	'(1 6 6)))

;; d1-list

(define d1-list '(1 2 3 4 5 6))

;; game1

(define (game1)
  (let ((org-d1-list d1-list))
    (define (helper total a-score d-score a-list d1-list)
      (cond ((null? d1-list)
	     (helper total a-score d-score (cdr a-list) org-d1-list))
	    ((null? a-list)
	     (newline)
	     (display (list 'd-score d-score (/ d-score total)))
	     (newline)
	     (display (list 'a-score a-score (/ a-score total)))
	     (newline)
	     (display (list 'total-score (+ d-score a-score) 'total: total)))
	    (else (let ((dj (car d1-list))
			(aj (cadar a-list))
			(a-freq (caar a-list)))
		    (cond ((>= dj aj)
			   (helper (+ total a-freq)
				   a-score
				   (+ d-score a-freq)
				   a-list
				   (cdr d1-list)))
			  (else (helper (+ total a-freq)
					(+ a-score a-freq)
					d-score
					a-list
					(cdr d1-list))))))))
    (helper 0 0 0 a-list d1-list)))
	     

;; game2

(define (game2)
  (let ((org-d2-list d2-list))
    (define (helper total draws a-score d-score a-list d2-list)
      (cond ((null? d2-list)
	     (helper total draws a-score d-score (cdr a-list) org-d2-list))
	    ((null? a-list)
	     (newline)
	     (display (list 'd-score: d-score (/ d-score total)))
	     (newline)
	     (display (list 'a-score: a-score (/ a-score total)))
	     (newline)
	     (display (list 'draws: draws (/ draws total)))
	     (newline)
	     (display (list 'total-score: (+ d-score a-score draws) 'total: total)))
	    (else (let ((dj (cadar d2-list))
			(di (caddar d2-list))
			(d-freq (caar d2-list))
			(aj (cadar a-list))
			(ai (caddar a-list))
			(a-freq (caar a-list)))
		    (cond ((and (>= dj aj) (>= di ai))
			   (helper (+ total (* a-freq d-freq))
				   draws
				   a-score
				   (+ d-score (* a-freq d-freq))
				   a-list
				   (cdr d2-list)))
			  ((and (> aj dj) (> ai di))
			   (helper (+ total (* a-freq d-freq))
				   draws
				   (+ a-score (* a-freq d-freq))
				   d-score
				   a-list
				   (cdr d2-list)))
			  (else (helper (+ total (* a-freq d-freq))
					(+ draws (* a-freq d-freq))
					a-score
					d-score
					a-list
					(cdr d2-list))))))))
    (helper 0 0 0 0 a-list d2-list)))
















