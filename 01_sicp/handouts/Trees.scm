;;; TREES
;;; Procedures for operating on trees
;;; ADU Lecture 12 October 2000


;;; define a couple of trees for testing

(define tree1 (cons (list 1 2) (list 3 4)))

(define tree2 (list (list 1 2)
		    (list 3
			  (list 4 5 6))
		    (list 7 8)))

;;; procedure to count the leaves of a tree

(define (count-leaves tree)
  (cond ((null? tree) 0)
	((not (pair? tree)) 1)
	(else (+ (count-leaves (car tree))
		 (count-leaves (cdr tree))))))

(count-leaves tree1)
;Value: 4

(count-leaves tree2)
;Value: 8

;;; fringe returns a list of the leaves of the tree

(define (fringe tree)
  (cond ((null? tree) nil)
	((not (pair? tree)) (list tree))
	(else (append (fringe (car tree))
		      (fringe (cdr tree))))))

(fringe tree1)
;Value: (1 2 3 4)

(fringe tree2)
;Value: (1 2 3 4 5 6 7 8)

;;; sum-fringe returns the sum of the leaves of a tree

(define (sum-fringe tree)
  (cond ((null? tree) 0)
	((not (pair? tree)) tree)
	(else (+ (sum-fringe (car tree))
		 (sum-fringe (cdr tree))))))

(sum-fringe tree1)
;Value: 10

(sum-fringe tree2)
;Value: 36

;;; acccumulate-tree captures a common pattern in tree accumulation

(define (accumulate-tree tree term combiner null-value)
  (cond ((null? tree) null-value)
	((not (pair? tree)) (term tree))
	(else (combiner (accumulate-tree (car tree) 
					 term
					 combiner
					 null-value)
			(accumulate-tree (cdr tree)
					 term
					 combiner
					 null-value)))))

;;; define sum-fringe using accumulate-tree

(define (sum-fringe tree)
  (accumulate-tree tree 
		   (lambda (x) x) 
		   + 
		   0))

(sum-fringe tree1)
;Value: 10

(sum-fringe tree2)
;Value: 36

;;; define count-leaves using accumulate

(define (count-leaves tree)
  (accumulate-tree tree 
		   (lambda (x) 1)
		   + 
		   0))

(count-leaves tree1)
;Value: 4

(count-leaves tree2)
;Value: 8

;;; define fringe using accumulate-tree

(define (fringe tree)
  (accumulate-tree tree
		   (lambda (x) (list x))
		   append
		   nil))

(fringe tree1)
;Value: (1 2 3 4)

(fringe tree2)
;Value: (1 2 3 4 5 6 7 8)


;;; map-tree applies a procedure to each leaf of the tree

(define (map-tree op tree)
  (cond ((null? tree) nil)
	((number? tree) (op tree))
	(else (cons (map-tree op (car tree))
		    (map-tree op (cdr tree))))))

(map-tree square tree1)
;Value: ((1 4) 9 16)

(map-tree (lambda (x) (* x 10)) tree2)
;Value: ((10 20) (30 (40 50 60)) (70 80))


;;; write scale-tree using map-tree

(define (scale-tree tree factor)
  (map-tree (lambda (x) (* x factor)) tree))

(scale-tree tree1 10)
;Value: ((10 20) 30 40)

(scale-tree tree2 2)
;Value: ((2 4) (6 (8 10 12)) (14 16))

;;; write increment-tree using map-tree

(define (increment-tree tree)
  (map-tree inc tree))

(increment-tree tree1)
;Value: ((2 3) 4 5)

(increment-tree tree2)
;Value: ((2 3) (4 (5 6 7)) (8 9))

;;; copy-tree makes a new copy of the tree structure (box and pointer
;;; diagram)

(define (copy-tree tree)
  (cond ((null? tree) nil)
	((not (pair? tree)) tree)
	(else (cons (copy-tree (car tree))
		    (copy-tree (cdr tree))))))

(define tree3 (copy-tree tree1))
;Value: "tree3 --> ((1 2) 3 4)"

(eq? tree1 tree3)
;Value: #f

(eqv? tree1 tree3)
;Value: #f

(equal? tree1 tree3)
;Value: #t


;;; deep-reverse returns a list structure whose atoms are the 
;;; atoms of the argument list in reverse order

(define (deep-reverse tree)
  (cond ((null? tree) nil)
	((not (pair? tree)) tree)
	(else (append (deep-reverse (cdr tree))
		      (list (deep-reverse (car tree)))))))

(deep-reverse tree1)
;Value: (4 3 (2 1))

(deep-reverse tree2)
;Value: ((8 7) ((6 5 4) 3) (2 1))


;;; flatten takes a list and returns a list whose elements are 
;;; the elements of each of the lists in the input list 
;;; only flattens the top level

(define (flatten tree)
  (cond ((null? tree) nil)
	((not (pair? tree)) (list tree))
	((not (pair? (car tree))) (append (list (car tree))
					  (flatten (cdr tree))))
	(else (append (car tree)
		      (flatten (cdr tree))))))

(flatten tree1)
;Value: (1 2 3 4)

(flatten tree2)
;Value: (1 2 3 (4 5 6) 7 8)


;;; max-depth returns a number that is equal to the maximum depth 
;;; of the tree

(define (max-depth tree)
  (cond ((null? tree) 0)
	((not (pair? tree)) 1)
	((pair? (car tree)) (max (+ 1 (max-depth (car tree)))
				 (max-depth (cdr tree))))
	(else (max (max-depth (car tree))
		   (max-depth (cdr tree))))))

(max-depth tree1)
;Value: 2

(max-depth tree2)
;Value: 3


;;; Procedure bf will return a list of the leaves of the tree
;;; in breadth-first order.

(define (bf tree)
  (cond ((null? tree) nil)
	((not (pair? tree)) (list tree))
	((pair? (car tree)) (bf (append (cdr tree) (car tree))))
	(else (append (list (car tree))
		      (bf (cdr tree))))))

tree1
;Value: ((1 2) 3 4)

(bf tree1)
;Value: (3 4 1 2)

tree2
;Value: ((1 2) (3 (4 5 6)) (7 8))

(bf tree2)
;Value: (1 2 3 7 8 4 5 6)




