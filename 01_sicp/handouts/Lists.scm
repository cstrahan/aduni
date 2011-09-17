;;; Code for ADU lecture on aggregate data
;;; Friday, 6 October 2000

;;; LISTS
;;; Procedures for operating on lists

;;; define a couple of lists for testing the procedures below

(define a (list 1 2 3 4 5))
(define b (list 6 7 8))


;;; length returns the length of a list
;;; an empty list has length 0

(define (length lst)
  (if (null? lst)
      0
      (+ 1 (length (cdr lst)))))


;;; nth returns the nth element of a list
;;; car of list is defined as the first element of the list
;;; returns nil if n is greater than the number of elements

(define (nth lst n)
  (cond ((null? lst) nil)
	((= n 0) "Not defined")
	((= n 1) (car lst))
	(else (nth (cdr lst) (- n 1)))))


;;; last returns the last element of a list

(define (last lst)
  (cond ((null? lst) nil)
        ((null? (cdr lst)) (car lst))
	(else (last (cdr lst)))))


;;; scale-list will return a list where each element is n 
;;; times the element in the original list

(define (scale-list lst n)
  (if (null? lst)
      nil
      (cons (* n (car lst))
	    (scale-list (cdr lst) n))))


;;; increment-list takes a list and returns a list consisting 
;;; of the orginial list values incremented by 1

(define (increment-list lst)
  (if (null? lst)
      nil
      (cons (+ 1 (car lst))
	    (increment-list (cdr lst)))))


;;; capture the patterns in scale and increment-list to write map

(define (map lst op)
  (if (null? lst)
      nil
      (cons (op (car lst))
	    (map (cdr lst) op))))


;;; Rewrite scale-list using map

(define (scale-list lst n)
  (map lst (lambda (x) (* n x))))


;;; Rewrite increment-list using map

(define (increment-list lst)
  (map lst (lambda (x) (+ x 1))))


;;; Write a procedure append which takes two lists and returns
;;; a list made up of the first list appended to the first

(define (append lst1 lst2)
  (if (null? lst1)
      lst2
      (cons (car lst1)
	    (append (cdr lst1) lst2))))


;;; Write a procedure that removes any elements of a list equal to n

(define (remove n lst)
  (cond ((null? lst) nil)
	((= n (car lst)) (remove n (cdr lst)))
	(else (cons (car lst)
		    (remove n (cdr lst))))))


;;; TREES
;;; Procedures for operating on trees

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

;;; fringe returns a list of the leaves of the tree

(define (fringe tree)
  (cond ((null? tree) nil)
	((not (pair? tree)) (list tree))
	(else (append (fringe (car tree))
		      (fringe (cdr tree))))))

;;; sum-fringe returns the sum of the leaves of a tree

(define (sum-fringe tree)
  (cond ((null? tree) 0)
	((number? tree) tree)
	(else (+ (sum-fringe (car tree))
		 (sum-fringe (cdr tree))))))

;;; map-tree applies a procedure to each leaf of the tree

(define (map-tree tree op)
  (cond ((null? tree) nil)
	((number? tree) (op tree))
	(else (cons (map-tree (car tree) op)
		    (map-tree (cdr tree) op)))))

;;; write scale-tree using map-tree

(define (scale-tree tree factor)
  (map-tree tree (lambda (x) (* x factor))))


;;; write increment-tree using map-tree

(define (increment-tree tree)
  (map-tree tree inc))

