(define diamond
  (transform-painter (make-vect 0 .5)
		     (make-vect .5 0)
		     (make-vect .5 1)))

;;; For make-non-square-diamond:
;;; n1 is y coord of origin (x set to 0)
;;; n2 is x coord of edge1 (y set to 0)
;;; n3 is x coord of edge2 (y set to 1)

(define (make-non-square-diamond n1 n2 n3)
  (transform-painter (make-vect 0 n1)
		     (make-vect n2 0)
		     (make-vect n3 1)))
  
(define non-square-diamond1 (make-non-square-diamond .5 .2 .8))

(define non-square-diamond2 (make-non-square-diamond .2 .1 .9))

;(paint g1 (non-square-diamond2 philip))

;(paint g1 (up-split (diamond philip) 3))

;(paint g3 (up-split (superpose black (diamond philip)) 3))

;(paint g1 (rotate90 philip))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
	(below painter (beside smaller smaller)))))

;(paint g2 (up-split philip 3))

;(paint g3 mark-of-zorro) 

;(frame-coord-map philip)

;(paint g1 (procedure->painter (lambda (x y) (* 255 x y))))

;(paint g1 (procedure->painter (lambda (x y) (* 255 x))))

;(paint g1 ((transform-painter (make-vect .1 .9) (make-vect 1.4 1) (make-vect .2 0)) philip))

(define (keep-combining combine-2)
  (lambda (painter n)
    ((repeated (lambda (x) (combine-2 x x)) n) painter)))



