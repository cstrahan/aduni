;; Pset 1
;;
;;

(define p1
  (lambda (x y)
    (+ (p2 x y) (p3 x y))))

(define p2
  (lambda (z w)
    (* z w)))

(define p3
  (lambda (a b)
    (+ (p2 a) (p2 b))))

(define fold
  (lambda (x y)
    (* (spindle x)
       (+ (mutilate y)
	  (spindle x y)))))

(define spindle
  (lambda (w) (* w w)))

(define mutilate
  (lambda (z)
    (+ (spindle z) z)))

(define fact
  (lambda (n)
    (if (= n 0)		
	m
	(* n (fact (- n 1))))))
