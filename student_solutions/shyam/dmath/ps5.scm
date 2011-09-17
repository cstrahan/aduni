;;; permutation


(define (p n r)
  (define (helper product n r)
    (if (< r 1)
	product
	(helper (* n product)
		(dec n)
		(dec r))))
  (helper 1 n r))

(define (c n r)
  (define (helper p-product r-product n r)
    (if (< r 1)
	(/ p-product r-product)
	(helper (* n p-product)
		(* r r-product)
		(dec n)
		(dec r))))
  (helper 1 1 n r))

(define (fact n)
  (define (helper product counter max-count)
    (if (> counter max-count)
	product
	(helper (* counter product)
		(+ counter 1)
		max-count)))
  (helper 1 1 n))







