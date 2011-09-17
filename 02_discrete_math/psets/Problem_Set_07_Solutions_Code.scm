;;; Discrete Math PS-7
;;; Shyam Visweswaran

;; Exercise 2

(define (extended-gcd a b)
  (cond	((= b 0) (list a 1 0))
	(else
	 (let ((lst (extended-gcd b (remainder a b))))
	   (list (car lst)
		 (caddr lst)
		 (- (cadr lst) (* (quotient a b) (caddr lst))))))))


(extended-gcd 233987973 41111687)
;Value: (1 -11827825 67318298)
;; the return value is of the form (gcd x y)

;; Exercise 9 - RSA encryption
;; the public key is (e, n) and the private key is (d, n)

(define (encode e n lst)
  (cond ((null? lst)
	 nil)
	(else
	 (cons (modulo (expt (car lst) e) n)
	       (encode e n (cdr lst))))))

(define (decode d n lst)
  (cond ((null? lst)
	 nil)
	(else
	 (cons (modulo (expt (car lst) d) n)
	       (decode d n (cdr lst))))))


;; "TOO MUCH WORK!" translates to ASCII as
;; (84 79 79 32 77 85 67 72 32 87 79 82 75 33)
;; Here n = 143, e = 37 d = 13

(encode 37 143 (list 84 79 79 32 77 85 67 72 32 87 79 82 75 33))
;Value: (6 40 40 32 77 46 67 85 32 87 40 69 114 33)

(decode 13 143 (list 6 40 40 32 77 46 67 85 32 87 40 69 114 33))
;Value: (84 79 79 32 77 85 67 72 32 87 79 82 75 33)


;; Exercise 10 - cracking UFO message
;; 1147 factors into 31.37
;; solving 7.d.mod1080 = 1 gives d = 463
;; hence the private key is (463, 1147)

(decode 463 1147 (list 128 1040 129 1144 788 735 570 875))
