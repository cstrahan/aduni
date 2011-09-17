;;; Discrete math PS-3

;;;===========================================================
;;; Exercise 2
;;;===========================================================

;;; 4-peg Tower of Hanoi - toh4-sloppy
;;; if n is odd, the top-half gets an extra disk

(define (toh4-sloppy n from to using1 using2)
  (let ((top-half (ceiling (/ n 2)))
	(bottom-half (floor (/ n 2))))
    (cond ((= n 1)
	 (display (list 'toh4-sloppy: 'move 'disk from '--> to))
	 (newline))
	(else
	 (toh4-sloppy top-half from using1 to using2)
	 (toh4-sloppy bottom-half from to using2 using1)
	 (toh4-sloppy top-half using1 to from using2)))))

;; works for n = 3
;(toh4-sloppy 3 'from 'to 'using1 'using2)
;(toh4-sloppy: move disk from --> to)
;(toh4-sloppy: move disk from --> using1)
;(toh4-sloppy: move disk to --> using1)
;(toh4-sloppy: move disk from --> to)
;(toh4-sloppy: move disk using1 --> from)
;(toh4-sloppy: move disk using1 --> to)
;(toh4-sloppy: move disk from --> to)

;; works for n = 4
;(toh4-sloppy 4 'from 'to 'using1 'using2)
;(toh4-sloppy: move disk from --> to)
;(toh4-sloppy: move disk from --> using1)
;(toh4-sloppy: move disk to --> using1)
;(toh4-sloppy: move disk from --> using2)
;(toh4-sloppy: move disk from --> to)
;(toh4-sloppy: move disk using2 --> to)
;(toh4-sloppy: move disk using1 --> from)
;(toh4-sloppy: move disk using1 --> to)
;(toh4-sloppy: move disk from --> to)

;; does not work for n = 8
;(toh4-sloppy 8 'from 'to 'using1 'using2)
;(toh4-sloppy: move disk from --> using1)
;(toh4-sloppy: move disk from --> to)
;(toh4-sloppy: move disk using1 --> to)
;(toh4-sloppy: move disk from --> using2)
;(toh4-sloppy: move disk from --> using1)
;(toh4-sloppy: move disk using2 --> using1)
;(toh4-sloppy: move disk to --> from)
;(toh4-sloppy: move disk to --> using1)
;(toh4-sloppy: move disk from --> using1)
;(toh4-sloppy: move disk from --> to)
;(toh4-sloppy: move disk from --> using2)
;(toh4-sloppy: move disk to --> using2)
;(toh4-sloppy: move disk from --> using1)
;(toh4-sloppy: move disk from --> to)
;(toh4-sloppy: move disk using1 --> to)
;(toh4-sloppy: move disk using2 --> from)
;(toh4-sloppy: move disk using2 --> to)
;(toh4-sloppy: move disk from --> to)
;(toh4-sloppy: move disk using1 --> to)
;(toh4-sloppy: move disk using1 --> from)
;(toh4-sloppy: move disk to --> from)
;(toh4-sloppy: move disk using1 --> using2)
;(toh4-sloppy: move disk using1 --> to)
;(toh4-sloppy: move disk using2 --> to)
;(toh4-sloppy: move disk from --> using1)
;(toh4-sloppy: move disk from --> to)
;(toh4-sloppy: move disk using1 --> to)

;;; 4-peg Tower of Hanoi - toh4-fruity
;;; if n is odd, the top-half gets the extra disk

(define (toh4-fruity n from to using1 using2)
  (let ((top-half (ceiling (/ n 2)))
	(bottom-half (floor (/ n 2))))
    (cond ((= n 1)
	 (display (list 'toh4-fruity: 'move 'disk from '--> to))
	 (newline))
	(else
	 (toh4-fruity top-half from using1 to using2)
	 (toh4-fruity bottom-half from to using1 using2)
	 (toh4-fruity top-half using1 to from using2)))))

;; works for n = 3
;(toh4-fruity 3 'from 'to 'using1 'using2)
;(toh4-fruity: move disk from --> to)
;(toh4-fruity: move disk from --> using1)
;(toh4-fruity: move disk to --> using1)
;(toh4-fruity: move disk from --> to)
;(toh4-fruity: move disk using1 --> from)
;(toh4-fruity: move disk using1 --> to)
;(toh4-fruity: move disk from --> to)

;; does not work for n = 4
;(toh4-fruity 4 'from 'to 'using1 'using2)
;(toh4-fruity: move disk from --> to)
;(toh4-fruity: move disk from --> using1)
;(toh4-fruity: move disk to --> using1)
;(toh4-fruity: move disk from --> using1)
;(toh4-fruity: move disk from --> to)
;(toh4-fruity: move disk using1 --> to)
;(toh4-fruity: move disk using1 --> from)
;(toh4-fruity: move disk using1 --> to)
;(toh4-fruity: move disk from --> to)

;; does not work for n = 8
;(toh4-fruity 8 'from 'to 'using1 'using2)
;(toh4-fruity: move disk from --> using1)
;(toh4-fruity: move disk from --> to)
;(toh4-fruity: move disk using1 --> to)
;(toh4-fruity: move disk from --> to)
;(toh4-fruity: move disk from --> using1)
;(toh4-fruity: move disk to --> using1)
;(toh4-fruity: move disk to --> from)
;(toh4-fruity: move disk to --> using1)
;(toh4-fruity: move disk from --> using1)
;(toh4-fruity: move disk from --> to)
;(toh4-fruity: move disk from --> using1)
;(toh4-fruity: move disk to --> using1)
;(toh4-fruity: move disk from --> using1)
;(toh4-fruity: move disk from --> to)
;(toh4-fruity: move disk using1 --> to)
;(toh4-fruity: move disk using1 --> from)
;(toh4-fruity: move disk using1 --> to)
;(toh4-fruity: move disk from --> to)
;(toh4-fruity: move disk using1 --> to)
;(toh4-fruity: move disk using1 --> from)
;(toh4-fruity: move disk to --> from)
;(toh4-fruity: move disk using1 --> from)
;(toh4-fruity: move disk using1 --> to)
;(toh4-fruity: move disk from --> to)
;(toh4-fruity: move disk from --> using1)
;(toh4-fruity: move disk from --> to)
;(toh4-fruity: move disk using1 --> to)

;;;========================================================================
;;; Exercise 3
;;;========================================================================

;;; 4-peg Tower of Hanoi - toh4 - (that works!)
;;; Divide the stack of disks into 2 equal halves if even; if odd
;;; the top-half gets an extra disk. The first and third recursive
;;; calls call toh4 and the second recursive call calls toh3

(define (toh4 n from to using1 using2)
  (let ((bottom-half (ceiling (/ n 2)))
	(top-half (floor (/ n 2))))
    (cond ((= n 1)
	 (display (list 'toh4: 'move 'disk from '--> to))
	 (newline))
	(else
	 (toh4 top-half from using1 to using2)
	 (toh3 bottom-half from to using2)
	 (toh4 top-half using1 to from using2)))))

(define (toh3 n from to using)
  (cond ((> n 0)
	 (toh3 (- n 1) from using to)
	 (display (list 'toh3: 'move 'disk from '--> to))
	 (newline)
	 (toh3 (- n 1) using to from))))

  
;; test for n = 3, 4, 8 - works for all

(toh4 3 'from 'to 'using1 'using2)
(toh4: move disk from --> using1)
(toh3: move disk from --> using2)
(toh3: move disk from --> to)
(toh3: move disk using2 --> to)
(toh4: move disk using1 --> to)
;Value: #[unspecified-return-value]

(toh4 4 'from 'to 'using1 'using2)
(toh4: move disk from --> to)
(toh3: move disk from --> using1)
(toh4: move disk to --> using1)
(toh3: move disk from --> using2)
(toh3: move disk from --> to)
(toh3: move disk using2 --> to)
(toh4: move disk using1 --> from)
(toh3: move disk using1 --> to)
(toh4: move disk from --> to)
;Value: #[unspecified-return-value]

(toh4 8 'from 'to 'using1 'using2)
(toh4: move disk from --> using1)
(toh3: move disk from --> to)
(toh4: move disk using1 --> to)
(toh3: move disk from --> using2)
(toh3: move disk from --> using1)
(toh3: move disk using2 --> using1)
(toh4: move disk to --> from)
(toh3: move disk to --> using1)
(toh4: move disk from --> using1)
(toh3: move disk from --> using2)
(toh3: move disk from --> to)
(toh3: move disk using2 --> to)
(toh3: move disk from --> using2)
(toh3: move disk to --> from)
(toh3: move disk to --> using2)
(toh3: move disk from --> using2)
(toh3: move disk from --> to)
(toh3: move disk using2 --> to)
(toh3: move disk using2 --> from)
(toh3: move disk to --> from)
(toh3: move disk using2 --> to)
(toh3: move disk from --> using2)
(toh3: move disk from --> to)
(toh3: move disk using2 --> to)
(toh4: move disk using1 --> to)
(toh3: move disk using1 --> from)
(toh4: move disk to --> from)
(toh3: move disk using1 --> using2)
(toh3: move disk using1 --> to)
(toh3: move disk using2 --> to)
(toh4: move disk from --> using1)
(toh3: move disk from --> to)
(toh4: move disk using1 --> to)

;;;===================================================
;;; Exercise 11
;;;===================================================

;; binary->gray proc takes a binary number n and returns
;; the nth element of the Gray code. It reads form left
;; to right and compares the current digit to the previous
;; digit - if they are the same it outputs 1 and if
;; different it outputs 0

(define (binary->gray binary-list)
  (define (helper prev lst)
    (cond ((null? lst)
	   '())
	  ((= prev (car lst))
	   (cons 0
		 (helper (car lst) (cdr lst))))
	  (else
	   (cons 1
		 (helper (car lst) (cdr lst))))))
  (helper 0 binary-list))
	  
;; gray->binary proc takes the nth Gray code element and
;; returns the binary number n that corresponds to the
;; position of the Gray code element. It read from left to
;; right and does not change any elements till it till after
;; the first 1 is seen. It then complements the next digit
;; and after that complements the current digit if there are an odd
;; number of preceeding 1s.

(define (gray->binary gray-list)
  (define (helper ones lst)
    (cond ((null? lst)
	   '())
	  ((= (remainder ones 2) 1)
	   (cons (remainder (+ (car lst) 1) 2)
		 (helper (+ ones (car lst)) (cdr lst))))
	  (else
	   (cons (car lst)
		 (helper (+ ones (car lst)) (cdr lst))))))
  (helper 0 gray-list))
    
;; now lets test - it works

(binary->gray (list 0 0 0 0 0 0 0))
;Value: (0 0 0 0 0 0 0)
(gray->binary (list 0 0 0 0 0 0 0))
;Value: (0 0 0 0 0 0 0)

(gray->binary (list 1 1 1 1 1 1 1))
;Value: (1 0 1 0 1 0 1)
(binary->gray (list 1 0 1 0 1 0 1))
;Value: (1 1 1 1 1 1 1)

(binary->gray (list 1 0 0 1 0 1 1 1 0 1 0 1 0 0 0 1))
;Value: (1 1 0 1 1 1 0 0 1 1 1 1 1 0 0 1)
(gray->binary (list 1 1 0 1 1 1 0 0 1 1 1 1 1 0 0 1))
;Value: (1 0 0 1 0 1 1 1 0 1 0 1 0 0 0 1)
		 
;;;====================================================








