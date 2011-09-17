;;; Code for evaluating orders of growth
;;; ADU SICP Lecture, 3 October 2000

;;; Multiplication using addition


(define (mult1 x y)
  (if (= y 0)
      0 
      (+ x (mult1 x (- y 1)))))


(define (mult2 x y)
  (define (mult-iter y ans)
    (if (= y 0)
	ans
	(mult-iter (- y 1) (+ x ans))))
  (mult-iter y 0))


;;; Defined for use in fast-mult procedures below

(define (halve n) 
  (/ n 2))

(define (double n) 
  (* 2 n))


;;; Fast-mult procedures: looking at multiplication another way


(define (fast-mult1 a b)
  (cond ((= b 0) 0)
	((even? b) (fast-mult1 (double a) (halve b)))
	(else (+ a (fast-mult1 a (- b 1))))))


(define (fast-mult2 a b)
  (define (iter a b result)
    (cond ((= b 0) result)
	  ((even? b) (iter (double a) (halve b) result))
	  (else (iter a (- b 1) (+ a result)))))
  (iter a b 0))


;;; Towers of Hanoi

(define (move-tower size from to extra)
  (cond ((= size 0) nil)
	(else (move-tower (- size 1) from extra to)
	      (print-move from to)
	      (move-tower (- size 1) extra to from))))


(define (print-move from to)
  (newline)
  (display "Move top disk from ")
  (display from)
  (display " to ")
  (display to))