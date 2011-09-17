;;;SICP PS-4
;;;Shyam Visweswaran


;;;==============================================================
;;; Exercise 3
;;;==============================================================

;;Captain Abstraction proc
;;the last make-segment creates 2 vectors that are half-way between
;;v1 and v2 and half-way between v2 and v3

(define captain-abstraction
  (let ((v1 (make-vect .1 .1))
	(v2 (make-vect .5 .9))
	(v3 (make-vect .9 .1)))
  (segments->painter
   (list (make-segment v1 v2)
	 (make-segment v2 v3)
	 (make-segment (make-vect (/ (+ (xcor-vect v1) (xcor-vect v2)) 2)
				  (/ (+ (ycor-vect v1) (ycor-vect v2)) 2))
		       (make-vect (/ (+ (xcor-vect v2) (xcor-vect v3)) 2)
				  (/ (+ (ycor-vect v2) (ycor-vect v3)) 2)))))))


;;;==========================================================================
;;; Exercise 4
;;;==========================================================================

;;proc below defined analogously to beside

(define (below painter1 painter2)
  (let ((split-point (make-vect 0 .5)))
    (superpose
     ((transform-painter zero-vector
			 (make-vect 1 0)
			 split-point)
      painter1)
     ((transform-painter split-point
			 (make-vect 1 .5)
			 (make-vect 0 1))
      painter2))))

;;tested with philip and it works

;;;=========================================================
;;; Exercise 5
;;;=========================================================

;; The transformation turns the picture upside-down, rotates
;; it slightly conterclockwise and stretches the picture
;; horizontally to 1.5 times. About one-third of the right
;; part of the picture will fall outside the frame and will not be
;; visible.

(define funny-upside-down
  (transform-painter (make-vect .1 .9)
		     (make-vect 1.5 1)
		     (make-vect .2 0)))


;;;===========================================================
;;; Exercise 6
;;;===========================================================

;;my primitive painters
;paints a uniform gray

(define my-gray (number->painter 120))


;paints noisy mosaic

(define my-noise
  (procedure->painter (lambda (x y) (random 256))))


;paints a square that is divided into 4 squares

(define my-lines
  (let ((v1 (make-vect .1 .1))
	(v2 (make-vect .5 .1))
	(v3 (make-vect .9 .1))
	(v4 (make-vect .1 .5))
	(v5 (make-vect .5 .5))
	(v6 (make-vect .9 .5))
	(v7 (make-vect .1 .9))
	(v8 (make-vect .5 .9))
	(v9 (make-vect .9 .9)))
    (segments->painter
     (list (make-segment v1 v3)
	   (make-segment v4 v6)
	   (make-segment v7 v9)
	   (make-segment v1 v7)
	   (make-segment v2 v8)
	   (make-segment v3 v9)))))

;loads San Franciso bridge and the ADU logo

(define sanfran (load-painter "sanfran"))
(define logo (load-painter "logo"))

;;;===============================================================
;;; Exercise 8
;;;===============================================================

;;diamond procedure

(define diamond
  (transform-painter (make-vect .5 0)
		     (make-vect 1 .5)
		     (make-vect 0 .5)))


;;;===============================================================
;;; Exercise 9
;;;===============================================================

;;non-square diamond proc

(define (make-non-square-diamond y-origin x-edge1 x-edge2)
  (transform-painter (make-vect 0 y-origin)
		     (make-vect x-edge1 0)
		     (make-vect x-edge2 1)))

(define non-square-diamond-1
  (make-non-square-diamond .2 .1 .9))


;;my non-sqaure diamond proc

(define non-square-diamond-2
  (make-non-square-diamond .3 .4 .6))


;;;==============================================================
;;; Exercise 10
;;;==============================================================

;;right-split proc

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
	(beside painter (below smaller smaller)))))

;;up-split proc

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
	(below painter (beside smaller smaller)))))


;;;=========================================================
;;; Exercise 11
;;;=========================================================

;;proc for keep-combining

(define (keep-combining combine-2)
  ;; combine-2 = (lambda (painter1 painter2) ...)
  (lambda (painter n)
    ((repeated
      (lambda (x) (combine-2 painter x))
      n)
     painter)))

;;New proc for right-split

(define new-right-split
  (keep-combining
   (lambda (p1 p2)
     (beside p1 (below p2 p2)))))

;;New proc for up-split

(define new-up-split
  (keep-combining
   (lambda (p1 p2)
     (below p1 (beside p2 p2)))))


;;;=====================================================
;;; Exercise 12
;;;=====================================================

;; Using keep-combining in various procedures

(define nest-diamonds
  (keep-combining
   (lambda (p1 p2) (superpose p1 (diamond p2)))))

(define new-comb
  (keep-combining
   (lambda (p1 p2) (square-limit (below p1 p2) 2))))

(define mix-with-philip
  (keep-combining
   (lambda (p1 p2)
     (below (beside p1 philip)
	    (beside p2 p2)))))

;;my variation

(define (variation-1 painter n)
  (below
  (beside (flip-horiz (nest-diamonds painter n))
	  (nest-diamonds painter n))
  (flip-vert (beside (flip-horiz (nest-diamonds painter n))
	  (nest-diamonds painter n)))))


;;;===================================================
;;; Exercise 13
;;;===================================================

;;this uses a couple of trig functions

(define trig-shading
  (procedure->painter (lambda (x y)
			(* 255
			   (sin (* 30 x))
			   (cos (* 30 y))))))

(paint g3 (square-limit trig-shading 3))

;;yet another trig proc

(define another-trig
  (procedure->painter (lambda (x y)
			(* 100
			   (tan (* 60 (+ x y)))
			   (cos (* 10 y))))))

(paint g2 (nest-diamonds another-trig 4))

;;;=======================================================