; Problem Set 4 Solutions
;   by Michael Allen

; ============
;  Exercise 1
; ============

; 1. Box and pointer diagram for
(make-frame (make-vect .1 .1)
	    (make-vect .9 .2)
	    (make-vect .2 .9))
;   |
;   V
; ---------  ---------  ---------  ---------
; |   |   +->|   |   +->|   |   +->|   | / |
; --|------  --|------  --|------  --|------
;   V          V          V          V
; 'frame     ---------  ---------  ---------
;            |0.1|0.1|  |0.9|0.2|  |0.2|0.9|
;            ---------  ---------  ---------

; 2. A new implementation for the frame abstraction

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (frame-origin frame)
  (car frame))

(define (frame-edge1 frame)
  (cadr frame))

(define (frame-edge2 frame)
  (caddr frame))

; Nothing else in the code would have to be changed
; because this is a data abstraction.

; 3. It might be useful to include the symbol 'frame
; as part of the representation to make it easier to
; determine if a particular piece of data is meant to
; be a frame or not.

; ============
;  Exercise 2
; ============

(paint g1 (procedure->painter (lambda (x y) (* x y))))
; Solid black field.

(paint g2 (procedure->painter (lambda (x y) (* 255 x y))))
; Shaded hyperbolas with dark in the lower right and white
; in the upper left.

(paint g3 (procedure->painter (lambda (x y) (* 255 x))))
; Shaded from dark on the left to light on the right.

; ============
;  Exercise 3
; ============

; CAPTAIN-ABSTRACTION is a line-drawing of an "A"
(define captain-abstraction
  (let ((v1 (make-vect 0 0))
	(v2 (make-vect .25 .5))
	(v3 (make-vect .5 1))
	(v4 (make-vect .75 .5))
	(v5 (make-vect 1 0)))
    (segments->painter
     (list (make-segment v1 v3)
	   (make-segment v3 v5)
	   (make-segment v2 v4)))))

(paint g1 captain-abstraction)

; ============
;  Exercise 4
; ============

; BELOW places painter1 below painter2
(define (below p1 p2)
  (superpose
   ((transform-painter (make-vect 0 0)
		       (make-vect 1 0)
		       (make-vect 0 .5))
    p1)
   ((transform-painter (make-vect 0 .5)
		       (make-vect 1 .5)
		       (make-vect 0 1))
    p2)))

(paint g2 (below captain-abstraction captain-abstraction))
   
; ============
;  Exercise 5
; ============

; Paint a transformed captain-abstraction
(paint g3 ((transform-painter (make-vect .1 .9)
			      (make-vect 1.5 1)
			      (make-vect .2 0)) captain-abstraction))

; This transformation flips the captain upside down,
; stretches him to the right, and rotates him
; counterclockwise a little bit.

; ============
;  Exercise 6
; ============

; GRAY128 is just a plain gray painter
(define gray128 (number->painter 128))
(paint g1 gray128)

; CIRCLES is a circular shading pattern
(define circles (procedure->painter 
		 (lambda (x y) 
		   (* (+ (square (- x .5))
			 (square (- y .5))) 512))))
(paint g2 circles)

; TRIANGLES draws two triangles on the screen
(define triangles
  (let ((v1 (make-vect .1 .8))
	(v2 (make-vect .1 .1))
	(v3 (make-vect .8 .1))
	(v4 (make-vect .2 .9))
	(v5 (make-vect .9 .9))
	(v6 (make-vect .9 .2)))
    (segments->painter
     (list
      (make-segment v1 v2)
      (make-segment v2 v3)
      (make-segment v3 v1)
      (make-segment v4 v5)
      (make-segment v5 v6)
      (make-segment v6 v4)))))

(paint g3 triangles)


; EINSTEIN is a cool guy
(define einstein (load-painter "einstein"))
; Note: You need the einstein.pgm file for this to work.
; If you don't have it, comment this line out.

(paint g1 einstein)

; ============
;  Exercise 7
; ============

; Some ways to play with the pictures
(paint g2 (beside (below triangles triangles) (below triangles triangles)))
(paint g3 (superpose circles captain-abstraction))

; ============
;  Exercise 8
; ============

; DIAMOND transforms the painter into a diamond pattern
(define (diamond painter)
  ((transform-painter (make-vect 0 .5)
		      (make-vect .5 0)
		      (make-vect .5 1)) painter))
(paint g1 (diamond circles))

; ============
;  Exercise 9
; ============

; NON-SQUARE-DIAMOND creates procedures for making diamonds of various shapes
(define (non-square-diamond y x1 x2)
  (lambda (painter)
    ((transform-painter (make-vect 0 y)
			(make-vect x1 0)
			(make-vect x2 1)) painter)))

(define non-square-diamond1 (non-square-diamond 0.2 0.1 0.9))
(paint g2 (non-square-diamond1 circles))

; Another nifty picture

(define non-square-diamond2 (non-square-diamond 0.8 0.9 0.1))
(paint g3 (superpose (non-square-diamond1 circles) (non-square-diamond2 triangles)))

; =============
;  Exercise 10
; =============

; UP-SPLIT recursively splits the picture upwards
; Analogous to right-split
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
	(below painter (beside smaller smaller)))))

(paint g1 (up-split circles 3))

; =============
;  Exercise 11
; =============

; KEEP-COMBINING is a higher order function for recursively
; applying a combiner to a painter.
(define (keep-combining combine-2)
  (lambda (painter n)
    ((repeated
      (lambda (x) (combine-2 painter x))
      n)
     painter)))

; NEW-RIGHT-SPLIT can be written with KEEP-COMBINING
(define new-right-split
  (keep-combining
   (lambda (p1 p2)
     (beside p1 (below p2 p2)))))

(paint g2 (new-right-split circles 3))

; NEW-UP-SPLIT can also
(define new-up-split
  (keep-combining
   (lambda (p1 p2)
     (below p1 (beside p2 p2)))))

(paint g3 (new-up-split circles 3))

; =============
;  Exercise 12
; =============

; Some pretty recursive pictures made with KEEP-COMBINING

(define crowd
  (keep-combining (lambda (p1 p2)
		    (below (beside p2 p2)
			   (beside p2 p2)))))
(paint g1 (crowd triangles 3))

(define corner
  (keep-combining (lambda (p1 p2)
		    (below p1 (beside p1 p2)))))
(paint g2 (corner circles 3))

; =============
;  Exercise 13
; =============

; There are lots of things you can do for this question.

; THREE lays out the painters in the four quadrants
; with the upper-right quadrant left empty.
(define (three p1 p2 p3)
  (below
   (beside p2 p3)
   ((transform-painter (make-vect 0 0)
		       (make-vect .5 0)
		       (make-vect 0 1)) p1)))

; STAIRS creates a pretty stair pattern using THREE.
(define (stairs p)
  ((keep-combining (lambda (p1 p2) (three p2 p1 p2))) p 3))

(paint g3 (stairs triangles))