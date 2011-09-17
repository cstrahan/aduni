; Problem Set 10 Solutions
;   by Michael Allen

; ============
;  Exercise 1
; ============

; Nothing to turn in :)

; ============
;  Exercise 2
; ============

; Eva is correct that the global environment of the ANALYZE evaluator
; will crash the MC-EVAL evaluator (and vice versa) because procedure
; bodies (and any other delayed evaluations) get stored in their
; analyzed form in the analyze evaluator. MC-EVAL cannot handle this
; form and will crash.

; The only things which are safe to preserve when switching evaluators
; are expressions which completely evaluate (ie not procedures).

; ============
;  Exercise 3
; ============

(start-mc-eval)

(adu:define (adu:fib n)
  (adu:cond ((adu:= n 0) 0)
	    ((adu:= n 1) 1)
	    (adu:else (adu:+ (adu:fib (adu:- n 1))
			     (adu:fib (adu:- n 2))))))

Quit

(show-time (lambda () (current-evaluator '(adu:fib 5) the-global-environment)))
;process time: 60 (60 RUN + 0 GC); real time: 60
;Value: 5

(show-time (lambda () (current-evaluator '(adu:fib 10) the-global-environment)))
;process time: 820 (820 RUN + 0 GC); real time: 820
;Value: 55

(show-time (lambda () (current-evaluator '(adu:fib 15) the-global-environment)))
;process time: 9730 (9400 RUN + 330 GC); real time: 9730
;Value: 610

(start-analyze)

(adu:define (adu:fib n)
  (adu:cond ((adu:= n 0) 0)
	    ((adu:= n 1) 1)
	    (adu:else (adu:+ (adu:fib (adu:- n 1))
			     (adu:fib (adu:- n 2))))))

Quit

(show-time (lambda () (current-evaluator '(adu:fib 5) the-global-environment)))
;process time: 60 (60 RUN + 0 GC); real time: 60
;Value: 5

(show-time (lambda () (current-evaluator '(adu:fib 10) the-global-environment)))
;process time: 550 (440 RUN + 110 GC); real time: 550
;Value: 55

(show-time (lambda () (current-evaluator '(adu:fib 15) the-global-environment)))
;process time: 4610 (4450 RUN + 160 GC); real time: 4610
;Value: 610

; ============
;  Exercise 4  (optional)
; ============

; There are many possible answers

; ============
;  Exercise 5
; ============

; PART A

; Garbage collection with Cy's algorithm
; *from*     0   1   2   3   4   5   6   7
; the-cars      p5  n3      n4  n1      n2
; the-cdrs      p2  p4      e0  p7      e0

; *to*      10  11  12  13  14  15  16  17
; the-cars p11  n1  n3  n2  n4
; the-cdrs p12 p13 p14  e0  e0

; Results in this structure (which looks correct)
;   |
;   V
; +---+---+             
; | . | .-----------------+
; +-|-+---+               |
;   V                     V
; +---+---+  +---+---+  +---+---+  +---+---+
; | 1 |  --->| 2 | / |  | 3 |  --->| 4 | / |
; +---+---+  +---+---+  +---+---+  +---+---+

; PART B

; But for this structure,
;   X
;   +-------+
;   V       |
; +---+---+ |           
; | 1 | .---+
; +---+---+

;            x
; *from*     0   1   2   3   4   5   6   7
; the-cars  n1
; the-cdrs  p0

; Cy's garbage collector will loop forever because without
; the broken hearts, it just keeps copying new versions of
; cell 0.

; The memory will wind up looking like

; *to*      10  11  12  13  14  15  16  17
; the-cars  n1  n1  n1  n1  n1  n1  n1  n1 ...
; the-cdrs p11 p12 p13 p14 p15 p16 p17 p18 ...

; ============
;  Exercise 6
; ============

; Diagram does not fit here

; ============
;  Exercise 7
; ============

; A register machine for iterative factorial

; (controller
;   (assign continue (label factorial-done))
;   (assign counter (const 1))
;   (assign product (const 1))
;  ITER-LOOP
;   (test (op >) (reg counter) (reg n))
;   (branch (label done))
;   (assign product (op *) (reg counter) (reg product))
;   (assign counter (op +) (reg counter) (reg 1))
;   (goto (label ITER-LOOP))
;  DONE)

; ============
;  Exercise 8  (optional)
; ============

; The expression is either

((lambda (a b) (/ 2 (+ b a))) 2 1)

;  - or -

(let ((a 2) (b 1))
  (/ 2 (+ b a)))