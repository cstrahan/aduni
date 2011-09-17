;;; SICP PS-10
;;; Shyam Visweswaran

;;;==========================================================================
;;; Exercise 1
;;;==========================================================================

;; nothing to turn in


;;;==========================================================================
;;; Exercise 2
;;;=========================================================================

;; A-Eval expects things already in the environment to be an execuatable
;; lambda procedure that takes as arguement the environment. However,
;; definitions created by MC-Eval are lists and hence these will crash
;; the A-Eval. However, variables defined in MC-Eval can be looked up
;; in the environment by M-Eval without problems.

;; Examples: adu:fact and adu:x are defined in MC-Eval. adu:fact crashes
;; A-Eval as expected and adu:x works fine in A-Eval.

;;; MC-Eval input: (adu:define (adu:fact n)
(adu:if (adu:= n 0)
	1
	(adu:* n (adu:fact (adu:- n 1)))))
;;; MC-Eval input: (adu:fact 10)
;;; MC-Eval value: 3628800
;;; A-Eval input: (adu:fact 10)
;The object ((adu:if (adu:= n 0) 1 (adu:* n (adu:fact (adu:- n 1))))) is not applicable.

;;; MC-Eval input: (adu:define adu:x 3)
;;; MC-Eval input: adu:x
;;; MC-Eval value: 3
;;; A-Eval input: adu:x
;;; A-Eval value: 3


;;;=======================================================
;;; Exercise 3
;;;=======================================================

;; Will test the 2 evaluators with the recursive Fibonacci procedure.
;; 4 runs: fib16, fib18, fib20, fib22

;; MC-Eval
(show-time (lambda() (current-evaluator '(adu:fib 16) the-global-environment)))
process time: 2490 (1910 RUN + 580 GC); real time: 2493
(show-time (lambda() (current-evaluator '(adu:fib 18) the-global-environment)))
process time: 6500 (4980 RUN + 1520 GC); real time: 6522
(show-time (lambda() (current-evaluator '(adu:fib 20) the-global-environment)))
process time: 17110 (13140 RUN + 3970 GC); real time: 17150
(show-time (lambda() (current-evaluator '(adu:fib 22) the-global-environment)))
process time: 44800 (34480 RUN + 10320 GC); real time: 44810

;; A-Eval
(show-time (lambda() (current-evaluator '(adu:fib 16) the-global-environment)))
process time: 1230 (950 RUN + 280 GC); real time: 1224
(show-time (lambda() (current-evaluator '(adu:fib 18) the-global-environment)))
process time: 3240 (2460 RUN + 780 GC); real time: 3243
(show-time (lambda() (current-evaluator '(adu:fib 20) the-global-environment)))
process time: 8420 (6450 RUN + 1970 GC); real time: 8419
(show-time (lambda() (current-evaluator '(adu:fib 22) the-global-environment)))
process time: 21990 (16710 RUN + 5280 GC); real time: 21984

;; Ratio MC-Eval : A-Eval
adu:fib16 -- 2.04
adu:fib18 -- 2.01
adu:fib20 -- 2.04
adu:fib22 -- 2.04

;; Now lets see what happens with the recursive factorial procedure
;; MC-Eval
(show-time (lambda() (current-evaluator '(adu:fact 2000) the-global-environment)))
process time: 1160 (1000 RUN + 160 GC); real time: 1164
(show-time (lambda() (current-evaluator '(adu:fact 7000) the-global-environment)))
process time: 5110 (3880 RUN + 1230 GC); real time: 5106

;; A-Eval
(show-time (lambda() (current-evaluator '(adu:fact 2000) the-global-environment)))
process time: 740 (580 RUN + 160 GC); real time: 737
(show-time (lambda() (current-evaluator '(adu:fact 7000) the-global-environment)))
process time: 3380 (2430 RUN + 950 GC); real time: 3373

;; Ratio MC-Eval : A-Eval
adu:fact 2000 -- 1.57
adu:fact 7000 -- 1.51


;;;==============================================================================
;;; Exercise 4
;;;==============================================================================

;; Here i test the iterative factorial procedure for really big numbers and 
;; the A-Eval runs less than 1.5 times the speed of the MC-Eval. The computation
;; of the numbers takes considerable time and overcomes any advantage that the
;; A-Eval gets by not re-parsing.

;; MC-Eval
;(show-time (lambda() (current-evaluator '(adu:i-fact 20000) the-global-environment)))
;process time: 23450 (17270 RUN + 6180 GC); real time: 23499
;(show-time (lambda() (current-evaluator '(adu:i-fact 50000) the-global-environment)))
;process time: 152940 (89230 RUN + 63710 GC); real time: 152939

;; A-Eval
;(show-time (lambda() (current-evaluator '(adu:i-fact 20000) the-global-environment)))
;process time: 18930 (12770 RUN + 6160 GC); real time: 18939
;(show-time (lambda() (current-evaluator '(adu:i-fact 50000) the-global-environment)))
;process time: 121320 (77190 RUN + 44130 GC); real time: 121334

;; Ratio MC-Eval : A-Eval
adu:-i-fact 20000 -- 1.24
adu:-i-fact 50000 -- 1.26

;;; Rest of the exercises on paper






