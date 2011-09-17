;; ta-schedule.k.txt 
;;
;; This file contains all of the assertions and rules used by the
;; forward chainer.
;;

ASSERTIONS

;; initial step
(Louis has taken 6.001)
(Louis has taken 6.002)
(Louis has taken 6.034)
(Ben has taken 6.001)
(Ben has taken 6.002)
(Alyssa has taken 6.001)
(6.034 needs ta)
(6.001 needs ta)
(6.002 needs ta)
(Louis has not been assigned)
(Ben has not been assigned)
(Alyssa has not been assigned)

RULES

(R1 IF (?ta has taken ?course)
       (?ta has not been assigned)
       (?course needs ta)
    ADD (?course has a ta)
        (?ta is assigned to ?course)
    DELETE (?ta has not been assigned)
           (?course needs ta)
    SAYING ?ta " assigned to " ?course)

