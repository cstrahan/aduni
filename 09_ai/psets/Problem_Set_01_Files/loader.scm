;;;; -*- mode:Scheme -*- ;;;;
;;
;; loader.scm
;; 
;; This is the file you actually want to execute.
;; It loads all five code files, then the knowledge file, 
;; then executes a best-first search for the desired solution.
;;

;; Load the files (compiled or otherwise):
(load "search")
(load "rule-utils")
(load "forward")
(load "match")
(load "schedule")

(read-k-file "ta-schedule.k.txt")

(display-assertions)

(best-first *assertions* '((6.001 has a ta)(6.002 has a ta)(6.034 has a ta)))
(get-step-count)
