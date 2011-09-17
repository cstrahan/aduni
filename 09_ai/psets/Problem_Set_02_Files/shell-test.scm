;;;; shell-test.scm

;;; This is a sample of the testing script for Shell-based solutions to Project 2

;;; Change working directory to the source directory
(cd "/mit/xxx/project2")

;;; Load the evaluation code
(load "/mit/6.034/Project2/evaluate.scm")

;;; Run the test, this should create an output file called "project-output"
;;; which has a set of entries of the form (article-id prediction)
;;; See the project specification.
;;; If you have problems with this, we can run the test before loading Scheme.
;;; Indicate in your on-line submission that we should do that.

(define (is-SunOS?)
  (string-ci=? microcode-id/operating-system-variant "SunOS"))

(define (is-unix?)
  (string-ci=? microcode-id/operating-system-name "unix"))

(define (system-exec cmd)
  (if (is-unix?)
      (system cmd)
      (run-shell-command cmd)))
      
(system-exec "project-test /mit/6.034/Project2/test.corpus")

;;;; NOTE: Previously, the undocumented function 'system' was used instead.
;;;; The two main differences are that the output is given in the console, not
;;;; the Edwin window, and that 'system' is not implemented in the Win32 version
;;;; of MIT Scheme. If you have problems, let us know; you can revert back to the
;;;; old:
;; (system "project-test /mit/6.034/project2/test.corpus")

;;;; Here is a definition of 'system' for Win32 if you'd like, although it is not
;;;; much use. It is inexplicable that it is not defined as such already.
;;(define (system cmd-line)
;;  (run-shell-command cmd-line 'input #f 'output #f))

;;;; ALSO NOTE: If your program is a mix of scheme and another executable, you are
;;;; probably best off coding the system calls into your scheme code yourself and
;;;; having us invoke the scheme-test code.


;;; Compare the predictions to the actual values
(evaluate-results "project-output" "/mit/6.034/Project2/project-output-nominal")

;;; Done.
;;(exit)

