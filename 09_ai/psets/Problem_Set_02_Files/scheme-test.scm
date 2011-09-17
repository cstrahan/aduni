;;;; scheme-test.scm

;;; This is a sample of the testing script for Scheme-based solutions to Project 2

;;; Change working directory to the source directory
(cd "/mit/xxx/project2")

;;; Load the  student's loader.scm file
(load "loader.scm")

;;; Load the evaluation code
(load "/mit/6.034/Project2/evaluate.scm")

;;; Run the test, this should create an output file called "project-output"
;;; which has a set of entries of the form (article-id prediction)
;;; See the project specification
(project-test "/mit/6.034/Project2/test.corpus")

;;; Compare the predictions to the actual values
(evaluate-results "project-output" "/mit/6.034/Project2/project-output-nominal")

;;; Done.
;;(exit)

