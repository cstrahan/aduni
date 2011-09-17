;;;; svm-load.scm
;;;;   Loader for the svm system.

;(cf "class")
;(cf "unknown")
;(cf "sample")
;(cf "kernal")
;(cf "errorcache")
;(cf "svmmodel")
;(cf "draw")
;(cf "svm-draw")

(load "class")
(load "unknown")
(load "sample")
(load "kernal")
(load "errorcache")
(load "svmmodel")
(load "draw")
(load "svm-draw")

;;;; PLEASE NOTE:
;;;;   This SVM system has not been completely tested. While it appears to
;;;;   work as desired, it may have bugs or need tweaking.
;;;;   Correspondingly, don't rely on it unless you are completely aware of
;;;;   what you are doing, and even then look out for problems.
;;;; ^^ IMPORTANT WARNING ^^

;; That said...
;; The system is relatively straightforward, although it is lacking comments.
;; class.scm implements a tagged-type system that is used for clarity.
;; The structure reflects the Java origin of the code - most objects are
;; passed in to method-like functions that act on them.
;;
;; To create a new SVM model, try (define sm (new-svmmodel)). This will
;; start it with the same training points as the applet version. You can
;; draw the output of the SVM right away with (draw-svm sm).
;;
;; (svmmodel-learn sm) will run the basic learning routine.
;; Look at the svmmodel and kernal code for details.
;; (svmmodel-samples sm) will return the current training samples for sm.
;; (svmmodel-output sm u) will give the output for unknown u.
;; (svmmodel-advance-kernal-choice sm) will switch between dot and radial.
;; (svmmodel-advance-parameter-choice sm) will shift the kernal parameters.
;; (svmmodel-clear-samples sm) will wipe out the training sample set.
;; (svmmodel-add-sample sm s) will add in sample 's'.
;;
;; The files needed:
;;
;;   class.scm: The simple tagged-type system, used for clarity.
;;   unknown.scm:    A data type representing an input vector.
;;   sample.scm:     An input vector augmented with other information, such as
;;                     the correct output value and whether it is a support.
;;                     Intended for points used in training.
;;   kernal.scm:     A data type that helps implement the dot product and
;;                     radial kernal functions.
;;   errorcache.scm: Used to store the error for each point, for speed. Could
;;                     probably be eliminated and refactored.
;;   svmmodel.scm:   The SVM model. Stores the data and implements learning
;;                     and application.
;;
;; The drawing system needs:
;;
;;   draw.scm:     Generic drawing of functions and other data in MIT Scheme.
;;   svm-draw.scm: Turns SVM models into output matrices.
;;
;; Note that the system may need some work to be extended beyond its
;; demonstration capabilities. Also, the draw routines found in 'draw.scm'
;; and 'svm-draw.scm' are a separable part of the system and will not work
;; correctly with SVMs that have more than 2 dimensions for their samples.

