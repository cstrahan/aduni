;;;; -*- mode:Scheme -*- ;;;;
;;
;; match.scm
;;
;; This file contains a matcher, as seen in PS1
;;
;; Derives from an implementation by Patrick Winston.
;; Modified by Tomas Lozano-Perez.

;; Ignore this; it aids compilation but affects nothing else.
(declare (usual-integrations))

(define (match p d) (do-match p d (empty-bindings)))

(define (do-match p d bindings)
  ;;Arguments:        Pattern, datum, optional bindings.
  ;;Returns:        A list of bindings or #f
  ;;Remarks:        Pattern variables are indicated by (? <variable name>)
  ;;                Prolog's nameless variable is indicated by (? _)
  (cond ((and (null? p) (null? d))
         bindings)
        ((and (not (pair? p)) (not (pair? d)))
         ;; atoms are the opposites of pairs.
         (match-atoms p d bindings))
        ((simple-variable? p)
         (match-simple-variable p d bindings))
        ((pair? p)
         ;; We don't check d here because ((* x)) matches ()
         (match-pieces p d bindings))
        (else #f)))

(define (match-atoms p d bindings)
  ;;Are the pattern and datum the same:
  (if (equal? p d)                        ; handle strings, etc so not eq?
      ;;If so, return the value of BINDINGS.
      bindings
      ;;Otherwise, return #f
      #f))

(define (match-simple-variable variable-expression d bindings)
  (let ((binding (find-binding variable-expression bindings)))
    ;;Is the pattern variable on the list of bindings:
    (if binding
        ;;If it is, substitute its value and try again:
        (do-match (binding-value binding) d bindings)
        ;;Otherwise, add new binding:
        (add-binding variable-expression d bindings))))

(define (match-pieces p d bindings)
  (if (pair? d)
      (let ((result (do-match (first p) (first d) bindings)))
        ;;See if the FIRST parts match producing new bindings:
        (if result
            ;;If they do match, try the REST parts using the resulting bindings:
            (do-match (rest p) (rest d) result)
            ;;If they do not match, fail.
            #f))
      #f))

;;; UTILITIES

;;; Add elt to the end of lst.
(define (add-to-end elt lst)
  (append lst (list elt)))
