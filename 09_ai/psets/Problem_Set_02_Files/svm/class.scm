;;;; class.scm

(declare (usual-integrations))


(define (make-tagged tag obj)
  (cons tag obj))

(define (get-tag to)
  (car to))

(define (detag to)
  (cdr to))

(define (is-tagged-type? to tag)
  (and (pair? to) (eq? tag (get-tag to))))

;; does *not* mutate, effectively, if 'num' is zero!
(define (list-set! lis num el)
  (if (>= num (length lis))
      (error "list-set! beyond end of list: " num)
      (if (zero? num)
          (cons el (cdr lis))
          (let ((prev (list-tail lis (-1+ num))))
            (set-cdr! prev (cons el (cddr prev)))
            lis))))


(define *class-check-assertions* #t)

;; if 'tag' is a list, all are acceptable
(define (assert-tagged-type obj tag)
  (if (not *class-check-assertions*)
      #t
      (if (not (pair? obj))
          (error "Wrong tagged type found: " tag-list 'non-pair obj)
          (let ((tag-list (if (pair? tag)
                              tag
                              (list tag))))
            (if (for-all? tag-list (lambda (x) (not (is-tagged-type? obj x))))
                (error "Wrong tagged type found: " tag-list (get-tag obj))
                #t)))))

(define (index-list n)
  (define (index-rev i)
    (if (zero? i)
        '()
        (cons (-1+ i) (index-rev (-1+ i)))))
  (if (pair? n)
      (reverse (index-rev (length n)))
      (reverse (index-rev n))))

;;; DISPLAY

(define *t:silent* #f)
(define (display* . l)
  ;; Print the list of arguments
  (cond (*t:silent* #f)
        (else
         (for-each display l)
         (newline))))

