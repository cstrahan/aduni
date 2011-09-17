;;;; errorcache.scm

;class ErrorCache
;{
;; errorcache class:
;;   '( 'ERRORCACHE .
;;      '( int length
;;         Vector errors ) )

;;        public ErrorCache (int l)
(define (new-errorcache l)
  (let ((ec (make-tagged 'ERRORCACHE
                         (list l #f))))
    (errorcache-clear-cache ec)
    ec))

(define (errorcache? obj)
  (is-tagged-type obj 'ERRORCACHE))

;;;;;;;;;;

(define (errorcache-show-fields)
  '(ERRORCACHE length errors))

(define (errorcache-length ec)
  (list-ref ec 1))

(define (errorcache-set-length! ec len)
  (list-set! ec 1 len))

(define (errorcache-errors ec)
  (list-ref ec 2))

(define (errorcache-set-errors! ec errors)
  (list-set! ec 2 errors))

;;;;;;;;;;

;;        public void clearCache ()
(define (errorcache-clear-cache ec)
  (errorcache-set-errors! ec (make-list (errorcache-length ec))))

;;        public void setError(int index, double d)
(define (errorcache-set-error ec index d)
  (let ((errors (errorcache-errors ec)))
    (errorcache-set-errors! ec (list-set! errors index d))))

;;        public Double getError(int index)
(define (errorcache-get-error ec index)
  (let ((o (list-ref (errorcache-errors ec) index)))
    (if o  ;; no use, really
        o
        #f)))

