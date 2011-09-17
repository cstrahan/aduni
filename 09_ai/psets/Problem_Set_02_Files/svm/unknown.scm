;;;; unknown.scm

(declare (usual-integrations))


;; unknown class:
;; '( 'UNKNOWN .
;;    Vector v = '( x1 x2 ...) )

;;        public Unknown (double x1, double x2)
(define (new-unknown x1 x2)
  (make-tagged 'UNKNOWN
        (list x1 x2)))

(define (new-unknown-vector v)
  (make-tagged 'UNKNOWN
        v))

(define (unknown? obj)
  (is-tagged-type? obj 'UNKNOWN))

;;;;;;;;;;

(define (unknown-show-fields)
  '(UNKNOWN vector))

;;;;;;;;;;

;;        public double x1 ()
(define (unknown-x1 unknown)
  (list-ref unknown 1))

;;        public double x2 ()
(define (unknown-x2 unknown)
  (list-ref unknown 2))

;;        public double xn (int n)
(define (unknown-xn unknown n)
  (list-ref unknown n))

;;        public Vector vector ()
(define (unknown-vector unknown)
  (detag unknown))

