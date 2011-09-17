;;;; sample.scm

(declare (usual-integrations))


;class Sample extends Unknown
;{
;; sample class:
;; '(        'SAMPLE .
;;          '( unknown u
;;             double alpha = 0.0
;;             int y
;;             double svmValue = 0.0
;;             boolean support = false
;;             wrong = false ) )


;;         public Sample (double x1, double x2, int i)
(define (new-sample x1 x2 i)
  (make-tagged 'SAMPLE
               (list (new-unknown x1 x2)
                     0.0
                     i
                     0.0
                     #f
                     #f)))

;; the standard sample creator assumes a simple system with only two
;; dimensions. It is of course possible to use arbitrary vectors:
(define (new-sample-vector v i)
  (make-tagged 'SAMPLE
               (list (new-unknown-vector v)
                     0.0
                     i
                     0.0
                     #f
                     #f)))

(define (sample? obj)
  (is-tagged-type? obj 'SAMPLE))

;;;;;;;;;;

(define (sample-show-fields)
  '(SAMPLE u alpha y svmValue support wrong))

;;        public double alpha()
(define (sample-alpha sample)
  (list-ref sample 2))

;;        public void alpha(double d)
(define (sample-set-alpha! sample d)
  (list-set! sample 2 d))

;;        public int y ()
(define (sample-y sample)
  (list-ref sample 3))

;;        public void y (int i)
(define (sample-set-y! sample i)
  (list-set! sample 3 i))

;;        public double getValue ()
(define (sample-value sample)
  (list-ref sample 4))

;;        public void setValue (double d)
(define (sample-set-value! sample d)
  (list-set! sample 4 d))

;;        public boolean getSupport ()
(define (sample-support sample)
  (list-ref sample 5))

;;        public void setSupport (boolean b)
(define (sample-set-support! sample b)
  (list-set! sample 5 b))

;;        public boolean getWrong ()
(define (sample-wrong sample)
  (list-ref sample 6))

;;        public void setWrong (boolean b)
(define (sample-set-wrong! sample b)
  (list-set! sample 6 b))

;;;;;;;;;;

;;;; UNKNOWN:

(define (sample-unknown sample)
  (list-ref sample 1))

;;        public double x1 ()
(define (sample-x1 sample)
  (unknown-x1 (sample-unknown sample)))

;;        public double x2 ()
(define (sample-x2 sample)
  (unknown-x2 (sample-unknown sample)))

;;        public double xn (int n)
(define (sample-xn sample n)
  (unknown-xn (sample-unknown sample) n))

;;        public Vector vector ()
(define (sample-vector sample)
  (unknown-vector (sample-unknown sample)))

