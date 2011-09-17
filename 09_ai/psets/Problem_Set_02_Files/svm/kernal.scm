;;;; kernal.scm

(declare (usual-integrations))


;class Kernal
;{
;; kernal class:
;; '( 'KERNAL .
;;    '(        Vector samples
;;              Vector matrix ) )

(define *KERNAL-DOT* 0)
(define *KERNAL-RADIAL* 1)
(define *KERNAL-EXPONENT-LIMIT* 5)
(define *KERNAL-SIGMA-LIMIT* 10)
(define *kernal-mode* *KERNAL-DOT*)
(define *kernal-exponent* 1.)
(define *kernal-exponent-root* 1)
(define *kernal-sigma* 1.)
(define *kernal-sigma-root* 1)

;;        public Kernal (Vector s)
(define (new-kernal s)
  (make-tagged 'KERNAL
               (list s #f)))

(define (kernal? obj)
  (is-tagged-type? obj 'KERNAL))

;;;;;;;;;;

(define (kernal-show-fields)
  '(KERNAL samples matrix))

(define (kernal-samples kernal)
  (list-ref kernal 1))

(define (kernal-set-samples! kernal s)
  (list-set! kernal 1 s))

(define (kernal-matrix kernal)
  (list-ref kernal 2))

(define (kernal-set-matrix! kernal m)
  (list-set! kernal 2 m))

(define (kernal-mode . kernal)
  *kernal-mode*)

(define (kernal-set-mode! mode . kmode)
  (let ((new-mode (if (null? kmode)
                      mode
                      (car kmode))))
    (set! *kernal-mode* new-mode)))

(define (kernal-sigma-root . kernal)
  *kernal-sigma-root*)

(define (kernal-set-sigma-root! sr . ksr)
  (let ((new-sr (if (null? ksr)
                    sr
                    (car ksr))))
    (set! *kernal-sigma-root* new-sr)))

(define (kernal-exponent-root . kernal)
  *kernal-exponent-root*)

(define (kernal-set-exponent-root! er . ker)
  (let ((new-er (if (null? ker)
                    er
                    (car ker))))
    (set! *kernal-exponent-root* new-er)))

;;;;;;;;;;

;;        public void advanceExponent()
(define (kernal-advance-exponent)
  (set! *kernal-exponent-root* (1+ *kernal-exponent-root*))
  (if (= *kernal-exponent-root* *KERNAL-EXPONENT-LIMIT*)
      (set! *kernal-exponent-root* 1))
  (set! *kernal-exponent* *kernal-exponent-root*))

;;        public void advanceSigma()
(define (kernal-advance-sigma)
  (set! *kernal-sigma-root* (1+ *kernal-sigma-root*))
  (if (= *kernal-sigma-root* *KERNAL-SIGMA-LIMIT*)
      (set! *kernal-sigma-root* 1))
  (set! *kernal-sigma* (/ 1.0 *kernal-sigma-root*)))


;;        public void setKernal (Vector s)
(define (kernal-set-kernal! kernal s)
  (kernal-set-samples! kernal s)
  (let ((matrix (map (lambda (i) (map (lambda (j) (kernal-kernal (sample-vector i)
                                                                 (sample-vector j)))
                                      s))
                     s)))
    (kernal-set-matrix! kernal matrix)))

;;        public double getKernal(int i, int j)
(define (kernal-get-kernal kernal i j)
  (list-ref (list-ref (kernal-matrix kernal) i) j))

;;        public static double kernal (Vector v1, Vector v2)
(define (kernal-double-kernal v1 v2)
  (if (eq? *kernal-mode* *KERNAL-DOT*)
      (expt (kernal-dot v1 v2) *kernal-exponent*)
      (if (eq? *kernal-mode* *KERNAL-RADIAL*)
          (kernal-radial v1 v2)
          (kernal-dot v1 v2))))

;;        private static double radial (Vector v1, Vector v2)
(define (kernal-radial v1 v2)
  (exp (/ (- (kernal-difference2 v1 v2)) (* 2 *kernal-sigma* *kernal-sigma*))))

;;        private static double difference2 (Vector v1, Vector v2)
(define (kernal-difference2 v1 v2)
  (reduce + 0. (map square (map - v1 v2))))

;;        public static double dot (Vector v1, Vector v2)
(define (kernal-dot v1 v2)
  (reduce + 0. (map * v1 v2)))

;;        public static double kernal (Unknown u1, Unknown u2)
(define (kernal-kernal u1 u2)
  (let ((v1 (if (sample? u1)
                (sample-vector u1)
                (if (unknown? u1)
                    (unknown-vector u1)
                    u1)))
        (v2 (if (sample? u2)
                (sample-vector u2)
                (if (unknown? u2)
                    (unknown-vector u2)
                    u2))))
  (kernal-double-kernal v1 v2)))

