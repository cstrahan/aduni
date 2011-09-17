;;;; svmmodel.scm

(declare (usual-integrations))


; class SvmModel extends Observable
; {
;; svmmodel class:
;;  '( 'SVMMODEL .
;;     '( Vector samples = new Vector()
;;        Kernal kernal = new Kernal(new Vector())
;;        double b = 0
;;        double c = 1000
;;        double tolerance = 1e-2
;;        Vector laGrangians = new Vector()
;;        ErrorCache errorCache ) )

(define *SVMMODEL-SHADING* 0)
(define *SVMMODEL-GRAPHING* 1)
(define *SVMMODEL-ADVANCING* 2)

;;        public SvmModel (Svm s)
(define (new-svmmodel)
  (let ((sm (make-tagged 'SVMMODEL (list '()
                                         (new-kernal '())
                                         0.
                                         1000.
                                         1e-2
                                         '()
                                         #f))))
    (svmmodel-add-sample sm (new-sample 0.3 0.4 -1))
    (svmmodel-add-sample sm (new-sample 0.4 0.3 -1))
    (svmmodel-add-sample sm (new-sample 0.6 0.6 1))
    (svmmodel-add-sample sm (new-sample 0.8 0.8 1))
        sm))

;;;; Note: comment out the above sample points for a practical system!

(define (svmmodel? obj)
  (is-tagged-type obj 'SVMMODEL))

;;;;;;;;;;

(define (svmmodel-show-fields)
  '(SVMMODEL samples kernal b c tolerance laGrangians errorCache))

(define (svmmodel-samples svmmodel)
  (list-ref svmmodel 1))

(define (svmmodel-kernal svmmodel)
  (list-ref svmmodel 2))

(define (svmmodel-b svmmodel)
  (list-ref svmmodel 3))

(define (svmmodel-c svmmodel)
  (list-ref svmmodel 4))

(define (svmmodel-tolerance svmmodel)
  (list-ref svmmodel 5))

(define (svmmodel-lagrangians svmmodel)
  (list-ref svmmodel 6))

(define (svmmodel-errorcache svmmodel)
  (list-ref svmmodel 7))

(define (svmmodel-set-samples! svmmodel s)
  (list-set! svmmodel 1 s))

(define (svmmodel-set-kernal! svmmodel k)
  (list-set! svmmodel 2 k))

(define (svmmodel-set-b! svmmodel b)
  (list-set! svmmodel 3 b))

(define (svmmodel-set-c! svmmodel c)
  (list-set! svmmodel 4 c))

(define (svmmodel-set-tolerance! svmmodel t)
  (list-set! svmmodel 5 t))

(define (svmmodel-set-lagrangians! svmmodel l)
  (list-set! svmmodel 6 l))

(define (svmmodel-set-errorcache! svmmodel ec)
  (list-set! svmmodel 7 ec))

;;;;;;;;;;

;;        public void addSample(Sample s)
(define (svmmodel-add-sample svmmodel s)
  (svmmodel-set-samples! svmmodel (append! (svmmodel-samples svmmodel) (list s)))
  (svmmodel-update svmmodel))

;;        public void clearSamples()
(define (svmmodel-clear-samples svmmodel)
  (svmmodel-set-samples! svmmodel '())
  (svmmodel-update svmmodel))

;;        public Vector getSamples()
(define (svmmodel-get-samples svmmodel)
  (svmmodel-samples svmmodel))

;;        public void update ()
(define (svmmodel-update svmmodel)
  (svmmodel-set-changed svmmodel)
  (svmmodel-notify-observers svmmodel))

;;        public void learn ()
(define (svmmodel-learn svmmodel)
  (svmmodel-main-learning-routine svmmodel)
  (svmmodel-set-changed svmmodel)
  (svmmodel-notify-observers *SVMMODEL-SHADING*))


(define (svmmodel-set-changed svmmodel . a)
  'ignore)

(define (svmmodel-notify-observers svmmodel . a)
  'ignore)

;;        public void mainLearningRoutine ()
(define (svmmodel-main-learning-routine svmmodel)
  (let ((samples (svmmodel-samples svmmodel))
        (kernal (svmmodel-kernal svmmodel)))
    (kernal-set-kernal! kernal samples)
    (svmmodel-set-errorcache! svmmodel (new-errorcache (length samples)))
    (for-each (lambda (s) (sample-set-alpha! s 0.0)) samples)
    (svmmodel-set-b! svmmodel 0.0)
    (svmmodel-set-lagrangians! svmmodel '())
    (do ((changes 0)
         (check-all-switch #t)
         (change-seen-switch #f))
        ((not (or change-seen-switch check-all-switch))
         (svmmodel-report svmmodel))
      (set! change-seen-switch #f)
      (if check-all-switch
          (for-each (lambda (i) (if (svmmodel-workon svmmodel i)
                                    (begin (set! change-seen-switch #t)
                                           (svmmodel-add-lagrangian svmmodel))))
                    (index-list samples))
          (for-each (lambda (i s) (if (and (svmmodel-unbound svmmodel s)
                                           (svmmodel-workon svmmodel i))
                                      (begin (set! change-seen-switch #t)
                                             (svmmodel-add-lagrangian svmmodel))))
                    (index-list samples) samples))
      (if check-all-switch
          (set! check-all-switch #f)
          (if (not change-seen-switch)
              (set! check-all-switch #t)))
      (if (> (length (svmmodel-lagrangians svmmodel)) 20000)
          (begin (display "Too many steps xxxxxxxxxxxxxxxxxxxxxxxxxx\n")
                 (set! change-seen-switch #f)
                 (set! check-all-switch #f)))
    )
    (svmmodel-report svmmodel)))

;;        private void report ()
(define (svmmodel-report svmmodel)
  (for-each
    (lambda (s)
      (let ((result (svmmodel-output svmmodel s))
            (tolerance (svmmodel-tolerance svmmodel)))
        (sample-set-value! s result)
        (if (or (and (< (sample-y s) 0) (> result (+ -1 tolerance)))
                (and (> (sample-y s) 0) (< result (-  1 tolerance))))
            (sample-set-wrong! s #t)
            (sample-set-wrong! s #f))
        (if (<= (- (abs result) 1) tolerance)
            (sample-set-support! s #t)
            (sample-set-support! s #f))
        ;;(display* "Result on " (sample-vector s) " is " result
        ;;          "; alpha is " (sample-alpha s))
        ))
    (svmmodel-samples svmmodel)))

;; System.out.println("Result on " + s.vector() + " is " + result + "; alpha is " + s.alpha());

;;        private void addLaGrangian ()
(define (svmmodel-add-lagrangian svmmodel)
  (svmmodel-set-lagrangians! svmmodel (cons (svmmodel-lagrangian svmmodel)
                                            (svmmodel-lagrangians svmmodel))))

;;        public Vector getLaGrangians ()
(define (svmmodel-get-lagrangians svmmodel)
  (svmmodel-lagrangians svmmodel))

;;        public Vector getWeights ()
(define (svmmodel-get-weights svmmodel)
  (map sample-alpha (svmmodel-samples svmmodel)))

;;        private double error (int i)
(define (svmmodel-error svmmodel i)
  (let* ((s (list-ref (svmmodel-samples svmmodel) i))
         (errorcache (svmmodel-errorcache svmmodel))
         (o (errorcache-get-error errorcache i)))
    (if (not o)
        (errorcache-set-error errorcache i (- (svmmodel-output svmmodel s) (sample-y s))))
    (errorcache-get-error errorcache i)))

;;        private boolean workOn(int i2)
(define (svmmodel-workon svmmodel i2)
  (let* ((s2 (list-ref (svmmodel-samples svmmodel) i2))
         (alpha2 (sample-alpha s2))
         (y2 (sample-y s2))
         (e2 (svmmodel-error svmmodel i2))
         (r2 (* e2 y2))
         (c (svmmodel-c svmmodel))
         (tolerance (svmmodel-tolerance svmmodel))
         (samples (svmmodel-samples svmmodel))
         (samples-len (length samples)) )
    (if (or (and (< r2 (- tolerance)) (< alpha2 c))
            (and (> r2 tolerance) (> alpha2 0)))
        (if (and (> (svmmodel-unbound-count svmmodel) 1)
                 (svmmodel-take-step svmmodel (svmmodel-select-partner svmmodel i2) i2))
            #t
            (let ((offset (random samples-len)))
              (if (not (for-all? (index-list samples)
                         (lambda (i) (let* ((index (modulo (+ i offset) samples-len))
                                            (s (list-ref samples index)) )
                                       (or (svmmodel-unbound svmmodel s)
                                           (not (svmmodel-take-step svmmodel index i2)))))
                        ))
                  #t
                  (let ((offset (random samples-len)))
                    (if (not (for-all? (index-list samples)
                               (lambda (i) (let* ((index (modulo (+ i offset) samples-len)))
                                             (not (svmmodel-take-step svmmodel index i2))))
                              ))
                        #t
                        #f)
                   ))))
        #f)))

;;        private int selectPartner (int s)
(define (svmmodel-select-partner svmmodel s)
  (let ((max-found 0.0)
        (answer 0)
        (error-s (svmmodel-error svmmodel s)))
    (for-each (lambda (i)
                (let ((testvalue (abs (- error-s (svmmodel-error svmmodel i)))))
                  (if (> testvalue max-found)
                      (begin (set! max-found testvalue)
                             (set! answer i)))))
              (index-list (svmmodel-samples svmmodel)))
    answer))

;;        private int unboundCount ()
(define (svmmodel-unbound-count svmmodel)
  (let ((count 0))
    (for-each (lambda (s) (if (svmmodel-unbound svmmodel s)
                              (set! count (1+ count))))
              (svmmodel-samples svmmodel))
    count))

;;        private boolean unbound(Sample s)
(define (svmmodel-unbound svmmodel s)
  (let ((alpha (sample-alpha s))
        (c (svmmodel-c svmmodel)) )
    (and (not (= alpha 0)) (not (= alpha c)))))

;;        private boolean takeStep(int i1, int i2)
(define (svmmodel-take-step svmmodel i1 i2)
  (let* ((eps 1e-20)
         (b (svmmodel-b svmmodel))
         (c (svmmodel-c svmmodel))
         (a1 #f)
         (a2 #f) )
    (if (= i1 i2)
        #f
        (let* ((samples (svmmodel-samples svmmodel))
               (s1 (list-ref samples i1))
               (s2 (list-ref samples i2))
               (alpha1 (sample-alpha s1))
               (alpha2 (sample-alpha s2))
               (y1 (sample-y s1))
               (y2 (sample-y s2))
               (s (* y1 y2))
               (e1 (svmmodel-error svmmodel i1))
               (e2 (svmmodel-error svmmodel i2))
               (L (svmmodel-lbound svmmodel y1 y2 alpha1 alpha2))
               (H (svmmodel-hbound svmmodel y1 y2 alpha1 alpha2)) )
          (if (= L H)
              #f
              (let* ((kernal (svmmodel-kernal svmmodel))
                     (k11 (kernal-get-kernal kernal i1 i1))
                     (k12 (kernal-get-kernal kernal i1 i2))
                     (k22 (kernal-get-kernal kernal i2 i2))
                     (eta (- (* 2 k12) k11 k22)) )
                (if (< eta 0)
                    (begin (set! a2 (- alpha2 (/ (* y2 (- e1 e2)) eta)))
                           (if (< a2 L)
                               (set! a2 L)
                               (if (> a2 H)
                                   (set! a2 H))))
                    (let ((Lobj (svmmodel-objective-at-a2 svmmodel y1 y2 alpha1 alpha2 L s1 s2))
                          (Hobj (svmmodel-objective-at-a2 svmmodel y1 y2 alpha1 alpha2 H s1 s2)) )
                      (if (> Lobj (+ Hobj eps))
                          (set! a2 L)
                          (if (< Lobj (- Hobj eps))
                              (set! a2 H)
                              (set! a2 alpha2)))))
                (if (< a2 1e-8)
                    (set! a2 0.)
                    (if (> a2 (- c 1e-8))
                        (set! a2 c)))
                (if (< (abs (- a2 alpha2)) (* eps (+ a2 alpha2 eps)))
                    #f
                    (let* ((a1 (+ alpha1 (* s (- alpha2 a2))))
                           (b-new (svmmodel-threshold svmmodel e1 e2 y1 y2 alpha1 alpha2 a1 a2 s1 s2 b)) )
                      (svmmodel-set-b! svmmodel b-new)
                      (sample-set-alpha! s1 a1)
                      (sample-set-alpha! s2 a2)
                      (errorcache-clear-cache (svmmodel-errorcache svmmodel))
                      #t)
                 )))))))

;;        private double threshold (double error1, double error2,
;;                                  int y1, int y2,
;;                                  double old1, double old2,
;;                                  double new1, double new2,
;;                                  Sample v1, Sample v2, double b)
(define (svmmodel-threshold svmmodel error1 error2 y1 y2 old1 old2
                                     new1 new2 v1 v2 b)
  (let* ((b (svmmodel-b svmmodel))
         (c (svmmodel-c svmmodel))
         (b1 (+ error1
                (* y1 (- new1 old1) (kernal-kernal v1 v1))
                (* y2 (- new2 old2) (kernal-kernal v1 v2))
                b))
         (b2 (+ error2
                (* y1 (- new1 old1) (kernal-kernal v1 v2))
                (* y2 (- new2 old2) (kernal-kernal v2 v2))
                b))
         (average (/ (+ b1 b2) 2.0)))
    (if (and (and (> new1 0) (< new1 c))
             (or (<= new2 0) (>= new2 c)))
        b1
       (if (and (and (> new2 0) (< new2 c))
                (or (<= new1 0) (>= new1 c)))
           b2
           average))))

;;        private double lBound (int y1, int y2, double x1, double x2)
(define (svmmodel-lbound svmmodel y1 y2 x1 x2)
  (let ((c (svmmodel-c svmmodel)))
    (if (not (= y1 y2))
        (max 0.0 (- x2 x1))
        (max 0.0 (- (+ x2 x1) c)))))

;;        private double hBound (int y1, int y2, double x1, double x2)
(define (svmmodel-hbound svmmodel y1 y2 x1 x2)
  (let ((c (svmmodel-c svmmodel)))
    (if (not (= y1 y2))
        (min c (- (+ c x2) x1))
        (min c (+ x2 x1)))))

;;        private double objectiveAtA2 (int y1, int y2,
;;                                      double alpha1, double alpha2, double new2,
;;                                       Sample s1, Sample s2)
(define (svmmodel-objective-at-a2 svmmodel y1 y2 alpha1 alpha2 new2 s1 s2)
  (let* ((s   (* y1 y2))
         (g   (+ alpha1 (* s alpha2)))
         (k11 (kernal-kernal s1 s1))
         (k12 (kernal-kernal s1 s2))
         (k21 k12)
         (k22 (kernal-kernal s2 s2))
         (b   (svmmodel-b svmmodel))
         (v1  (- (+ (svmmodel-output svmmodel s1) b)
                 (* y1 alpha1 k11)
                 (* y2 alpha2 k21)))
         (v2  (- (+ (svmmodel-output svmmodel s2) b)
                 (* y1 alpha1 k12)
                 (* y2 alpha2 k22)))
         (e   (- g (* s new2)))
         (w   (- (+ (- g (* s new2)) new2)
                 (* 0.5 k11 e e)
                 (* 0.5 k22 new2 new2)
                 (* s k12 e new2)
                 (* y1 e v1)
                 (* y2 new2 v2))) )
    w))

;;        public double laGrangian ()
(define (svmmodel-lagrangian svmmodel)
  (let ((samples (svmmodel-samples svmmodel))
        (sum1 0.0)
        (sum2 0.0))
    (for-each (lambda (s) (set! sum1 (+ sum1 (sample-alpha s))))
              samples)
    (for-each (lambda (si) (for-each (lambda (sj)
                                       (set! sum2 (+ sum2 (* (sample-y si) (sample-y sj)
                                                             (kernal-kernal si sj)
                                                             (sample-alpha si) (sample-alpha sj)))))
                                     samples))
              samples)
    (- sum1 (* 0.5 sum2))))


        ;; Whew, now handle some regular stuff

;;        public double output (Unknown u)
(define (svmmodel-output svmmodel u)
  (let ((sum (- (svmmodel-b svmmodel))))
    (for-each (lambda (s) (set! sum (+ sum (* (sample-y s) (sample-alpha s) (kernal-kernal u s)))))
              (svmmodel-samples svmmodel))
    sum))

;;        public void advanceKernalChoice()
(define (svmmodel-advance-kernal-choice svmmodel)
  (let ((mode (kernal-mode)))
    (if (eq? mode *KERNAL-DOT*)
        (begin (kernal-set-mode! *KERNAL-RADIAL*)
               (kernal-set-sigma-root! 1)))
    (if (eq? mode *KERNAL-RADIAL*)
        (begin (kernal-set-mode! *KERNAL-DOT*)
               (kernal-set-exponent-root! 1)))
    (svmmodel-set-changed svmmodel)
    (svmmodel-notify-observers svmmodel *SVMMODEL-ADVANCING*)))

;;        public void advanceParameterChoice()
(define (svmmodel-advance-parameter-choice svmmodel)
  (let ((mode (kernal-mode)))
    (if (eq? mode *KERNAL-DOT*)
        (kernal-advance-exponent))
    (if (eq? mode *KERNAL-RADIAL*)
        (kernal-advance-sigma))
    (svmmodel-set-changed svmmodel)
    (svmmodel-notify-observers svmmodel *SVMMODEL-ADVANCING*)))

(define (svmmodel-sample-map svmmodel)
  (let ((samples (svmmodel-samples svmmodel)))
    (map (lambda (s) (list (list (sample-x1 s) (sample-x2 s))
                           (sample-y s)
                           (sample-support s)))
         samples)))

