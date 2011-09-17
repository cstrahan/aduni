;;;; svm-draw.scm

(declare (usual-integrations))


;;; Size of the window in pixels, or half that if using pixel doubling.
(define *draw-window-width* 170)
(define *draw-window-height* 150)
;;; Numerical limits on the range of inputs that we wish to plot. (x = x1, y = x2)
(define *draw-max-x* 1.1)
(define *draw-min-x* -0.1)
(define *draw-max-y* 1.1)
(define *draw-min-y* -0.1)
(define *tic-period* 1.0)
;;; The window that is used to draw in each time.
(define *draw-window* #f)
;;; Double (quadruple, actually) pixel size to be more visible.
(define *draw-pixel-double* #t)
;;; Show image progressively (slower). Turned off during training. Always
;;; shows progress (very slowly) under X anyway.
(define *draw-show-progress* #t)
;;; Plot the points in the array *sample-points*.
(define *draw-plot-training* #t)
(define *sample-points* '())

(define *draw-print-data* #t)
(define *draw-save-bitmaps* #f)
(define *draw-bitmap-name* "draw")
(define *draw-bitmap-number* 0)


;; Draw the picture for a given matrix. If 'graphics' is provided, it will
;; reuse that window - so (draw m *draw-window*) would be an easy way to
;; draw in the current window.
;; 'matrix' is assumed to be a grid of color values - either 3-element
;; RGB lists of exact integers, or a floating point between 0.0 and 1.0
;; (which translates to a color between red and blue).
;; If *draw-plot-training* is true, it will also draw dots for each of
;; the points in *sample-points*.
(define (draw matrix . graphics)
  (begin
    (display* "Drawing image...")
    (set! *draw-window-height* (length matrix))
    (set! *draw-window-width* (length (car matrix)))
    (let* ((step-x (/ (- *draw-max-x* *draw-min-x*) *draw-window-width*))
           (step-y (/ (- *draw-max-y* *draw-min-y*) *draw-window-height*))
           (step-x-half (/ step-x 2))
           (step-y-half (/ step-y 2))
           (g (if (and (not (null? graphics)) (not (graphics-changed?)))
                  (car graphics)
                  (let ((gtmp (if *draw-pixel-double*
                                  (graphics-create (* 2 *draw-window-width*)
                                                   (* 2 *draw-window-height*))
                                  (graphics-create *draw-window-width*
                                                   *draw-window-height*))))
                    (if (and (not (null? graphics)) (car graphics))
                        (graphics-close (car graphics)))
                    (if *draw-window*
                        (graphics-close *draw-window*))
                    (set! *draw-window* gtmp)
                    (if *draw-pixel-double*
                        (graphics-operation gtmp 'resize-window (* 2 *draw-window-width*) (* 2 *draw-window-height*))
                        (graphics-operation gtmp 'resize-window *draw-window-width* *draw-window-height*))
                    (graphics-set-coordinate-limits gtmp *draw-min-x* *draw-min-y* *draw-max-x* *draw-max-y*)
                    (graphics-enable-buffering gtmp)
                    (graphics-clear gtmp)
                    gtmp))))
      (do ((y *draw-min-y* (+ y step-y))
           (lines matrix (cdr lines)))
          ((null? lines)
           (if *draw-show-progress* (graphics-flush g))
           'ok)
        (if (and *draw-show-progress* (zero? (remainder (length lines) 4)))
            (graphics-flush g))
        (do ((x *draw-min-x* (+ x step-x))
             (line (car lines) (cdr line)))
            ((null? line)
             'ok)
          (let* ((val (car line))
                 (color (if (pair? val)
                            val
                            (list (round->exact (* val 255)) 0 (round->exact (* (- 1 val) 255))))))
            (graphics-set-color g color)
            (graphics-draw-point g x y)
            (if *draw-pixel-double*
                (begin (graphics-draw-point g (+ x step-x-half) y)
                       (graphics-draw-point g (+ x step-x-half) (+ y step-y-half))
                       (graphics-draw-point g x (+ y step-y-half)))))))
      (draw-axes g *tic-period* *tic-period*)
      (if *draw-plot-training*
          (plot-points g *sample-points*))
      (graphics-flush g)
      'ok
      )))

(define *draw-window-width-old* 0)
(define *draw-window-height-old* 0)
(define (graphics-changed?)
  (let ((res (and (= *draw-window-width* *draw-window-width-old*)
                  (= *draw-window-height* *draw-window-height-old*))))
    (set! *draw-window-width-old* *draw-window-width*)
    (set! *draw-window-height-old* draw-window-height*)
    res))

(define (set-window-size width height)
  (set! *draw-window-width* width)
  (set! *draw-window-height* height))

(define (set-coordinate-limits x-min x-max y-min y-max)
  (set! *draw-min-x* x-min)
  (set! *draw-max-x* x-max)
  (set! *draw-min-y* y-min)
  (set! *draw-max-y* y-max))

;; Draw the points in 'point-set'. Each one is in the form:
;; (x y)       - draws a black dot at (x,y).
;; ((x y))     - same.
;; ((x y) c)   - draws a dot at (x,y). If 'c' is an exact integer, the dot is
;;               white if 'c' is 1 and black otherwise. If 'c' is a floating
;;               point 0.0 - 1.0, it is a corresponding gray.
;; ((x y) c s) - same, with the addition of drawing a green circle around the
;;               point if 's' is true.
(define (plot-points g point-set)
  (let* ((step-x (/ (- *draw-max-x* *draw-min-x*) *draw-window-width*))
         (step-y (/ (- *draw-max-y* *draw-min-y*) *draw-window-height*))
         (step-x-half (/ step-x 2))
         (step-y-half (/ step-y 2)))
    (for-each
     (lambda (s)
       (let* ((pt (if (pair? (car s))
                      s
                      (list s)))
              (x (first (first pt)))
              (y (second (first pt)))
              (class (if (> (length pt) 1)
                         (second pt)
                         0)))
         (if (and (exact? class) (integer? class))
             (if (= class 1)  ;; white if 1, black otherwise
                 (graphics-set-color g '(254 254 254))
                 (graphics-set-color g '(1 1 1)))
             (graphics-set-color g (list (round->exact (* class 255))
                                         (round->exact (* class 255))
                                         (round->exact (* class 255)))))
         (graphics-draw-point g x y)
         (if *draw-pixel-double*
             (begin (graphics-draw-point g (+ x step-x-half) y)
                    (graphics-draw-point g (+ x step-x-half) (+ y step-y-half))
                    (graphics-draw-point g x (+ y step-y-half))
                    (graphics-draw-point g (- x step-x-half) y)
                    (graphics-draw-point g (- x step-x-half) (+ y step-y-half))
                    (graphics-draw-point g (+ x step-x) y)
                    (graphics-draw-point g (+ x step-x) (+ y step-y-half))
                    (graphics-draw-point g x (- y step-y-half))
                    (graphics-draw-point g (+ x step-x-half) (- y step-y-half))
                    (graphics-draw-point g x (+ y step-y))
                    (graphics-draw-point g (+ x step-x-half) (+ y step-y))
                    ))
         (if (and (> (length s) 2) (third pt))
             (let ((center-x (+ x (/ step-x-half 2)))
                   (center-y (+ y (/ step-y-half 2)))
                   (radius (* 2.25 (max step-x step-y))))
               ;;(display* "Found support in: " pt)
               (draw-ring g center-x center-y radius '(0 247 0))
               ;;(draw-ring g center-x center-y (* 0.9 radius) '(0 200 0))
               ))))
     point-set)))

;; Make an empty matrix, which stores color values for a drawing.
;; Probably will not be used by most programs.
(define (make-matrix width height)
  (define (add-row n)
    (if (= n height)
        '()
        (cons (make-list width #f) (add-row (1+ n)))))
  (add-row 0))

;; Create a matrix by probing a function for all values in the current
;; x-y display. Set the virtual and screen dimensions first.
(define (function-matrix func)
  (begin
    (let* ((step-x (/ (- *draw-max-x* *draw-min-x*) *draw-window-width*))
           (step-y (/ (- *draw-max-y* *draw-min-y*) *draw-window-height*))
           (step-x-half (/ step-x 2))
           (step-y-half (/ step-y 2)))
      (do ((y *draw-min-y* (+ y step-y))
           (lines 0 (+ lines 1))
           (m '()))
          ((> lines *draw-window-height*)
           (reverse m))
        (set! m (cons (do ((x *draw-min-x* (+ x step-x))
                           (cols 0 (+ cols 1))
                           (r '()))
                          ((> cols *draw-window-width*)
                           (reverse r))
                        (set! r (cons (func x y) r)))
                      m)))
      )))

;; draw the coordinate axes, with tic marks
(define (draw-axes g unit-x unit-y)
  (let ((step-x (/ (- *draw-max-x* *draw-min-x*) *draw-window-width*))
        (step-y (/ (- *draw-max-y* *draw-min-y*) *draw-window-height*))
        (small-x (* 3.0 (/ (- *draw-max-x* *draw-min-x*) *draw-window-width*)))
        (small-y (* 3.0 (/ (- *draw-max-y* *draw-min-y*) *draw-window-height*)))
        (y-limit (max (abs *draw-max-y*) (abs *draw-min-y*)))
        (x-limit (max (abs *draw-max-x*) (abs *draw-min-x*))))
    (graphics-set-color g '(0 0 0))
    (graphics-draw-line g 0 *draw-min-y* 0 *draw-max-y*)
    (graphics-draw-line g *draw-min-x* 0 *draw-max-x* 0)
    (do ((y unit-y (+ y unit-y)))
        ((> y y-limit)
         'ok)
      (graphics-draw-line g (- small-x) y small-x y)
      (graphics-draw-line g (- small-x) (- y) small-x (- y)))
    (do ((x unit-x (+ x unit-x)))
        ((> x x-limit)
         'ok)
      (graphics-draw-line g x (- small-y) x small-y)
      (graphics-draw-line g (- x) (- small-y) (- x) small-y))
    (graphics-flush g)))

;; draw a circle with the given parameters
(define (draw-ring g x y r color)
  (graphics-set-color g color)
  (if (is-win32?)
      (graphics-operation g 'draw-ellipse (- x r) (- y r) (+ x r) (+ y r))
      (if (is-X?)
          (graphics-operation g 'draw-circle x y r)
          'graphics-type-not-supported-yet)))

;; set the drawing color (MIT Scheme really reads to have its
;; graphics functions reworked)
(define (graphics-set-color g c)
  (let ((color (if (is-win32?)
                   c
                   (rgb->x-string c))))
    (graphics-operation g 'set-foreground-color color)))

;; create a graphics window - X messes up if you create a window and
;; resize it
(define (graphics-create width height)
  (if (is-win32?)
      (make-graphics-device 'win32 width height 'standard)
      (if (is-X?)
          (make-graphics-device 'x #f (string-append (number->string width) "x" (number->string height)) #f)
          (make-graphics-device #f))))

;; writes a line of text to the bottom of the image.
(define (graphics-write-note g txt)
  (graphics-set-color g '(247 247 247))
  (graphics-draw-text g *draw-min-x* *draw-min-y* txt)
  (graphics-flush g))

;; saves the current window as a bitmap (win32 only; I have a PPM
;; encoder/decoder that I wrote in Scheme, but I can't find it.)
(define (save-draw-window . graphics)
  (let ((g (if (null? graphics)
               *draw-window*
               (car graphics))))
    (graphics-operation g
                        'save-bitmap
                        (string-append *draw-bitmap-name* (number->string *draw-bitmap-number*)))
    (set! *draw-bitmap-number* (1+ *draw-bitmap-number*))
    (-1+ *draw-bitmap-number*)))

;; name to use when saving
(define (set-bitmap-name f)
  (set! *draw-bitmap-name* f))

;; reset appended number to 0
(define (reset-bitmap-number)
  (set! *draw-bitmap-number* 0))

(define (rgb->x-string c)
  (string-append "#"
                 (int->hex-string (first c))
                 (int->hex-string (second c))
                 (int->hex-string (third c))))

(define (int->hex-string i)
  (let ((s (number->string i 16)))
    (if (< (string-length s) 2)
        (string-append "0" s)
        s)))

;; check if the current system is Win32
(define (is-win32?)
  (string-ci=? microcode-id/operating-system-name "nt"))

;; check if the current system is a unix variant (for X display)
(define (is-X?)
  (string-ci=? microcode-id/operating-system-name "unix"))


;;; DISPLAY

(define *t:silent* #f)
(define (display* . l)
  ;; Print the list of arguments
  (cond (*t:silent* #f)
        (else
         (for-each display l)
         (newline))))


