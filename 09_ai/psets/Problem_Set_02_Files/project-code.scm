;;;; project-code.scm
;;
;; Support code for 6.034 Project II
;;     Sorting news articles for International Conglomerates, Inc.
;;
;; This file provides accessors and abstractions for a news
;; article corpus, which consists of articles stored as 
;; s-expressions in a text file.
;; Also, a simple testing interface.

(define *verbose* #t)

;; accessors
(define article-id car)			; a number
(define (article-category x) (car (cadr x))) ; a symbol, e.g. organic, etc.
(define article-title caddr)		; a list of symbols
(define article-header cadddr)		; a list of symbols
(define (article-body x) (cadr (cdddr x))) ; a list of symbols

;; retrieves an article based on its id
(define (get-article corpus id)
  (with-input-from-file corpus
    (lambda ()
        (do ((input (read) (read)))
            ((or (eof-object? input) (= (article-id input) id)) input)))))

;; reads a corpus from a file and returns the result of the function f on each 
;; article in the corpus.
(define (corpus-map f corpus)
  (define (loop)
    (let ((input (read)))
      (cond ((eof-object? input) '())
	    (else
	     (if *verbose*
		 (display* "Reading Article " (article-id input)))
	     (let ((result (f input)))
	       (cons result (loop)))))))
  (with-input-from-file corpus loop))

;; The testing interface.  We will evaluate the project by calling project-test 
;; on a corpus file we have saved.  You should make sure that calling project test
;; will initialize any necessary values.  If you use a neural net or an id-tree, 
;; make sure that you store the values of the weights or the tree in a file that 
;; you load from this file.  You should NOT attempt to do the training at the time
;; of testing.  The output of project-test should be a list of divisions, drawn 
;; from the list below.

(define *divisions* '(organic financial metal petro))					      

;; This is the trivial classifier that just guesses randomly.  You should do better
;; than this...
(define (random-classifier doc)
  (let ((result (list-ref *divisions* (random 4))))
    (display* (list (article-id doc) result))
    result))

;; Redefine this function to use your classification code.
(define (project-test corpus)
  (fluid-let ((*verbose* #f))
    (with-output-to-file "project-output"
      (lambda () (corpus-map random-classifier corpus)))))


;; Added 12/01:
;; Our old helper functions.

(define *t:silent* #f)
(define (display* . l)
  ;; Print the list of arguments
  (cond (*t:silent* #f)
	(else
	 (for-each display l)
	 (newline))))

;;;  Purpose:	   Remove duplicates in a list.
;;;  Sample argument: (a b c b d)
;;;  Sample value:    (a b c d)
(define (remove-duplicates l)
  (cond ((null? l) '())
	((member (first l) (cdr l)) (remove-duplicates (cdr l)))
	(else (cons (first l) (remove-duplicates (cdr l))))))

