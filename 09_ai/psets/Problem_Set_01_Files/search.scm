;;;; -*- mode:Scheme -*- ;;;;
;;
;; search.scm
;;
;; This file contains the generalized search procedure
;; you've all come to know and love
;;

;; Ignore this; it aids compilation but affects nothing else.
(declare (usual-integrations))
(declare (integrate rest))

;;(require 'sort)

(define first car)
(define rest cdr)
(define second cadr)

;;; GENERALIZED SEARCH PROCEDURE

(define *left-overs* '()) ;; allows examining the Q after search ends.
(define *number-of-search-steps* 0)

;;; This allows implementing most of the search strategies (depth-first, etc.) by
;;; specifying the appropriate functional args.
;;; Returns a list of winning paths (possibly null).
;;; The nodes on each path on Q is kept in reversed order, for ease of access to
;;; the "head" node.

(define (search finish                        ; finish node
                how-many-paths                ; either 'all or an integer
                ;; The functions below specify the type of search
                pick-and-remove-path
                merge-paths-into-Q
                ;; The successor function depends on the particular problem
                successors
                ;; The state of the search
                Q                        ; current Q (list of paths)
                wins                        ; current winning paths
                )
  (cond
   ((Q-empty? Q)
    ;; If no paths left, return accumulated wins, if any
    (if (null? wins)
        (begin
          ;; Failed, at least be cute...
          (display* "Cain't get thar from heah.")
          wins)
        wins))
   (else
    ;; Get the next path to examine and remove it from Q (by side-effect).
    (let ((current (pick-and-remove-path Q))) ; get partial path to extend
      (set! *number-of-search-steps* (+ *number-of-search-steps* 1))
      (cond ((done? (path-head-node current) finish) ; are we there?
             (finish-action current Q)        ; do this upon finding a finish
             (cond
              ;; see if we've found enough solutions
              ((and (not (eq? how-many-paths 'all))
                    (<= how-many-paths (+ 1 (length wins))))
               ;; got enuf, we're outa here
               (set! *left-overs* Q)        ; so we can examine the queue
               ;; add current path to previous wins and return that.
               (cons (path-reverse current) wins))
              (else
               ;; found a finish state but still need to find more paths
               (search finish
                       how-many-paths
                       pick-and-remove-path
                       merge-paths-into-Q
                       successors
                       (merge-paths-into-Q (successors current) Q)
                       (cons (path-reverse current) wins)))))
            (else
             ;; Q still has entries in it and we haven't found finish yet, so
             ;; extend current path and merge results into path Q:
             (search finish how-many-paths
                     pick-and-remove-path
                     merge-paths-into-Q
                     successors
                     (merge-paths-into-Q (successors current) Q)
                     wins)))))))

;; The action which should be taken when the finish node is reached.
;; Here we simply print length of path and describe the path.

(define (finish-action current Q)
  (display* "Final path length is " (path-node-count current))
  ;; Called with reversed path, the way they are kept on Q
  (describe-path (path-reverse current)))

(define done? equal?)                        ; might change for some problems

(define *t:verifier* (list 'search-step-count))
(define (get-step-count)
  (list *t:verifier* *number-of-search-steps*))

;;; PATH OPERATIONS
;;; Paths are implemented as (reversed) lists of nodes, prefixed
;;; by a value (e.g. from heuristic), which may be #f.

(define (path? p)
  (and (pair? p)
       (or (eq? (path-value p) #f) (number? (path-value p)))
       (list? (path-nodes p))))

(define (make-path value nodes)        ; constructor
  (cons value nodes))

(define (path-value p)                        ; get the value of a path
  (first p))

(define (path-nodes p)                        ; get the nodes of a path
  (rest p))

(define (path-empty? p)                        ; is the path empty?
  (null? (path-nodes p)))

(define (path-head-node p)                ; returns first node
  (first (path-nodes p)))

(define (path-reverse p)                ; reverse the path
  (cons (path-value p) (reverse (path-nodes p))))

(define (path-node-count p)                ; count the nodes in a path
  (length (path-nodes p)))

;;; Q OPERATIONS
;;; Q is implemented as a list of paths, prefixed by the symbol Q.

(define (Q? Q)
  (and (pair? Q) (eq? (first Q) 'Q)))        ; (Q paths)

(define (make-Q path)                ; constructor with a single path
  (list 'Q path))

(define Q-paths rest)                        ; get the list of paths

(define (Q-empty? Q)                        ; any paths?
  (null? (rest Q)))

(define (Q-first-path Q)                ; first path on Q
  (if (Q-empty? Q)
      (error "Q is empty; no first path")
      (first (Q-paths Q))))

(define (Q-rest-paths Q)                ; rest of paths on Q
  (if (Q-empty? Q)
      (error "Q is empty; no rest paths")
      (rest (Q-paths Q))))

(define (Q-set-paths! Q paths)                ; modify Q, set the paths
  (if (Q? Q)
      (set-cdr! Q paths)
      (error "Not a Q" Q))
  Q)

;;; Used for blind searches, just pick the first path in the Q.
;;; modifies Q to remove the best path.

(define (pick-and-remove-first-path Q)
  (if (Q-empty? Q)
      (error "Trying to get first path, but Q is empty")
      (let ((path (Q-first-path Q)))
        (Q-set-paths! Q (Q-rest-paths Q))
        path)))

;;; Used for heuristic searches, pick the path with the best heuristic value
;;; modifies Q to remove the best path.

(define (pick-and-remove-best-path Q)
  ;; This relies on knowing the implementation of Q
  (define (loop P best-P)
    (cond ((null? (rest P))                ; no more paths
           (let ((best-path (second best-P)))
             ;; remove best path from Q
             (set-cdr! best-P (cddr best-P))
             ;; return it.
             best-path))
          ;; the value of the current path is better than the best so far
          ((< (path-value (second P)) (path-value (second best-P)))
           (loop (rest P) P))
          ;; keep going
          (else
           (loop (rest P) best-P))))
  (if (Q-empty? Q)
      (error "Trying to get first path, but Q is empty")
      ;; Treat first path as the best and start looking for a better one.
      (loop (rest Q) Q)))

;;;; THE ACTUAL SEARCH METHODS

;;; DEPTH-FIRST
(define (depth-first start finish)

  ;; A Q addition function specific to depth-first search
  ;; Add the new paths to the front of the queue
  (define (merge-paths-into-Q new-paths Q)
    (Q-set-paths! Q (append new-paths (Q-paths Q)))
    )

  (define (successors path)
    (extend-path path))

  (set! *number-of-search-steps* 0)

  ;; Fire up generalized search using Q constructor defined above:
  (search
   ;; Just start with a partial path including only start node.
   finish                                ; target node
   1                                        ; only 1 path wanted
   pick-and-remove-first-path                ; pick the first path from Q
   merge-paths-into-Q                        ; add to the front of Q
   successors                                ; successors of path
   ;; The initial Q, just one path = (start)
   (make-Q (make-path #f (list start))) ; initial Q
   '()                                        ; initial wins
   ))

;;; HILL-CLIMB (WITH BACKUP)
(define (hill-climb start finish)

  ;; A Q addition function specific to hill-climbing with backup
  ;; Add the (sorted) new paths to the front of the queue
  (define (merge-paths-into-Q new-paths Q)
    (Q-set-paths!
     Q
     (append (sort-paths-by-value new-paths) (Q-paths Q)))
    )

  (define (heuristic node)
    ;; in general, the heuristic value may depend on the finish node
    (get-heuristic-value node finish))

  (define (successors path)
    (extend-path-with-heuristic path heuristic))

  (set! *number-of-search-steps* 0)

  ;; Fire up generalized search using Q constructor defined above:
  (search
   ;; Just start with a partial path including only start node.
   finish                                ; finish node
   1                                        ; only 1 path wanted
   pick-and-remove-first-path                ; pick the first
   merge-paths-into-Q                        ; add to the front of Q
   successors                                ; successors, using heuristic
   ;; The initial Q, just one path = (start)
   (make-Q (make-path (heuristic start) (list start))) ; initial Q
   '()                                        ; initial wins
   ))

;;; HILL-CLIMB (NO BACKUP)
(define (hill-climb-no-backup start finish)

  ;; A Q addition function specific to hill-climbing without backup
  ;; Set Q to be just the sorted paths
  (define (merge-paths-into-Q new-paths Q)
    (Q-set-paths!
     Q (sort-paths-by-value new-paths))
    )

  (define (heuristic node)
    ;; in general, the heuristic value may depend on the finish node
    (get-heuristic-value node finish))

  (define (successors path)
    (extend-path-with-heuristic path heuristic))

  (set! *number-of-search-steps* 0)

  ;; Fire up generalized search using Q constructor defined above:
  (search
   ;; Just start with a partial path including only start node.
   finish                                ; finish node
   1                                        ; only 1 path wanted
   pick-and-remove-first-path                ; pick the first
   merge-paths-into-Q                        ; add to the front of Q
   successors                                ; successors of path (using heuristic)
   ;; the initial Q, just one path = (start)
   (make-Q (make-path (heuristic start) (list start))) ; initial Q
   '()                                        ; initial wins
   ))

;;; BEST-FIRST
(define (best-first start finish)

  ;; A Q addition function specific to best-first search
  ;; Add the new paths to the front of the queue, could go anywhere
  (define (merge-paths-into-Q new-paths Q)
    (Q-set-paths! Q (append new-paths (Q-paths Q)))
    )

  (define (heuristic node)
    ;; in general, the heuristic value may depend on the finishn node
    (get-heuristic-value node finish))

  (define (successors path)
    (extend-path-with-heuristic path heuristic))

  (set! *number-of-search-steps* 0)

  ;; Fire up generalized search using Q constructor defined above:
  (search
   ;; Just start with a partial path including only start node.
   finish                                ; goal node
   1                                        ; only 1 path wanted
   pick-and-remove-best-path                ; pick the best
   merge-paths-into-Q                        ; add to the front of Q
   successors                                ; successors, using heuristic
   ;; The initial Q, just one path = (start)
   (make-Q (make-path (heuristic start) (list start))) ; initial Q
   '()                                        ; initial wins
   ))

;;; Constructing successors to a path

;; Extend a path to the neighbors of the head node
;; Returns a list of extended paths (with values = #f)

(define (extend-path path)
  (display* "Extending the path " (path-reverse path))
  (remove-falses
   (map
    (lambda (next-node)
      (if (member next-node (path-nodes path))
          #f
          (make-path
           #f                                ; path value is not relevant here
           (cons next-node (path-nodes path)))))
    (get-neighboring-nodes (path-head-node path)))))

;; Extend a path to the neighbors of the head node
;; Returns a list of extended paths (with values given by the heuristic function)

(define (extend-path-with-heuristic path heuristic)
  (display* "Extending the path " (path-reverse path))
  (remove-falses
   (map
    (lambda (next-node)
      (if (member next-node (path-nodes path))
          #f
          (make-path
           (heuristic next-node)        ; path value is from calling heuristic
           (cons next-node (path-nodes path)))))
    (get-neighboring-nodes (path-head-node path)))))

;; Sort the paths based on the heuristic value
(define (sort-paths-by-value paths)
  (sort paths (lambda (x y) (< (path-value x) (path-value y)))))

;;;; VARIOUS AUXILIARIES

(define (describe-path path)
  (display* "The path is: ")
  (for-each display* (rest path))
  'ok)

;;; This is a simple stepper to allow you to test the extend-path code
(define (step-by-step path)
  ;;; generate a simple path by taking the first successor of the first node on the path
  (display* "The path " path)
  (let ((extensions (extend-path path)))
    (if (null? extensions)
        (display " cannot be extended any further.\n")
            (begin
          (display* " can be extended to get " (path-head-node extensions) "\n")
          (display "Continue? [y or n]: ")
          (if (eq? 'y (read))
              (step-by-step (path-head-node (extend-path path)))
              '())))))

;;; DATA DEPENDENT OPERATIONS

;;; A little test network (the one from the on-line material).
;;; Each sublist is (node . connected-nodes) - this is unidirectional.
(define *data*
  '((S A B)
    (A C D)
    (B D G)
    (C)
    (D C G)
    (G)))

(define (get-neighboring-nodes node)
  (let ((ans (assoc node *data*)))
    (if ans
        (rest ans)
        (error "get-neighboring-nodes:Unknown node" node))))

;; Trivial heuristic values for fixed finish node G
(define *heuristic-values*
  '((a 2) (b 3) (c 1) (d 4) (s 10) (g 0)))

;; in general, the heuristic value may depend on the finish node
;; but, here, we simply lookup heuristic value in a table
(define (get-heuristic-value node finish)
  (let ((ans (assoc node *heuristic-values*)))
    (if ans
        (second ans)
        (error "get-heuristic-value:Unknown node" node))))

(define *t:silent* #f)
(define (display* . l)
  ;; Print the list of arguments
  (cond (*t:silent* #f)
        (else
         (for-each display l)
         (newline))))

;;; Removes false entries from the list

(define (remove-falses x)
  (if (null? x)
      '()
      (if (car x)
          (cons (car x) (remove-falses (cdr x)))
          (remove-falses (cdr x)))))
