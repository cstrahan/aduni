;;;; -*- mode:Scheme -*- ;;;;
;;
;; schedule.scm
;;
;; Planning for a simple robot using the forward-chainer and the
;; search code.

;; Ignore this; it aids compilation but affects nothing else.
(declare (usual-integrations))

;;
;; Given a state of the world (list of assertions), we need to
;; construct all the triggered rule-instances and fire each of them in
;; parallel to construct many alternative states, these are the
;; neighboring nodes of the initial state.

;; get-neighboring-nodes
;;
;; Given a state, returns a list of states reachable by applying
;; one rule instantiation

(define (get-neighboring-nodes state)
  (fluid-let ((*trace* #f))
    (let ((instances
           ;; adding assertions modifies the list so we need to get a COPY.
           ;; the state defines the assertions
           (fluid-let ((*assertions* (copy-list state)))
             (apply append         ; append the rule-instances from each rule
                    (map build-rule-instances (get-rules))))))
      (map (lambda (instance)
             (fluid-let ((*assertions* (copy-list state)))
               (fire-rule-instance instance) ; fire each instance
               ;; return the list of assertions as a state
               (get-assertions)))
           instances))))

;; get-heuristic-value
;;
;; Given a current state and a goal state, returns an integer
;; estimating the distance between them
;;
;; Currently returns the difference between the two states

(define (get-heuristic-value node finish)
  (define (loop assertions score)
    (cond ((null? assertions) score)
          ((member (car assertions) node)
           (loop (cdr assertions) (- score 1)))
          (else
           (loop (cdr assertions) score))))
  (loop finish 0)
  0)

;; done?
;;
;; returns true if the current state contains every state in the goal
;; otherwise returns false

(define (done? current goal)                ; Termination test
  ;; Every assertion in the goal is present in the current state.
  (for-all? goal (lambda (x) (member x current))))

;; copy-list
;;
;; Utility needed because append copies its first argument.

(define (copy-list x) (append x '()))

