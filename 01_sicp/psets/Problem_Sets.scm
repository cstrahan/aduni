;;; -*-Scheme-*-

;;; The structure "problem-sets" must be loaded from a file whenever
;;; the set of available problem sets changes, or when the default
;;; problem set changes.  Files should appear with name and extension, but
;;; without device, directory, or version; these will be supplied
;;; automatically.
;;;
;;; The first number is used as the "default" problem set

(define problem-sets
  '(10
    (1
     (load&reference "ps1.scm"))
    (3
     (load&reference "ps3.scm"))
    (4
     (load "psgo.scm")
     (load&reference "hutils.scm" "hend.scm"))
    (5
     (load&reference "ps5match.scm" "ps5adv.scm"))
    (6
     (load&reference "put-get.scm" "types.scm" "generic.scm"))
    (7
     (load&reference "game.scm" "world.scm"))
    (8
     (load&reference "ps8code.scm"))
    (9
     (load&reference "mc-eval.scm"))
    (10
     (load&reference "mc-eval-ps10.scm" "analyze.scm"))
   )
)


