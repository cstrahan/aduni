;;;PS-1
;;;Shyam Visweswaran

;Exercise 1
;fold is calling spindle with 2 arguments, but spindle requires exactly only
;one argument. fold could be defined as follows:
(define fold
(lambda (x y)
  (* (spindle x) (+ (mutilate y) (spindle x))))

;Exercise 2
(define fact
  (lambda (n)
    (if (= n 0)		
        1
	(* n (fact (- n 1))))))

(fact 243)
;value: 576510720734055648599325993789888243895446127697487852895785
14753791226660795447787952561780489668440613028916503471522241703645
76799681069513522627829674263760611513430078705299131943141237931254
02307920602501370887088117944245648331070851734647189855089998587919
70609491066045711874321516918150905413944789377156315207186998055591
45167063389871456774538682693667884054822564808996172787570544453816
7142818292862812160000000000000000000000000000000000000000000000000
000000000

;Exercise 3
(define comb
  (lambda (n k)
    (/ (fact n)
       (* (fact k)
	  (fact (- n k))))))

(comb 243 90)
;value: 193404342391239489855973693417880600543891038618846567058
277413638164

;Exercise 4
Copyright (c) 1985 Free Software Foundation

   Permission is granted to anyone to make or distribute verbatim copies
   of this document as received, in any medium, provided that the
   copyright notice and permission notice are preserved,
   and that the distributor grants the recipient permission
   for further redistribution as permitted by this notice.

   Permission is granted to distribute modified versions
   of this document, or of portions of it,
   under the above conditions, provided also that they
   carry prominent notices stating who last altered them.

The conditions for copying Emacs itself are slightly different
but in the same spirit.  Please read the file COPYING and then
do give copies of GNU Emacs to your friends.
Help stamp out software obstructionism ("ownership") by using,
writing, and sharing free software!

;Exercise 5
Cheese Pizza
12" Cheese				       5.70

;Exercise 6
File: jargon.info  Node: UNIX conspiracy, Prev: UNIX brain damage, Up: = U =, Next: UNIX weenie

:UNIX conspiracy: [ITS] n. According to a conspiracy theory long
   popular among {{ITS}} and {{TOPS-20}} fans, UNIX's growth is the
   result of a plot, hatched during the 1970s at Bell Labs, whose
   intent was to hobble AT&T's competitors by making them dependent
   upon a system whose future evolution was to be under AT&T's
   control.  This would be accomplished by disseminating an operating
   system that is apparently inexpensive and easily portable, but also
   relatively unreliable and insecure (so as to require continuing
   upgrades from AT&T).  This theory was lent a substantial impetus
   in 1984 by the paper referenced in the {back door} entry.

   In this view, UNIX was designed to be one of the first computer
   viruses (see {virus}) --- but a virus spread to computers indirectly
   by people and market forces, rather than directly through disks and
   networks.  Adherents of this `UNIX virus' theory like to cite the
   fact that the well-known quotation "UNIX is snake oil" was
   uttered by DEC president Kenneth Olsen shortly before DEC began
   actively promoting its own family of UNIX workstations.  (Olsen now
   claims to have been misquoted.)

File: jargon.info  Node: phase of the moon, Prev: phase, Up: = P =, Next: phase-wrapping

:phase of the moon: n. Used humorously as a random parameter on which
   something is said to depend.  Sometimes implies unreliability of
   whatever is dependent, or that reliability seems to be dependent on
   conditions nobody has been able to determine.  "This feature
   depends on having the channel open in mumble mode, having the foo
   switch set, and on the phase of the moon."

   True story: Once upon a time there was a bug that really did depend
   on the phase of the moon.  There is a little subroutine that had
   traditionally been used in various programs at MIT to calculate an
   approximation to the moon's true phase.  GLS incorporated this
   routine into a LISP program that, when it wrote out a file, would
   print a timestamp line almost 80 characters long.  Very
   occasionally the first line of the message would be too long and
   would overflow onto the next line, and when the file was later read
   back in the program would {barf}.  The length of the first line
   depended on both the precise date and time and the length of the
   phase specification when the timestamp was printed, and so the bug
   literally depended on the phase of the moon!

   The first paper edition of the Jargon File (Steele-1983) included
   an example of one of the timestamp lines that exhibited this bug,
   but the typesetter `corrected' it.  This has since been
   described as the phase-of-the-moon-bug bug.

;Exercise 7
     the-word-recursion-has-many-meanings

;Exercise 8
; C-M q, the keys for the command indent-sexp, indents the whole
; expression. Found it by going into Edwin help A and searching
; for strings lisp, scheme, indent. Then looked up the key bindings
; for indent-sexp in Edwin help B.

;Exercise 9
[sviswesw@localhost work]$ ls
#*Scheme*#   ps1-ans.scm.~1~  ps1-ans.scm.~3~  ps1.scm.~1~
ps1-ans.scm  ps1-ans.scm.~2~  ps1.scm

[sviswesw@localhost work]$ date
Mon Oct  2 00:44:17 EDT 2000

[sviswesw@localhost work]$ echo "Hello world"
Hello world

;Exercise 10
(define cube
  (lambda (x)
    (* x x x)))

;Exercise 11
(define sum-of-cubes
  (lambda (a b)
    (+ (cube a) (cube b))))

;Exercise 12
(define biggest-of-three
  (lambda (x y z)
    (cond ((> x y) (if (> x z) x z))
	  (else (if (> y z) y z)))))

;Exercise 13
(define cube-of-largest
  (lambda (x y z)
    (cube (biggest-of-three x y z))))

;Exercise 14
(define f
  (lambda (x a0 a1 a2 a3)
    (+ a0 (* a1 x) (* a2 (square x)) (* a3 (cube x)))))

(f 0.5 1 2 3 4)
;Value: 3.25

;For a more efficient way, each power of x can be evaluated by
;multiplying x with the previous evaluation for the (n-1) power
;of x.