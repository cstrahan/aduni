;;; SICP PS-3
;;; Shyam Visweswaran

;;; Scheme code for Twenty-One Simulator 

(define (twenty-one player-strategy house-strategy)
  (let ((house-initial-hand (make-new-hand (deal))))
    (let ((player-hand
           (play-hand player-strategy
                      (make-new-hand (deal))
                      (hand-up-card house-initial-hand))))
      (if (> (hand-total player-hand) 21)
          0                                ; ``bust'': player loses
          (let ((house-hand 
                 (play-hand house-strategy
                            house-initial-hand
                            (hand-up-card player-hand))))
            (cond ((> (hand-total house-hand) 21)
                   1)                      ; ``bust'': house loses
                  ((> (hand-total player-hand)
                      (hand-total house-hand))
                   1)                      ; house loses
                  (else 0)))))))           ; player loses

(define (play-hand strategy my-hand opponent-up-card)
  (cond ((> (hand-total my-hand) 21) my-hand) ; I lose... give up
        ((strategy my-hand opponent-up-card) ; hit?
         (play-hand strategy
                    (hand-add-card my-hand (deal))
                    opponent-up-card))
        (else my-hand)))                ; stay


(define (deal) (+ 1 (random 10)))

(define (make-new-hand first-card)
  (make-hand first-card first-card))

(define (make-hand up-card total)
  (cons up-card total))

(define (hand-up-card hand)
  (car hand))

(define (hand-total hand)
  (cdr hand))

(define (hand-add-card hand new-card)
  (make-hand (hand-up-card hand)
             (+ new-card (hand-total hand))))

(define (hit? your-hand opponent-up-card)
  (newline)
  (display "Opponent up card ")
  (display opponent-up-card)
  (newline)
  (display "Your Total: ")
  (display (hand-total your-hand))
  (newline)
  (user-wants-hit?))

(define (user-wants-hit?) 
  (let ((x (prompt-for-command-char "Hit? ")))
    (display x)
    (eq? x '#\y))) ;prompt-for-command char returns #\ before char


;;;========================================================
;;; Exercise 1
;;;========================================================

;; Box and pointer diagram for (make hand 10 15)












;;;========================================================
;;; Exercise 2
;;;========================================================

;; Evaluating (twenty-one hit? hit?)
;; no code to turn in


;;;========================================================
;;; Exercise 3
;;;========================================================

;; procedure stop-at takes a number as argument and returns
;; #t or #f to the twenty-one procedure

(define (stop-at x)
  (lambda (your-hand opponent-up-card)
    (< (hand-total your-hand))))

;; Test of (stop-at 16) as house strategy

;(twenty-one hit? (stop-at 16))

;Opponent up card 8
;Your Total: 7

;Hit? y
;Opponent up card 8
;Your Total: 13

;Hit? y
;Opponent up card 8
;Your Total: 21

;Hit? n
;Opponent up card 7
;Your Total: 8

;Hit? y
;Value: 1


;;;===================================================
;;; Exercise 4
;;;===================================================

;; test-strategy proc
;; n is the number of games to be played
;; count is the number of games played so far
;; wins is the nummber of games won by player

(define (test-strategy player-strategy house-strategy n)
  (define (helper count wins)
    (if (> count n)
	wins
	(if (= (twenty-one player-strategy house-strategy) 1)
	    (helper (+ 1 count) (+ 1 wins))
	    (helper (+ 1 count) wins))))
  (helper 1 0))


;; Test with both player and house using stop-at (5 games)

;(test-strategy (stop-at 16) (stop-at 15) 100)
;Value: 38
;Value: 48
;Value: 49
;Value: 46
;Value: 43


;;;============================================================
;;; Exercise 5
;;;============================================================

;; watch-player proc - returns a proc that displays HIT or SHOW
;; and returns #t or #f to twenty-one

(define (watch-player strategy)
  (lambda (your-hand opponent-up-card)
      (newline)
      (display "Player Total: ")
      (display (hand-total your-hand))
      (display " * ")
      (display "Opponent Up-card: ")
      (display opponent-up-card)
      (cond ((strategy your-hand opponent-up-card)
	     (display " --> HIT") #t)
	    (else
	     (display " --> SHOW") #f))))

;; testing for 2 games

;(test-strategy (watch-player (stop-at 16))
	       (watch-player (stop-at 15))
	       2)

;Player Total: 8 * Opponent Up-card: 5 --> HIT
;Player Total: 9 * Opponent Up-card: 5 --> HIT
;Player Total: 13 * Opponent Up-card: 5 --> HIT
;Player Total: 17 * Opponent Up-card: 5 --> SHOW
;Player Total: 5 * Opponent Up-card: 8 --> HIT
;Player Total: 12 * Opponent Up-card: 8 --> HIT
;Player Total: 17 * Opponent Up-card: 8 --> SHOW
;Player Total: 3 * Opponent Up-card: 1 --> HIT
;Player Total: 11 * Opponent Up-card: 1 --> HIT
;Player Total: 20 * Opponent Up-card: 1 --> SHOW
;Player Total: 1 * Opponent Up-card: 3 --> HIT
;Player Total: 10 * Opponent Up-card: 3 --> HIT
;Player Total: 12 * Opponent Up-card: 3 --> HIT
;Player Total: 18 * Opponent Up-card: 3 --> SHOW
;Value: 1


;;;===========================================================
;;; Exercise 6
;;;===========================================================

;; Louis strategy proc

(define louis
  (lambda (your-hand opponent-up-card)
    (cond ((< (hand-total your-hand) 12) #t)
	  ((> (hand-total your-hand) 16) #f)
	  ((and (= (hand-total your-hand) 12) (< opponent-up-card 4)) #t)
	  ((and (= (hand-total your-hand) 16) (= opponent-up-card 10)) #t)
	  (else (if (> opponent-up-card 6) #t #f)))))


;; testing louis procedure against house strategy

;(test-strategy louis (stop-at 15) 100) - simulated 5 games
;Value: 35
;Value: 29
;Value: 36
;Value: 40
;Value: 28

;(test-strategy louis (stop-at 16) 100) - simulated 5 games
;Value: 33
;Value: 33
;Value: 44
;Value: 43
;Value: 43

;(test-strategy louis (stop-at 17) 100) - simulated 5 games
;Value: 43
;Value: 39
;Value: 32
;Value: 36
;Value: 32


;;;==========================================================
;;; Exercise 7
;;;==========================================================

;; procedure both

(define (both strategy1 strategy2)
  (lambda (your-hand opponent-up-card)
    (if (and (strategy1 your-hand opponent-up-card)
	     (strategy2 your-hand opponent-up-card))
	#t
	#f)))

;; testing proc both

;(both (stop-at 19) hit?)
;(twenty-one both (stop-at 16))
;Value: 0

;;;=========================================================
;;; Exercise 8
;;;=========================================================

;; rewrite constructor proc make-new-hand with 3 elements.
;; 1st - first card
;; 2nd - also first card
;; 3rd - a list containing the first card

(define (make-new-hand first-card)
  (make-hand first-card first-card (list first-card)))


;; rewrite contructor proc make-hand with 3 elements.
;; 1st - first card
;; 2nd - total of the hand
;; 3rd - list containing all the cards in the hand
;; cons total to card-list and then cons up-card to the result

(define (make-hand up-card total card-list)
  (cons up-card (cons total card-list)))


;;;==============================================================
;;; Exercise 9
;;;==============================================================

;; rewrite hand-add-card so that it updates 3 elements.
;; 1st - first card: leave it unchanged
;; 2nd - update total of the hand
;; 3rd - add the newest card to the start of the card-list

(define (hand-add-card hand new-card)
  (make-hand (hand-up-card hand)
	     (+ new-card (hand-total hand))
	     (cons new-card (list-of-cards hand))))


;;;==============================================================
;;; Exercise 10
;;;==============================================================

;; box and pointer diagram of (define my-hand (make-new-hand 5))
















;;;============================================================
;;; Exercise 11
;;;============================================================

;; box and pointer diagram of
;; (define my-hand-after-new-card (hand-add-card my-hand 10))
















;;;================================================================
;;; Exercise 12
;;;================================================================

;; new selectors hand-up-card, hand-total, list-of-cards
;; new hand-up-card: same proc as before, no change needed

(define (hand-up-card hand)
  (car hand))

;; new hand-total: get the 2nd element from hand

(define (hand-total hand)
  (cadr hand))

;; list-of-cards is a new selector procedure to retrieve the 3rd element from
;; hand, which is a list

(define (list-of-cards hand)
  (cddr hand))


;;;===========================================================================
;;; exercise 13
;;;===========================================================================

;; test (twenty-one hit? hit?) with the new constructors and selectors.
;; No changes were made to twenty-one because of data abstraction. It does
;; not matter to twenty-one how the constructors and selectors are implemented.

;(twenty-one hit? hit?)

;Opponent up card 6
;Your Total: 10

;Hit? y
;Opponent up card 6
;Your Total: 14

;Hit? y
;Opponent up card 6
;Your Total: 15

;Hit? n
;Opponent up card 10
;Your Total: 6

;Hit? y
;Opponent up card 10
;Your Total: 10

;Hit? y
;Opponent up card 10
;Your Total: 17

;Hit? y
;Opponent up card 10
;Your Total: 19

;Hit? y
;Value: 1


;;;=================================================================
;;; Exercise 14
;;;=================================================================

;; rewrite hit? to display the list of cards in the hand everytime

(define (hit? your-hand opponent-up-card)
  (newline)
  (display "Opponent up card ")
  (display opponent-up-card)
  (newline)
  (display "Your total (and your hand): ")
  (display (hand-total your-hand))
  (display " ")
  (display (list-of-cards your-hand))
  (newline)
  (user-wants-hit?))


;; test twenty-one with the new hit?
;; note that the hand is shows the cards in the reverse order
;; they were drawn

;(twenty-one hit? hit?)

;Opponent up card 5
;Your total (and your hand): 2 (2)

;Hit? y
;Opponent up card 5
;Your total (and your hand): 8 (6 2)

;Hit? y
;Opponent up card 5
;Your total (and your hand): 15 (7 6 2)

;Hit? n
;Opponent up card 2
;Your total (and your hand): 5 (5)

;Hit? y
;Opponent up card 2
;Your total (and your hand): 11 (6 5)

;Hit? y
;Opponent up card 2
;Your total (and your hand): 13 (2 6 5)

;Hit? y
;Value: 1


;;;=======================================================================




