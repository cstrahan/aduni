;Problem Set 3 Solutions
;  by Dimitri Kountourogiannis

; ==========
; Exercise 1
; ==========

; Draw the box and pointer diagram resulting from the evaluation of
;

(make-hand 10 15)

;   |
;   V
; ------------- 
; |     |     |
; ---|-----|--- 
;    |     | 
;    V     V 
;   10    15



; ==========
; Exercise 2
; ==========

;Nothing to turn in


; ==========
; Exercise 3
; ==========

; STOP-AT Asks for another card if the hand total 
; is too low. Notice we don't need to use
; an (if (< ...  ) #t #f)

(define (stop-at n)
  (lambda (your-hand opponent-up-card)
    (< (hand-total your-hand) n)))       

; ==========
; Exercise 4
; ==========

; TEST-STRATEGY plays n games with the given strategies and returns 
; the number of games won.
 
(define (test-strategy player-strategy house-strategy n)
  (define (iter i games-won)
    (if  (> i n) games-won
	 (iter (inc i)
	       (+ (twenty-one player-strategy house-strategy)
		  games-won))))
  (iter 1 0))

; ==========
; Exercise 5
; ==========

; WATCH-PLAYER takes a strategy and makes it verbose.
; The modified strategy returns the same values, but
; also prints out the player's hand total and the opponent's up-card.
; \t is the tab character.
 
(define (watch-player strategy)
  (lambda (your-hand opponent-up-card)
    (let ((a (strategy your-hand opponent-up-card)))
      (display     "  Player's Hand: ") 
      (display     " \t Hand Total:  ")
      (display (hand-total your-hand))
      (display     " \t Opponent's Up Card:  ")
      (display opponent-up-card)
      (newline)
      (if a
	  (display "  Player's Move: \t Hit Me " )
          (display "  Player's Move: \t Hold " ))
      (newline)
      (newline)
      a)))


; ==========
; Exercise 6
; ==========
 
;LOUIS is a convoluted strategy only a COND could love. 

 
(define (louis your-hand opponent-up-card)
  (cond ( (< (hand-total your-hand) 12) #t)
	( (> (hand-total your-hand) 16) #f)
	( (and (= (hand-total your-hand) 12) (< opponent-up-card 4)) #t)
	( (and (= (hand-total your-hand) 16) (= opponent-up-card 10)) #f)
	(else (> opponent-up-card 6))))

; ==========
; Exercise 7
; ==========
 
; BOTH returns the strategy that asks for another card only of both
; of the inpput strategies would. 

(define (both strategy1 strategy2)
  (lambda (hand opponent-up-card)
    (and (strategy1 hand opponent-up-card)
	 (strategy2 hand opponent-up-card))))

; ==========
; Exercise 8
; ==========

; MAKE-HAND and MAKE-NEW_HAND are our constructors.
 
(define (make-new-hand first-card)
  (make-hand first-card first-card (list first-card)))
 
(define (make-hand up-card total card-list)
  (list up-card total card-list))
 
; ==========
; Exercise 9
; ==========
 
; HAND-ADD-CARD adds a card to the hand and adjusts
; the hand-total accordingly. The cons does not violate
; the data abstraction barrier because we are assuming
; make-hand takes a list of numbers card-list for input.

(define (hand-add-card hand new-card)
  (make-hand (hand-up-card hand)
             (+ new-card (hand-total hand))
             (cons new-card (list-of-cards hand))))
 
 
; ===========
; Exercise 10
; ===========

(define my-hand (make-new-hand 5))

; my-hand 
;   |
;   V
; ---------     ---------    ---------
; |   |   +---> |   |   +--->|   | / |
; --|------     --|------    --|------
;   |             |            |     
;   V             V            V
;   5             5          ---------
;                            |   | / |
;                            --|------
;                              |      
;                              V      
;                              5      
; 
 
 
 
; ===========
; Exercise 11
; ===========

(define my-hand-after-new-card (hand-add-card my-hand 10))
 
; my-hand-after-new-card 
;   |
;   V
; ---------     ---------    ---------
; |   |   +---> |   |   +--->|   | / |
; --|------     --|------    --|------
;   |             |            |     
;   V             V            V
;   5            15          ---------    --------- 
;                            |   |   +--->|   | / | 
;                            --|------    --|------ 
;                              |            |     
;                              V            V
;                             10            5 
; 
 
 
 
; ===========
; Exercise 12
; ===========
 
; Selectors for hand. 

(define (hand-up-card hand)
  (car hand))
 
(define (hand-total hand)
  (cadr hand))
 
(define (list-of-cards hand)
  (caddr hand))


; ===========
; Exercise 13
; ===========
 
 
; We do not need to change twenty-one to reflect the
; new constructors because the implementation of the hand
; is hidden from twenty-one.


; ===========
; Exercise 15
; ===========


; HIT? is modified here to display the player's list of cards
; in addition to the hand total. It is in reverse order

(define (hit? your-hand opponent-up-card)
  (newline)
  (display "Your Total: ")
  (display (hand-total your-hand))
  (display "\t Opponent up card: ")
  (display opponent-up-card)
  (display "\t Your Cards: ")
  (display (list-of-cards your-hand))
(user-wants-hit?))


















