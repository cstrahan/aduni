;;; Adapted from ...
;;;
;;;           Structure and Interpretation of Computer Programs
;;;                            Second Edition 
;;;                          Sample Problem Set 
;;;
;;;                         Code file WORLD.SCM
;;;
;;; ... by Pezaris and Yanco, ADU SICP October 2000.  Localized from
;;; the MIT campus to the ADU building.  Changes here were
;;; significant.  Original code taken from the MIT Press web site,
;;; supplied as part of the supporting materials for the SICP text.


;;;============================================================================
;;; You can extend this file to make more stuff part of your world.
;;;============================================================================

;;;============================================================================
;;; *CAVEAT* To keep your world consistent, whenever you change a procedure or 
;;;          redefine a person/place/etc you should reload this entire file    
;;;          into Scheme. This prevents you from having old-moldy folks running
;;;          around who have not evolved to adhere to your modifications. To   
;;;          make this work out well, you should create little scripts at the  
;;;          end of this file to make the game evolve as you work through it.  
;;;          [See the bottom of this file for an example.]                     
;;;============================================================================


(initialize-clock-list)

;; Here we define the places in our world...
;;------------------------------------------

(define student-lab          (make-place 'student-lab))
(define foundation-office    (make-place 'foundation-office))
(define faculty-office       (make-place 'faculty-office))
(define network-closet       (make-place 'network-closet))
(define bike-storage-hallway (make-place 'bike-storage-hallway))
(define lower-stairwell      (make-place 'lower-stairwell))
(define upper-stairwell      (make-place 'upper-stairwell))
(define the-big-room         (make-place 'the-big-room))
(define room-with-elevator   (make-place 'room-with-elevator))
(define elevator             (make-place 'elevator))
(define supply-closet        (make-place 'supply-closet))
(define classroom            (make-place 'classroom))
(define dungeon              (make-place 'dungeon))

;; The following isolated place is defined in GAME.SCM too but redefined
;; here so you can just "zap" altered definitions there then re-evaluate this
;; file w/o worrying about forgetting to update any places.
;;
;; Consequently, to be consistent, if you find it appropriate to define any new
;; places in GAME.SCM, you should likewise duplicate their definitions here.

(define heaven             (make-place 'heaven))        ; The point of no return


;; One-way paths connect individual places in the world.
;;------------------------------------------------------

(define (can-go from direction to)
  (ask from 'add-neighbor direction to))

(define (can-go-both-ways from direction reverse-direction to)
  (can-go from direction to)
  (can-go to reverse-direction from))

(can-go-both-ways student-lab          'east     'west     foundation-office)
(can-go-both-ways student-lab          'north    'south    network-closet)
(can-go-both-ways foundation-office    'east     'west     faculty-office)
(can-go-both-ways bike-storage-hallway 'north    'south    foundation-office)
(can-go-both-ways lower-stairwell      'east     'west     bike-storage-hallway)
(can-go-both-ways upper-stairwell      'down     'up       lower-stairwell)
(can-go-both-ways the-big-room         'out      'in       upper-stairwell)
(can-go-both-ways room-with-elevator   'north    'south    bike-storage-hallway)
(can-go-both-ways elevator             'out      'elevator room-with-elevator)
(can-go-both-ways supply-closet        'out      'west     room-with-elevator)
(can-go-both-ways classroom            'north    'south    room-with-elevator)

(can-go dungeon        'up        supply-closet)
(can-go network-closet 'secretly  supply-closet)

;; The important critters in our world...
;;---------------------------------------

(define holly   (make&install-person 'holly   classroom      3))
(define john    (make&install-person 'john    faculty-office 2))
(define grendel (make&install-troll  'grendel dungeon        4))

;; And some things for the world...
;;---------------------------------

(define computer-manual
  (make&install-thing 'computer-manual student-lab))
(define chalk
  (make&install-thing 'chalk           classroom))
(define red-bike
  (make&install-thing 'red-bike        bike-storage-hallway))

(define holly-card
  (make&install-id-card 'holly-card faculty-office '2000-32))
(define john-card
  (make&install-id-card 'john-card  faculty-office '2000-33))

;; The beginning of an ever-expanding game script
;;------------------------------------------------

(define (play-game)
  (display (ask holly 'go 'north))
  (display (ask holly 'go 'west))
  (display (ask grendel 'move))
  (display (ask grendel 'move))
  true)

;; ...now whenever you re-load this file, you can bring things up to
;; date by invoking PLAY-GAME.

