;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-project) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; TODO:
;;  [x] Create data definition for ListOfInvader
;;  [x] Create data definition for ListOfMissile
;;  [x] Finish render invaders function
;;  [x] Finish render missils function
;;  [x] Finish render tank function
;;  [x] Finish advance-game function
;;  [x] Finish advance-missiles function
;;  [x] Finish add-missile function
;;  [x] Finish fire-missile function
;;  [x] Finish prune-missiles function
;;  [x] Finish move-missiles function
;;  [x] Finish advance-invaders function
;;  [x] Finish move-invaders function
;;  [x] Finish move-invader function
;;  [x] Finish add-invader function
;;  [x] Finish advance-tank function
;;  [x] Finish game-over? function
;;  [x] Finish ship-landed? function
;;  [x] Finish handle-key function
;;  [x] Finish prune-collisions function
;;  [x] Finish prune-invader-collisions function
;;  [x] Finish prune-missile-collisions function
;;  [x] finish implementing all other functions

;; FINISHED 2022-12-21


(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))



;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game g)
  (... (fn-for-loinvader (game-invaders g))
       (fn-for-lom (game-missiles g))
       (fn-for-tank (game-tank g))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx dy))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader is moving along x by dx pixels per clock tick
;;         the invader is moving along y by dy pixels per clock tick. Should ALWAYS be positive (invaders never move toward the top of the screen)

(define I1 (make-invader 150 100 12 12))           ;not landed, moving both right and down at 12 px/tick
(define I2 (make-invader 150 HEIGHT -10 10))       ;exactly landed, moving both left and down at 10 px/tick
(define I3 (make-invader 150 (+ HEIGHT 10) 10 10)) ;> landed, moving both right and down at 10 px/tick


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader) (invader-dy invader)))



;; ListOfInvader is one of:
;;  - empty
;;  - (cons Invader ListOfInvader)
;; interp. a list of Invaders

(define LOI1 empty)
(define LOI2 (cons I1 empty))
(define LOI3 (cons I2 (cons (make-invader 50 23 10 10) empty)))

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (fn-for-invader (first loi))
              (fn-fo-loi (rest loi)))]))
;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Invader ListOfInvader)
;;  - reference: (first loi) is Invader
;;  - self-reference: (rest loi) is ListOfInvader



(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))



;; ListOfMissile is one of:
;;  - empty
;;  - (cons Missile ListOfMissile)
;; interp. a list of Missiles

(define LOM1 empty)
(define LOM2 (cons M1 empty))
(define LOM3 (cons M2 (cons (make-missile 40 50) empty)))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom (rest lom)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound (cons Missile ListOfMissile)
;;  - reference: (first lom) is Missile
;;  - self-reference (rest lom) is ListOfMissile
  


(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))



;; Functions:

;; Game -> Game
;; start the world with (main G0)
;; 
(define (main g)
  (big-bang g                        ; Game
    (on-tick   advance-game) ; Game -> Game
    (to-draw   render-game)  ; Game -> Image
    (stop-when game-over?) ; Game -> Boolean
    (on-key    handle-key))) ; Game KeyEvent -> Game


;; Game -> Game
;; advances the entire game state every tick
(check-expect (advance-game G0)
              (prune-collisions (make-game (advance-invaders (game-invaders G0))
                                           (advance-missiles (game-missiles G0))
                                           (advance-tank (game-tank G0)))))

;(define (advance-game g) G0) ;stub
;<using template from Game data definition>

(define (advance-game g)
  (prune-collisions (make-game (advance-invaders (game-invaders g))
                               (advance-missiles (game-missiles g))
                               (advance-tank (game-tank g)))))


;; Game -> Game
;; removes all invaders and missiles that have collided from the game state
(check-expect (prune-collisions G0)
              (make-game (prune-invader-collisions (game-invaders G0) (game-missiles G0))
                         (prune-missile-collisions (game-missiles G0) (game-invaders G0))
                         (game-tank G0)))
(check-expect (prune-collisions (make-game (list I2 I3) (list M1 M2 M3) T0))
              (make-game (prune-invader-collisions (list I2 I3) (list M1 M2 M3))
                         (prune-missile-collisions (list M1 M2 M3) (list I2 I3))
                         T0))
(check-expect (prune-collisions (make-game (list I1 I2 I3) (list M1 M3) T1))
              (make-game (prune-invader-collisions (list I1 I2 I3) (list M1 M3))
                         (prune-missile-collisions (list M1 M3) (list I1 I2 I3))
                         T1))


;(define (prune-collisions g) G0) ;stub

(define (prune-collisions g)
  (make-game (prune-invader-collisions (game-invaders g) (game-missiles g)) (prune-missile-collisions (game-missiles g) (game-invaders g)) (game-tank g)))


;; ListOfInvader ListOfMissile -> ListOfInvader
;; Removes any invaders from loi if they have collided with any missiles in lom
(check-expect (prune-invader-collisions empty LOM3) empty)
(check-expect (prune-invader-collisions LOI3 empty) LOI3)
(check-expect (prune-invader-collisions (list (make-invader 150 100 10 10)
                                              (make-invader 200 75 10 10)
                                              (make-invader 175 60 10 10))
                                        (list (make-missile 161 100) ; missed invader 1 by 1px
                                              (make-missile 139 100) ; missed again by 1px
                                              (make-missile 150 89) ; missed by 1px in y dir
                                              (make-missile 150 111))) ; missed by 1px in y dir
              (list (make-invader 150 100 10 10)
                    (make-invader 200 75 10 10)
                    (make-invader 175 60 10 10)))
(check-expect (prune-invader-collisions (list (make-invader 150 100 10 10)
                                              (make-invader 200 75 10 10)
                                              (make-invader 175 60 10 10))
                                        (list (make-missile 161 100) ; missed invader 1 by 1px
                                              (make-missile 139 100) ; missed again by 1px
                                              (make-missile 201 78) ; HIT invader 2
                                              (make-missile 150 111))) ; missed invader 1 by 1px in y dir
              (list (make-invader 150 100 10 10) ; invader 2 removed
                    (make-invader 175 60 10 10)))
              
;(define (prune-invader-collisions loi lom) LOI1) ;stub
; <using template from ListOfInvader data definition>

;; for each item in loi, check each item in lom and see if there is a collision. If yes, don't include that invader in output list
(define (prune-invader-collisions loi lom)
  (cond [(empty? loi) empty]
        [else
         (if (invader-collision? (first loi) lom)
             (prune-invader-collisions (rest loi) lom) ; skip over current invader
             (cons (first loi) (prune-invader-collisions (rest loi) lom)))]))


;; Invader ListOfMissile -> Boolean
;; produces true if the given invader has been involved in a collision with any missile in the given list.
(check-expect (invader-collision? (make-invader 100 75 10 10) empty) false)
(check-expect (invader-collision? (make-invader 50 100 10 10) (list (make-missile 61 100) (make-missile 50 89))) false)
(check-expect (invader-collision? (make-invader 75 85 10 10) (list (make-missile 100 100) (make-missile 85 85))) true)
(check-expect (invader-collision? (make-invader 215 150 10 10) (list (make-missile 200 100) (make-missile 218 140))) true)

;(define (invader-collision? i lom) false) ;stub
;<using template from ListOfMissile data definition>

(define (invader-collision? i lom)
  (cond [(empty? lom) false]
        [else
         (if (and (<= (abs (- (missile-x (first lom)) (invader-x i))) HIT-RANGE)
                  (<= (abs (- (missile-y (first lom)) (invader-y i))) HIT-RANGE))
             true
             (invader-collision? i (rest lom)))]))


;; ListOfMissile ListOfInvader -> ListOfMissile
;; Removes any missiles from lom if they have collided with any invaders in loi
(check-expect (prune-missile-collisions empty LOI3) empty)
(check-expect (prune-missile-collisions LOM3 empty) LOM3)
(check-expect (prune-missile-collisions (list (make-missile 161 100) ; missed invader 1 by 1px
                                              (make-missile 139 100) ; missed again by 1px
                                              (make-missile 150 89) ; missed by 1px in y dir
                                              (make-missile 150 111)) ; missed by 1px in y dir
                                        (list (make-invader 150 100 10 10)
                                              (make-invader 200 75 10 10)
                                              (make-invader 175 60 10 10)))
              (list (make-missile 161 100) 
                    (make-missile 139 100) 
                    (make-missile 150 89) 
                    (make-missile 150 111)))
(check-expect (prune-missile-collisions (list (make-missile 161 100) ; missed invader 1 by 1px
                                              (make-missile 139 100) ; missed again by 1px
                                              (make-missile 201 78) ; HIT invader 2
                                              (make-missile 150 111)) ; missed invader 1 by 1px in y dir
                                        (list (make-invader 150 100 10 10)
                                              (make-invader 200 75 10 10)
                                              (make-invader 175 60 10 10))) 
              (list (make-missile 161 100) ; missile 3 removed
                    (make-missile 139 100)
                    (make-missile 150 111)))
              
;(define (prune-missile-collisions lom loi) LOM1) ;stub
;<using template from ListOfMissile data definition>

(define (prune-missile-collisions lom loi)
  (cond [(empty? lom) empty]
        [else
         (if (missile-collision? (first lom) loi)
             (prune-missile-collisions (rest lom) loi) ;skip over this missile
             (cons (first lom) (prune-missile-collisions (rest lom) loi)))]))


;; Missile ListOfInvader
;; produces true if the given missile has collided with any invader in the list
(check-expect (missile-collision? (make-missile 50 50) empty) false)
(check-expect (missile-collision? (make-missile 100 200) (list (make-invader 100 211 12 12) (make-invader 89 200 12 12))) false)
(check-expect (missile-collision? (make-missile 75 150) (list (make-invader 200 50 10 10) (make-invader 85 155 10 10) (make-invader 100 100 10 10))) true)

;(define (missile-collision? m loi) false) ;stub
;<using template from ListOfInvader data definition>

(define (missile-collision? m loi)
  (cond [(empty? loi) false]
        [else
         (if (and (<= (abs (- (invader-x (first loi)) (missile-x m))) HIT-RANGE)
                  (<= (abs (- (invader-y (first loi)) (missile-y m))) HIT-RANGE))
             true
             (missile-collision? m (rest loi)))]))


;; ListOfInvaders -> ListOfInvaders
;; Determines whether to add another Invader to the game and advances all existing invaders
(check-random (advance-invaders empty) (add-invader (move-invaders empty)))
(check-random (advance-invaders LOI3) (add-invader (move-invaders LOI3)))
(check-random (advance-invaders LOI2) (add-invader (move-invaders LOI2)))

;(define (advance-invaders loi) empty) ;stub

(define (advance-invaders loi)
  (add-invader (move-invaders loi)))


;; ListOfInvaders -> ListOfInvaders
;; advances x and y positions of all invaders in list by according to their invader-dx and invader-dy properties
(check-expect (move-invaders empty) empty)
(check-expect (move-invaders LOI2) (cons (move-invader I1) empty))
(check-expect (move-invaders LOI3) (cons (move-invader I2) (cons (move-invader (make-invader 50 23 10 10)) empty)))

;(define (move-invaders loi) empty) ;stub
;<using template from ListOfInvaders data definition>

(define (move-invaders loi)
  (cond [(empty? loi) empty]
        [else
         (cons (move-invader (first loi))
               (move-invaders (rest loi)))]))


;; Invader -> Invader
;; Advances x and y position of a single invader by its invader-dx and invader-dy properties.
;; Switches x direction if it is bouncing off wall
(check-expect (move-invader I1) (make-invader (+ 150 12) (+ 100 12) 12 12))
(check-expect (move-invader I2) (make-invader (+ 150 -10) (+ HEIGHT 10) -10 10))
(check-expect (move-invader I3) (make-invader (+ 150 10) (+ (+ HEIGHT 10) 10) 10 10))
(check-expect (move-invader (make-invader 0 150 -10 10)) ; at left edge, should bounce off wall (change direction)
              (make-invader 0 160 10 10))
(check-expect (move-invader (make-invader WIDTH 50 10 10)) ; at right edge, should bounce off wall (change direction)
              (make-invader WIDTH 60 -10 10))
(check-expect (move-invader (make-invader (- WIDTH 5) 50 10 10)) ; not quite right edge, should bounce off wall (change direction) and will reset position to right edge
              (make-invader WIDTH 60 -10 10))
(check-expect (move-invader (make-invader 5 150 -10 10)) ; not quite left edge, should bounce off wall (change direction) and will reset position to left edge
              (make-invader 0 160 10 10))

;(define (move-invader i) I1) ;stub
;<using template from Invader data definition>

(define (move-invader i)
  (cond [(<= (+ (invader-x i) (invader-dx i)) 0)     ; bounce off left edge, change direction and reset x to left edge
         (make-invader 0
                       (+ (invader-dy i) (invader-y i))
                       (* -1 (invader-dx i))
                       (invader-dy i))] 
        [(>= (+ (invader-x i) (invader-dx i)) WIDTH) ; bounce off right edge, change direction and reset x to right edge
         (make-invader WIDTH
                       (+ (invader-dy i) (invader-y i))
                       (* -1 (invader-dx i))
                       (invader-dy i))] 
        [else
         (make-invader (+ (invader-dx i) (invader-x i))
                       (+ (invader-dy i) (invader-y i))
                       (invader-dx i)
                       (invader-dy i))]))


;; ListOfInvaders -> ListOfInvaders
;; Randomly generates a Natural[0, 100), if it is less than 2 then another invader is added to the given list
;; If an Invader is added, it will have a random x coordinate Natural[0, WIDTH) and y coordinate = 0
(check-random (add-invader LOI1) (if (< (random INVADE-RATE) 2)
                                     (cons (make-invader (random WIDTH) 0 INVADER-X-SPEED INVADER-Y-SPEED) LOI1)
                                     LOI1))
(check-random (add-invader LOI2) (if (< (random INVADE-RATE) 2)
                                     (cons (make-invader (random WIDTH) 0 INVADER-X-SPEED INVADER-Y-SPEED) LOI2)
                                     LOI2))
(check-random (add-invader LOI3) (if (< (random INVADE-RATE) 2)
                                     (cons (make-invader (random WIDTH) 0 INVADER-X-SPEED INVADER-Y-SPEED) LOI3)
                                     LOI3))

;(define (add-invader loi) empty) ;stub
;<not using template, pretty simple function>

(define (add-invader loi)
  (if (< (random INVADE-RATE) 2)
      (cons (make-invader (random WIDTH) 0 INVADER-X-SPEED INVADER-Y-SPEED) loi)
      loi))


;; ListOfMissiles -> ListOfMissles
;; Advances all existing missles. Removes missiles that have flown off game screen
(check-expect (advance-missiles empty) empty)
(check-expect (advance-missiles LOM2) (cons (make-missile 150 (- 300 MISSILE-SPEED)) empty))
(check-expect (advance-missiles LOM3)
              (cons (make-missile (invader-x I1) (- (+ (invader-y I1) 10) MISSILE-SPEED))
                    (cons (make-missile 40 (- 50 MISSILE-SPEED)) empty)))
(check-expect (advance-missiles (list M1 (make-missile 20 40) (make-missile 50 0))) ; one missile off screen
              (list (make-missile 150 (- 300 MISSILE-SPEED))
                    (make-missile 20 (- 40 MISSILE-SPEED))))
(check-expect (advance-missiles (list M1 (make-missile 75 -5) (make-missile 20 40) (make-missile 50 0))) ; two missiles off screen
              (list (make-missile 150 (- 300 MISSILE-SPEED))
                    (make-missile 20 (- 40 MISSILE-SPEED))))

;(define (advance-missiles lom) empty) ;stub
(define (advance-missiles lom)
  (prune-missiles (move-missiles lom)))


;; ListOfMissile -> ListOfMissile
;; removes all offscreen missiles from the given list
(check-expect (prune-missiles empty) empty)
(check-expect (prune-missiles (list M1 M2 M3))
              (list M1 M2 M3))
(check-expect (prune-missiles (list M1 (make-missile 20 40) (make-missile 50 0)))
              (list M1 (make-missile 20 40)))
(check-expect (prune-missiles (list M1 (make-missile 75 -5) (make-missile 20 40) (make-missile 50 0)))
              (list M1 (make-missile 20 40)))

;(define (prune-missiles lom) empty) ;stub
;<using template from ListOfMissile data definition>

(define (prune-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (if (<= (missile-y (first lom)) 0)
             (prune-missiles (rest lom)) 
             (cons (first lom) (prune-missiles (rest lom))))]))


;; ListOfMissle -> ListOfMissile
;; moves y position of all missiles in list by MISSLE-SPEED toward top of screen
(check-expect (move-missiles empty) empty)
(check-expect (move-missiles LOM2) (cons (make-missile 150 (- 300 MISSILE-SPEED)) empty))
(check-expect (move-missiles (cons (make-missile 45 100) (cons (make-missile 100 200) empty)))
              (cons (make-missile 45 (- 100 MISSILE-SPEED)) (cons (make-missile 100 (- 200 MISSILE-SPEED)) empty)))

;(define (move-missiles lom) empty) ;stub
;<using template from ListOfMissile data definition>

(define (move-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (cons (make-missile (missile-x (first lom)) (- (missile-y (first lom)) MISSILE-SPEED))
               (move-missiles (rest lom)))]))


;; ListOfMissile x y -> ListOfMissile
;; Adds a missile with the given x and y coordinates to the front of the given list
(check-expect (add-missile empty 50 100)
              (list (make-missile 50 100)))
(check-expect (add-missile (list M1) 60 150)
              (list (make-missile 60 150) M1))
(check-expect (add-missile (list M1 M2) (/ WIDTH 2) 25)
              (list (make-missile (/ WIDTH 2) 25) M1 M2))

;(define (add-missile lom x y) LOM1) ;stub
;didn't use template, is a simple function

(define (add-missile lom x y)
  (cons (make-missile x y) lom))


;; ListOfMissle Tank -> ListOfMissile
;; adds a new missle to the given list, located at the end of the turret of the given Tank
(check-expect (fire-missile empty T0) (add-missile empty (tank-x T0) (- HEIGHT (image-height TANK))))
(check-expect (fire-missile LOM1 T1) (add-missile LOM1 (tank-x T1) (- HEIGHT (image-height TANK))))
(check-expect (fire-missile LOM2 T2) (add-missile LOM2 (tank-x T2) (- HEIGHT (image-height TANK))))

;(define (fire-missile lom t) LOM1) ;stub
;didn't use template, is a simple function

(define (fire-missile lom t)
  (add-missile lom (tank-x t) (- HEIGHT (image-height TANK))))


;; Tank -> Tank
;; Advances the position of the given tank appropriately. Disallows movement outside bounds of screen
(check-expect (advance-tank T0) (move-tank T0))                        ; advance to the right
(check-expect (advance-tank T2) (move-tank T2))                        ; advance to the left
(check-expect (advance-tank (make-tank 0 -1)) (make-tank 0 -1))        ; at left wall moving left, can't move any more
(check-expect (advance-tank (make-tank WIDTH 1)) (make-tank WIDTH 1))  ; at right wall moving right, can't move any more      

;(define (advance-tank t) T0) ;stub
;<using template from Tank data definition>

(define (advance-tank t)
  (cond [(< (tank-x (move-tank t)) 0) t]     ; tank going off left edge, return existing tank with no modification
        [(> (tank-x (move-tank t)) WIDTH) t] ; tank going off right edge, return existing tank with no modification
        [else (move-tank t)]))               ; move tank normally
  

;; Tank -> Tank
;; Produces a tank whose x coordinate has been advanced by TANK-SPEED in tank-dir t direction
(check-expect (move-tank T0) (make-tank (+ (/ WIDTH 2) (* 1 TANK-SPEED)) 1))
(check-expect (move-tank T2) (make-tank (+ 50 (* -1 TANK-SPEED)) -1))

;(define (move-tank t) T0) ;stub
;<using template from Tank data definition>

(define (move-tank t)
  (make-tank (+ (tank-x t) (* TANK-SPEED (tank-dir t))) (tank-dir t)))


;; Game -> Image
;; displays the current game state
(check-expect (render-game G0) (render-invaders (game-invaders G0)
                                                (render-missiles (game-missiles G0)
                                                                 (render-tank (game-tank G0) BACKGROUND))))

; (define (render-game g) BACKGROUND) ;stub
;<using template frrom Game data definition>

(define (render-game g)
  (render-invaders (game-invaders g)
                   (render-missiles (game-missiles g)
                                    (render-tank (game-tank g) BACKGROUND))))


;; ListOfInvaders Image -> Image
;; produces an image where the given list of invaders (loi) is placed on the given image (img)
(check-expect (render-invaders LOI1 BACKGROUND) BACKGROUND)
(check-expect (render-invaders LOI2 BACKGROUND) (place-image INVADER 150 100 BACKGROUND))
(check-expect (render-invaders LOI3 BACKGROUND) (place-image INVADER 150 HEIGHT (place-image INVADER 50 23 BACKGROUND)))

;(define (render-invaders loi img) BACKGROUND) ;stub
;<using template from ListOfInvaders data definition>

(define (render-invaders loi img)
  (cond [(empty? loi) img]
        [else
         (place-image INVADER (invader-x (first loi)) (invader-y (first loi))
                      (render-invaders (rest loi) img))]))


;; ListOfMissiles Image -> Image
;; produces an image where the given list of missiles (lom) is placed on the given image (img)
(check-expect (render-missiles LOM1 BACKGROUND) BACKGROUND)
(check-expect (render-missiles LOM2 BACKGROUND) (place-image MISSILE 150 300 BACKGROUND))
(check-expect (render-missiles LOM3 BACKGROUND) (place-image MISSILE 150 110 (place-image MISSILE 40 50 BACKGROUND)))

;(define (render-missiles lom img) BACKGROUND) ;stub
;<using template from ListOfMissile data definition>

(define (render-missiles lom img)
  (cond [(empty? lom) img]
        [else
         (place-image MISSILE (missile-x (first lom)) (missile-y (first lom))
                      (render-missiles (rest lom) img))]))


;; Tank Image -> Image
;; produces an image where the given tank (t) is placed on the given image (img)
(check-expect (render-tank T0 BACKGROUND) (place-image TANK (/ WIDTH 2) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))
(check-expect (render-tank T1 BACKGROUND) (place-image TANK 50 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))
;(define (render-tank t img) BACKGROUND) ;stub
;<using template from Tank data definition>

(define (render-tank t img)
  (place-image TANK (tank-x t) (- HEIGHT TANK-HEIGHT/2) img))


;; Game -> Boolean
;; produces True if any invader ship in the game state has landed
(check-expect (game-over? G0) (ship-landed? empty))
(check-expect (game-over? G1) (ship-landed? empty))
(check-expect (game-over? G2) (ship-landed? (list I1)))
(check-expect (game-over? G3) (ship-landed? (list I1 I2)))
(check-expect (game-over? (make-game (list I1 I2 I3) empty T1)) ; an invader is beyond the HEIGHT, meaning it landed but y coord didnt exactly match HEIGHT when it was advanced
              (ship-landed? (list I1 I2 I3)))

;(define (game-over? g) false) ;stub
;<using template from Game data definition>

(define (game-over? g)
  (ship-landed? (game-invaders g)))


;; ListOfInvader -> Boolean
;; Produces true if any Invader in the given list has landed
(check-expect (ship-landed? empty) false)
(check-expect (ship-landed? (list I1)) false)
(check-expect (ship-landed? (list I1 I3)) true)
(check-expect (ship-landed? (list I1 I2 I3)) true)
(check-expect (ship-landed? (list I1 (make-invader 56 (- HEIGHT 10) 10 10))) false)

;(define (ship-landed? loi) false) ;stub
;<using template from ListOfInvader data definition>

(define (ship-landed? loi)
  (cond [(empty? loi) false]
        [else
         (if (>= (invader-y (first loi)) HEIGHT)
             true
             (ship-landed? (rest loi)))]))


;; Game KeyEvent -> Game
;; change the direction of the tank (left and right arrow keys) and/or fire missles (spacebar)
(check-expect (handle-key G0 "left")
              (make-game empty empty (make-tank (tank-x T0) -1)))
(check-expect (handle-key (make-game empty empty T2) "right")
              (make-game empty empty (make-tank (tank-x T2) 1)))
(check-expect (handle-key G0 "right")
              (make-game empty empty (make-tank (tank-x T0) 1)))
(check-expect (handle-key (make-game empty empty T2) "left")
              (make-game empty empty (make-tank (tank-x T2) -1)))
(check-expect (handle-key G0 " ")
              (make-game empty (fire-missile empty T0) T0))
(check-expect (handle-key G2 "F")
              G2)

;(define (handle-key g ke) G0) ;stub
;<using template from KeyEvent>

(define (handle-key g ke)
  (cond [(key=? ke "left")
         (make-game (game-invaders g)
                    (game-missiles g)
                    (make-tank (tank-x (game-tank g)) -1))]
        [(key=? ke "right")
         (make-game (game-invaders g)
                    (game-missiles g)
                    (make-tank (tank-x (game-tank g)) 1))]
        [(key=? ke " ")
         (make-game (game-invaders g)
                    (fire-missile (game-missiles g) (game-tank g))
                    (game-tank g))]
        [else g]))

