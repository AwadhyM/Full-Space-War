;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Full-Space-War) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Full-Space-War

;; Constant definitions

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (rectangle WIDTH HEIGHT "solid" "black"))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "white")              
              -5 6
              (ellipse 20 10 "solid"   "white")))

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "green")       ;tread center
                       (ellipse 30 10 "solid" "white"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "green")       ;gun
                     (rectangle 20 10 "solid" "green"))))   ;main body

;Data Definitions

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

(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))

;Data that represents how many invaders are in the game 
;ListOfInvaders is one of:
 ; - empty
 ; - cons Invader ListOfInvaders 

(define LOI0 empty) ; no invaderss in the game
(define LOI1 (cons I1 empty)) ; 1 invader
(define LOI2 (list I1`I2 I3)) ;multiple invaders

;template for functions

(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]                   ;BASE CASE
        [else (... (first loi)                 ;String
                   (fn-for-loi (rest loi)))])) ;NATURAL RECURSION


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

;Data that represents how many missiles are in the game 
;ListOfMissiles is one of:
 ; - empty
 ; - cons Missile ListOfMissiles 

(define LOM0 empty) ; no missiles in the game
(define LOM1 (cons M1 empty)) ; 1 missile
(define LOM2 (list (make-missile 100 150) (make-missile 200 210) (make-missile 50 50))) ;multiple missiles

;template for functions

(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]                   ;BASE CASE
        [else (... (first lom)                 ;String
                   (fn-for-lom (rest lom)))])) ;NATURAL RECURSION

; Score is Number
; interp. score that has been accumulated so far in the game
(define START-SCORE 0)
(define MID-GAME-SCORE 50)

#;
(define (fn-for-score s)
  (... s))


(define-struct game (invaders missiles tank score))
;; game is (make-game (listOf Invaders) (listOf Missile) Tank Score))
 ;represents the current state of the game world such as number of invaders on screen, and the score of the current player

(define G0 (make-game empty empty T0 0))
(define G1 (make-game empty empty T1 0))
(define G4 (make-game empty empty T2 0))
(define G2 (make-game (list I1) (list M1) T1 5))
(define G3 (make-game (list I1 I2) (list M1 M2) T1 25))

#;;template for functions relating to the game state 
(define (fn-for-game g)
  (... (fn-for-loinvader (game-invaders g))
       (fn-for-lom (game-missiles g))
       (fn-for-tank (game-tank g))
       (fn-for-score (game-score g))))

;; Functions 

;; Function that alters the game state (Using the big-bang functionality of this language) 
(define (start-game game)
  (big-bang game                   ; game
            (on-tick  move-data)     ; game -> game
            (to-draw  render-data)   ; game -> Image
            ;(on-mouse  shoot-missile)      ; game Integer Integer MouseEvent -> game
            (on-key    change-direction)))    ;game key -> game

; #1 - Change-direction
;game key --> game
;Change the direction that the tank is moving in depending on which key is pressed
;(define (change-direction game ke) game)

(check-expect (change-direction (make-game empty empty T1 0) "left") (make-game empty empty T2 0))
(check-expect (change-direction (make-game empty empty T1 0) "right") (make-game empty empty T1 0))
(check-expect (change-direction (make-game empty empty T2 0) "up") (make-game empty empty T2 0))

(define (change-direction game ke)
  (cond [(same-direction? game ke) game]
        [else 
         (if (key=? ke "left")
             (make-game (game-invaders game) (game-missiles game) (make-tank (tank-x (game-tank game)) -1) (game-score game)) 
             (make-game (game-invaders game) (game-missiles game) (make-tank (tank-x (game-tank game)) 1) (game-score game)))]))

; #2 - same-direction? (helper function)
; game + ke --> boolean
;Produce true if key pressed matches the direction that the tank is already going in
;(define (same-direction? ke game) true)

(check-expect (same-direction? (make-game empty empty T1 0) "left") false)
(check-expect (same-direction? (make-game empty empty T1 0) "up") true)
(check-expect (same-direction? (make-game empty empty T1 0) "right") true)

(define (same-direction? game ke)
  (cond [(and (key=? ke "left") (= (tank-dir (game-tank game)) -1)) true]
        [(and (key=? ke "right") (= (tank-dir (game-tank game)) 1)) true]
        [(false? (or (key=? ke "left") (key=? ke "right"))) true]
        [else
         false]))

; #3 - render-data
;ListOfInvaders + ListOfMissiles + tank + score --> Image
;(render-data (invaders missiles tank))
;Present the data in the game as an image
;(define (render-data invaders missiles tank))

;Tests for showing tank
(check-expect (render-data G0) (place-image TANK (tank-x T0) 480 BACKGROUND))
(check-expect (render-data (make-game empty empty T1 0)) (place-image TANK (tank-x T1) 480 BACKGROUND))
(check-expect (render-data (make-game empty empty (make-tank 140 -1) 0)) (place-image TANK 140 480 BACKGROUND))


(define (render-data game)
     (overlay
      ;(render-invaders (game-invaders game))
      ;(render-missiles (game-missiles game))
      (render-tank (game-tank game))
      ;(render-score (game-score game))
      BACKGROUND
              ))

; #4 - place-tank
;tank --> image
;Helper function that allows the render-data game to render the tank
;(define (render-tank tank) TANK)

(check-expect (render-tank T1) (place-image TANK (tank-x T1) 480 BACKGROUND))

(define (render-tank tank)
  (place-image TANK (tank-x tank) 480 BACKGROUND))

; #5 - move-data
; Game --> Game
;Ensure that data is changing with each second to match flow of the game
;(define (move-data game) game)

;Tests for movement of tank
;(check-expect (move-data G0)(make-game empty empty (make-tank (+ (tank-x T0) TANK-SPEED) (tank-dir T0)))) ;Tank going right
;(check-expect (move-data (make-game empty empty T2))(make-game empty empty (make-tank (- (tank-x T2) TANK-SPEED) (tank-dir T2)))) ;Tank going left

(define (move-data game) 
  (make-game
       empty
       empty
       (move-tank (game-tank game))
       0))

; #6 - move-tank (helper function)
;;tank --> tank
; Change the x coordinate of the tank
;(define (move-tank tank) game)

(check-expect (move-tank T1) (make-tank (+ (tank-x T1) TANK-SPEED) (tank-dir T1)))
(check-expect (move-tank T2) (make-tank (- (tank-x T2) TANK-SPEED) (tank-dir T2)))

(define (move-tank tank)
  (cond [(and (out-of-bounds? tank) (= (tank-dir tank) 1) (> (tank-x tank) 279)) tank]
        [(and (out-of-bounds? tank) (= (tank-dir tank) -1) (< (tank-x tank) 21)) tank]
        [else
         (if (> (tank-dir tank) 0)
             (make-tank (+ (tank-x tank) TANK-SPEED) (tank-dir tank))
             (make-tank (- (tank-x tank) TANK-SPEED) (tank-dir tank)))]))

; #7 - out-of-bounds? (helper function)
;; tank --> boolean
;; Check the x coordinate of the tank to prevent it from going out of the screen
;(define (out-of-bounds? tank) true)

(check-expect (out-of-bounds? (make-tank 300 1)) true)
(check-expect (out-of-bounds? (make-tank 3 -1)) true)
(check-expect (out-of-bounds? (make-tank 50 1)) false)

(define (out-of-bounds? tank)
  (if (or (> (tank-x tank) 280) (< (tank-x tank) 20))
      true
      false))

; #8- Functions for the missiles
; game x y me --> game
; Add missiles to the game 
;(define (shoot-missile game x y me) game)

(check-expect (shoot-missile (make-game empty empty (make-tank 50 1) 0) 100 150 "button-down") (make-game empty (list (make-missile 51 TANK-HEIGHT/2)) (make-tank 50 1) 0)) ; valid click
(check-expect (shoot-missile (make-game empty empty (make-tank 50 1) 0) 100 150 "drag") (make-game empty empty (make-tank 50 1))) ;invalid me
(check-expect (shoot-missile (make-game empty
                                        (list (make-missile 100 150) (make-missile 20 30) (make-missile 80 90)) (make-tank 50 1) 0) 40 70 "button-down")
                                        (make-game empty (list (make-missile 51 TANK-HEIGHT/2)(make-missile 100 150) (make-missile 20 30) (make-missile 80 90)) (make-tank 50 1) 0))


(define (shoot-missile game x y me)
  (cond [(mouse=? me "button-down") (make-game (game-invaders game) (cons (make-missile (+ (tank-dir (game-tank game)) (tank-x (game-tank game))) TANK-HEIGHT/2) (game-missiles game)) (game-tank game) (game-score game))]
        [else game]))

; #9 - Function for the missiles - next-missiles
;lom --> lom
; Advance and filter the list of missiles
(define (next-missiles lom loi)
  (on-screen-only (advance-missiles (filter-missiles lom loi))))


(check-expect (next-missiles empty empty) empty)
(check-expect (next-missiles (list (make-missile 15 15) (make-missile 100 500) (make-missile 30 20) (make-missile 50 500)) empty) (list (make-missile 15 25) (make-missile 30 30))) 