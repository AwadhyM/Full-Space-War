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

(define-struct game (invaders missile tank score))
;; game is (make-game (listOf Invaders) (listOf Missile) Tank Score))
 ;represents the current state of the game world such as number of invaders on screen, and the score of the current player

#;;template for functions relating to the game state 
(define (fn-for-game g)
  (... (fn-for-loinvader (game-invaders g))
       (fn-for-lom (game-missiles g))
       (fn-for-tank (game-tank g))
       (fn-for-score (game-score g))))

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
(define LOM2 (list I1`I2 I3)) ;multiple invaders

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