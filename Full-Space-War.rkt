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
