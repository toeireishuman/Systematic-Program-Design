;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-ms) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)
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
(define TANK-HEIGHT (* 2 TANK-HEIGHT/2))
(define TANK-YPOS (+ 11 (- HEIGHT TANK-HEIGHT)))
(define MISSILE (ellipse 5 15 "solid" "red"))

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)


;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loi (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))          ;center going right
(define T1 (make-tank 50 1))                   ;going right
(define T2 (make-tank 50 -1))                  ;going left
(define TANK-START (make-tank (/ WIDTH 2) 0))  ;center motionless

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
(define (fn-for-invader i)
  (... (invader-x i) (invader-y i) (invader-dx i)))

;; (listof Invader) is one of:
;;  - empty
;;  - (cons Invader (listofInvader))
;; interp. a list of invaders
(define LOI1 (list I1 I2 I3))

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (fn-for-invader (first loi))
              (fn-for-loi (rest loi)))]))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates
(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

;; (listof Missile) is one of:
;;  - empty
;;  - (list Missile (listof Missile))
;; interp. a list of missiles
(define LOM1 (list M1 M2 M3))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom (rest lom)))]))


(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))


;; =================
;; Functions:

;; Game -> Game
;; start the world with (main (make-game empty empty TANK-START)
;; 
(define (main s)
  (big-bang s                        ; Game
    (on-tick   next-game)            ; Game -> Game
    (to-draw   render-game)          ; Game -> Image
    (stop-when game-over)            ; Game -> Boolean
    (on-key    handle-controls)))    ; Game KeyEvent -> Game

;; Game -> Game
;; Produce the next state of the game and
;; handle the logic of all the game elements.
;; !!!
(define (next-game s) ...)


;; Game -> Image
;; Render all the game elements on the screen.
;; !!!
(define (render-game s) ...)


;; Game -> Boolean
;; Stop the program when player loses.
;; The lose conditions include:
;;  - an invader hits the tank, and
;;  - an invader passes through the bottom side of the screen.
(check-expect (game-over (make-game (list (make-invader 50 50 INVADER-X-SPEED)
                                          (make-invader 150 TANK-YPOS INVADER-X-SPEED)
                                          (make-invader (- 150 2) (- TANK-YPOS 2) INVADER-X-SPEED)
                                          (make-invader (- 150 HIT-RANGE) (- TANK-YPOS HIT-RANGE) INVADER-X-SPEED))
                                    (list (make-missile 40 40)
                                          (make-missile 150 TANK-YPOS)
                                          (make-missile (- 150 2) (- TANK-YPOS 2))
                                          (make-missile (- 150 HIT-RANGE) (- TANK-YPOS HIT-RANGE)))
                                    (make-tank 150 0)))
              true)
(check-expect (game-over (make-game (list (make-invader 64 64 INVADER-X-SPEED))
                                    (list (make-missile 11 11))
                                    (make-tank 150 0)))
              false)

;(define (game-over g) false)

(define (game-over g)
  (or (invaders-past-tank? (game-invaders g))
      (invaders-hit-tank? (game-invaders g) (game-tank g))))


;; (listof Invader) -> Boolean
;; Return true if any invader in the list is
;; on or past the screen's bottom side,
;; passing the tank
(check-expect (invaders-past-tank? empty)
              false)
(check-expect (invaders-past-tank? (list (make-invader 1 1 INVADER-X-SPEED)
                                         (make-invader 100 HEIGHT INVADER-X-SPEED)
                                         (make-invader 49 (+ HEIGHT 1) INVADER-X-SPEED)))
              true)

;(define (invaders-past-tank? loi) false)

(define (invaders-past-tank? loi)
  (cond [(empty? loi) false]
        [else
         (if (invader-past-tank? (first loi))
             true
             (invaders-past-tank? (rest loi)))]))

;; Invader -> Boolean
;; Return true if the given invader is
;; on or past the screen's bottom side
(check-expect (invader-past-tank? (make-invader 2 100 INVADER-X-SPEED))
              false)
(check-expect (invader-past-tank? (make-invader 44 HEIGHT INVADER-X-SPEED))
              true)
(check-expect (invader-past-tank? (make-invader 98 (+ HEIGHT 1) INVADER-X-SPEED))
              true)

;(define (invader-past-tank? loi t) false)

(define (invader-past-tank? i)
  (>= (invader-y i) HEIGHT))


;; (listof Invader) Tank -> Boolean
;; Return true if any invader in the list is
;; within the hit range of the tank
(check-expect (invaders-hit-tank? empty (make-tank 200 1)) false)
(check-expect (invaders-hit-tank? (list (make-invader 50 50 INVADER-X-SPEED)
                                        (make-invader 111 TANK-YPOS INVADER-X-SPEED)
                                        (make-invader (- 111 2) (- TANK-YPOS 2) INVADER-X-SPEED)
                                        (make-invader (- 111 HIT-RANGE) (- TANK-YPOS HIT-RANGE) INVADER-X-SPEED))
                                  (make-tank 111 0))
              true)

;(define (invaders-hit-tank? loi t) false)

;<Template from (listof Invader) with added parameter, Tank>

(define (invaders-hit-tank? loi t)
  (cond [(empty? loi) false]
        [else
         (if (invader-hit-tank? (first loi) t)
             true
             (invaders-hit-tank? (rest loi) t))]))

;; Invader Tank -> Boolean
;; Return true if the given invader is
;; withint the hit range of the tank
(check-expect (invader-hit-tank? (make-invader 11 11 INVADER-X-SPEED)
                                 (make-tank 100 -1))
              false)
(check-expect (invader-hit-tank? (make-invader 24 TANK-YPOS INVADER-X-SPEED)
                                 (make-tank 24 0))
              true)
(check-expect (invader-hit-tank? (make-invader (- 160 (- HIT-RANGE 1)) (- TANK-YPOS (- HIT-RANGE 1)) INVADER-X-SPEED)
                                 (make-tank 160 1))
              true)
(check-expect (invader-hit-tank? (make-invader (- 111 HIT-RANGE) (- TANK-YPOS HIT-RANGE) INVADER-X-SPEED)
                                 (make-tank 111 -1))
              true)

;(define (invader-hit-tank? i t) false)

;<Template from Invader with added parameter, Tank>

(define (invader-hit-tank? i t)
  (and (<= (abs (- (invader-x i) (tank-x t))) HIT-RANGE)
       (<= (abs (- (invader-y i) TANK-YPOS)) HIT-RANGE)))



;; Game KeyEvent -> Game
;; Handle the key inputs from the player.
;; The controls include:
;;  - "left" -> (game-dir g) = -1
;;  - "right" -> (game-dir g) = 1
;;  - spacebar -> tank launches missiles
;;  - no key press or any other key -> (game-dir g) = 0 and no missiles
(check-expect (handle-controls (make-game empty empty (make-tank 64 0)) "a")
              (make-game empty empty (make-tank 64 0)))
(check-expect (handle-controls (make-game empty empty (make-tank 100 0)) " ")
              (make-game empty (cons (make-missile 100 TANK-YPOS) empty) (make-tank 100 0)))
(check-expect (handle-controls (make-game empty
                                          (list (make-missile 54 100) (make-missile 14 99))
                                          (make-tank 100 0))
                               " ")
              (make-game empty
                         (cons (make-missile 100 TANK-YPOS)
                               (list (make-missile 54 100) (make-missile 14 99)))
                         (make-tank 100 0)))
(check-expect (handle-controls (make-game empty empty (make-tank 111 0)) "left")
              (make-game empty empty (make-tank 111 -1)))
(check-expect (handle-controls (make-game empty empty (make-tank 64 0)) "right")
              (make-game empty empty (make-tank 64 1)))

;(define (handle-controls g ke) g)

;<Template from KeyEvent>

(define (handle-controls g ke)
  (cond [(key=? ke " ") (make-game (game-invaders g)
                                   (cons (make-missile (tank-x (game-tank g)) TANK-YPOS)
                                         (game-missiles g))
                                   (game-tank g))]
        [(key=? ke "left") (make-game (game-invaders g)
                                      (game-missiles g)
                                      (make-tank (tank-x (game-tank g)) -1))]
        [(key=? ke "right") (make-game (game-invaders g)
                                       (game-missiles g)
                                       (make-tank (tank-x (game-tank g)) 1))]
        [else 
         g]))