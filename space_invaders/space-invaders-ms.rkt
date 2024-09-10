;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-ms) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)
(define MTS (empty-scene WIDTH HEIGHT))

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
(define XLIMIT-L (+ 0 (image-width INVADER)))
(define XLIMIT-R (- WIDTH (image-width INVADER)))
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
(define (fn-for-game g)
  (... (fn-for-loi (game-invaders g))
       (fn-for-lom (game-missiles g))
       (fn-for-tank (game-tank g))))



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
;;  - (cons Missile (listof Missile))
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
(define (main g)
  (big-bang g                        ; Game
    (on-tick   next-game)            ; Game -> Game
    (to-draw   render-game)          ; Game -> Image
    (stop-when game-over)            ; Game -> Boolean
    (on-key    handle-controls)))    ; Game KeyEvent -> Game

;; Game -> Game
;; Produce the next state of the game and
;; handle the logic of all the game elements.
(check-random (next-game (make-game (list (make-invader 54 99 INVADER-X-SPEED)
                                          (make-invader 110 241 INVADER-X-SPEED)
                                          (make-invader 154 284 INVADER-X-SPEED)
                                          (make-invader 281 405 INVADER-X-SPEED))
                                    (list (make-missile 110 32)
                                          (make-missile 74 79)
                                          (make-missile 19 29))
                                    TANK-START))
              (make-game (list (make-invader 114
                                             0
                                             INVADER-X-SPEED)
                               (make-invader (+ 54 INVADER-X-SPEED)
                                             (+ 99 INVADER-Y-SPEED)
                                             INVADER-X-SPEED)
                               (make-invader (+ 110 INVADER-X-SPEED)
                                             (+ 241 INVADER-Y-SPEED)
                                             INVADER-X-SPEED)
                               (make-invader (+ 154 INVADER-X-SPEED)
                                             (+ 284 INVADER-Y-SPEED)
                                             INVADER-X-SPEED)
                               (make-invader WIDTH
                                             (+ 405 INVADER-Y-SPEED)
                                             INVADER-X-SPEED))
                         (list (make-missile (+ 110 MISSILE-SPEED) 32)
                               (make-missile (+ 74 MISSILE-SPEED) 79)
                               (make-missile (+ 19 MISSILE-SPEED) 29))
                         TANK-START))

;(define (next-game g) g)

(define (next-game g)
  (make-game (next-invaders (game-invaders g))
             (next-missiles (game-missiles g))
             (game-tank g)))


;; (listof Invader) -> (listof Invader)
;; handle the game logic for the invaders
(check-random (next-invaders empty) empty)
(check-random (next-invaders (list (make-invader 54 99 INVADER-X-SPEED)
                                   (make-invader 110 241 INVADER-X-SPEED)
                                   (make-invader 154 284 INVADER-X-SPEED)
                                   (make-invader 281 405 INVADER-X-SPEED)))
              (list (make-invader 114
                                  0
                                  INVADER-X-SPEED)
                    (make-invader (+ 54 INVADER-X-SPEED)
                                  (+ 99 INVADER-Y-SPEED)
                                  INVADER-X-SPEED)
                    (make-invader (+ 110 INVADER-X-SPEED)
                                  (+ 241 INVADER-Y-SPEED)
                                  INVADER-X-SPEED)
                    (make-invader (+ 154 INVADER-X-SPEED)
                                  (+ 284 INVADER-Y-SPEED)
                                  INVADER-X-SPEED)
                    (make-invader WIDTH
                                  (+ 405 INVADER-Y-SPEED)
                                  INVADER-X-SPEED)))

;(define (next-invaders loi) loi)

;<Template from Function Composition>

(define (next-invaders loi)
  (spawn-invaders
   (filter-invaders
    (move-invaders loi))))


;; (listof Invader) -> (listof Invader)
;; make all the invaders move towards the player
;; while shifting left and right
(check-expect (move-invaders empty) empty)
(check-expect (move-invaders (list (make-invader (/ WIDTH 2)
                                                 (/ HEIGHT 2)
                                                 INVADER-X-SPEED)
                                   (make-invader (/ WIDTH 2)
                                                 (/ HEIGHT 2)
                                                 (- INVADER-X-SPEED))
                                   (make-invader (- XLIMIT-R (- INVADER-X-SPEED 1))
                                                 100
                                                 INVADER-X-SPEED)
                                   (make-invader (- XLIMIT-L (- INVADER-X-SPEED 1))
                                                 100
                                                 (- INVADER-X-SPEED))
                                   (make-invader XLIMIT-R
                                                 100
                                                 INVADER-X-SPEED)
                                   (make-invader XLIMIT-L
                                                 100
                                                 (- INVADER-X-SPEED))))
              (list (make-invader (+ (/ WIDTH 2) INVADER-X-SPEED)
                                  (+ (/ HEIGHT 2) INVADER-Y-SPEED)
                                  INVADER-X-SPEED)
                    (make-invader (+ (/ WIDTH 2) (- INVADER-X-SPEED))
                                  (+ (/ HEIGHT 2) INVADER-Y-SPEED)
                                  (- INVADER-X-SPEED))
                    (make-invader XLIMIT-R
                                  (+ 100 INVADER-Y-SPEED)
                                  INVADER-X-SPEED)
                    (make-invader XLIMIT-L
                                  (+ 100 INVADER-Y-SPEED)
                                  (- INVADER-X-SPEED))
                    (make-invader XLIMIT-R
                                  100
                                  (- INVADER-X-SPEED))
                    (make-invader XLIMIT-L
                                  100
                                  INVADER-X-SPEED)))

;(define (move-invaders loi) loi)

(define (move-invaders loi)
  (cond [(empty? loi) empty]
        [else
         (cons (move-invader (first loi))
               (move-invaders (rest loi)))]))

;; Invader -> Invader
;; make the given invader move toward the player
;; and shifting left and right
(check-expect (move-invader (make-invader (/ WIDTH 2)
                                          (/ HEIGHT 2)
                                          INVADER-X-SPEED))
              (make-invader (+ (/ WIDTH 2) INVADER-X-SPEED)
                            (+ (/ HEIGHT 2) INVADER-Y-SPEED)
                            INVADER-X-SPEED))
(check-expect (move-invader (make-invader (/ WIDTH 2)
                                          (/ HEIGHT 2)
                                          (- INVADER-X-SPEED)))
              (make-invader (+ (/ WIDTH 2) (- INVADER-X-SPEED))
                            (+ (/ HEIGHT 2) INVADER-Y-SPEED)
                            (- INVADER-X-SPEED)))
(check-expect (move-invader (make-invader (- XLIMIT-R (- INVADER-X-SPEED 1))
                                          100
                                          INVADER-X-SPEED))
              (make-invader XLIMIT-R
                            (+ 100 INVADER-Y-SPEED)
                            INVADER-X-SPEED))
(check-expect (move-invader (make-invader (- XLIMIT-L (- INVADER-X-SPEED 1))
                                          100
                                          (- INVADER-X-SPEED)))
              (make-invader XLIMIT-L
                            (+ 100 INVADER-Y-SPEED)
                            (- INVADER-X-SPEED)))
(check-expect (move-invader (make-invader XLIMIT-R
                                          100
                                          INVADER-X-SPEED))
              (make-invader XLIMIT-R
                            100
                            (- INVADER-X-SPEED)))
(check-expect (move-invader (make-invader XLIMIT-L
                                          100
                                          (- INVADER-X-SPEED)))
              (make-invader XLIMIT-L
                            100
                            INVADER-X-SPEED))

;(define (move-invader i) i)

(define (move-invader i)
  (cond [(or (and (= (invader-x i) XLIMIT-R)
                  (> (invader-dx i) 0))
             (and (= (invader-x i) XLIMIT-L)
                  (< (invader-dx i) 0)))
         (make-invader (invader-x i)
                       (invader-y i)
                       (- (invader-dx i)))]
        [(and (<= (abs (- XLIMIT-R (invader-x i))) (abs (invader-dx i)))
              (> (invader-dx i) 0))
         (make-invader XLIMIT-R
                       (+ (invader-y i) INVADER-Y-SPEED)
                       (invader-dx i))]
        [(and (<= (abs (- XLIMIT-L (invader-x i))) (abs (invader-dx i)))
              (< (invader-dx i) 0))
         (make-invader XLIMIT-L
                       (+ (invader-y i) INVADER-Y-SPEED)
                       (invader-dx i))]
        [else
         (make-invader (+ (invader-x i) (invader-dx i))
                       (+ (invader-y i) INVADER-Y-SPEED)
                       (invader-dx i))]))



;; (listof Invader) -> (listof Invader)
;; filter out the invaders that are outside
;; of the screen
;; !!!
(define (filter-invaders loi) ...)


;; (listof Invader) -> (listof Invader)
;; create new invaders for the player to kill
;; !!!
(define (spawn-invaders loi) ...)



;; (listof Missile) -> (listof Missile)
;; make each missile in the list move for the game
;; !!!
(define (next-missiles lom) ...)


;; Game -> Image
;; Render all the game elements on the screen.
(check-expect (render-game (make-game (list (make-invader 44 109 INVADER-X-SPEED)
                                            (make-invader 100 251 INVADER-X-SPEED)
                                            (make-invader 144 294 INVADER-X-SPEED)
                                            (make-invader 271 415 INVADER-X-SPEED))
                                      (list (make-missile 100 42)
                                            (make-missile 64 89)
                                            (make-missile 9 10))
                                      TANK-START))
              (place-image INVADER 44 109
                           (place-image INVADER 100 251
                                        (place-image INVADER 144 294
                                                     (place-image INVADER 271 415
                                                                  (place-image MISSILE 100 42
                                                                               (place-image MISSILE 64 89
                                                                                            (place-image MISSILE 9 10
                                                                                                         (place-image TANK
                                                                                                                      (tank-x TANK-START)
                                                                                                                      TANK-YPOS
                                                                                                                      MTS)))))))))

;(define (render-game g) MTS)

(define (render-game g)
  (render-invaders (game-invaders g)
                   (render-missiles (game-missiles g)
                                    (render-tank (game-tank g)
                                                 MTS))))


;; Tank Image -> Image
;; render the tank on the given image based on
;; its x position and default y position
(check-expect (render-tank (make-tank 64 -1) MTS)
              (place-image TANK 64 TANK-YPOS MTS))

;(define (render-tank t i) MTS)

;<Template from Tank with added parameter, Image>

(define (render-tank t i)
  (place-image TANK (tank-x t) TANK-YPOS i))


;; (listof Missile) Image -> Image
;; render the given missiles on the given image
;; based on their respective x- and y- positions
(check-expect (render-missiles empty MTS) MTS)
(check-expect (render-missiles (list (make-missile 100 289)) MTS)
              (place-image MISSILE 100 289 MTS))
(check-expect (render-missiles (list (make-missile 167 169)
                                     (make-missile 56 341)
                                     (make-missile 214 418)) MTS)
              (place-image MISSILE 167 169
                           (place-image MISSILE 56 341
                                        (place-image MISSILE 214 418 MTS))))

;(define (render-missiles lom i) i)

;<Template from (listof Missile) with added parameter, Image>

(define (render-missiles lom i)
  (cond [(empty? lom) i]
        [else
         (place-image MISSILE
                      (missile-x (first lom))
                      (missile-y (first lom))
                      (render-missiles (rest lom) i))]))


;; (listof Invader) Image -> Image
;; render the given invaders on the given image
;; based on their respective x- and y- positions
(check-expect (render-invaders empty MTS) MTS)
(check-expect (render-invaders (list (make-invader 100 101 INVADER-X-SPEED))
                               MTS)
              (place-image INVADER 100 101 MTS))
(check-expect (render-invaders (list (make-invader 57 195 INVADER-X-SPEED)
                                     (make-invader 197 315 INVADER-X-SPEED)
                                     (make-invader 214 441 INVADER-X-SPEED))
                               MTS)
              (place-image INVADER 57 195
                           (place-image INVADER 197 315
                                        (place-image INVADER 214 441 MTS))))

;(define (render-invaders loi i) i)

;<Template from (listof Invader) with added parameter, Image>

(define (render-invaders loi i)
  (cond [(empty? loi) i]
        [else
         (place-image INVADER
                      (invader-x (first loi))
                      (invader-y (first loi))
                      (render-invaders (rest loi) i))]))


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



;;;;;;;;;;;; Make the handle-controls move the tank, as well. This function is still not complete.

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