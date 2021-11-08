(require 2htdp/image)
(require 2htdp/universe)

;; My final version of the "Space Invaders" game

;; =================
;; Constants:

(define WIDTH 400)
(define HEIGHT (+ 100 WIDTH))
(define MTS (empty-scene WIDTH HEIGHT))
(define PLAYER-IMG (above (rectangle 7 10 "solid" "black")
                          (rectangle 24 7 "solid" "black")
                          (ellipse 33 8 "solid" "black")))
(define MISSILE-IMG (ellipse 5 12 "solid" "red"))
(define ENEMY-BOTTOM-IMG (ellipse 30 12 "solid" "blue"))
(define ENEMY-IMG (overlay/align "middle" "bottom" (circle 10 "outline" "blue")
                                 ENEMY-BOTTOM-IMG))
(define GAME-OVER-IMG (text "GAME OVER" 36 "red"))
(define PLAYER-SPEED 2)
(define MISSILE-SPEED 5)
(define ENEMY-SPEED 1)
(define CTR-X (/ WIDTH 2))                                      ; x-coord of the player's starting position
(define CTR-Y (/ HEIGHT 2))                                     ; Needed for game-over image
(define HALF-MISSILE-W (/ (image-width MISSILE-IMG) 2))         ; Needed for hit detection, testing and such
(define HALF-ENEMY-W (/ (image-width ENEMY-IMG) 2))
(define HALF-PLAYER-W (/ (image-width PLAYER-IMG) 2))
(define HALF-MISSILE-H (/ (image-height MISSILE-IMG) 2))
(define HALF-ENEMY-H (/ (image-height ENEMY-BOTTOM-IMG) 2))


;; =================
;; Data definitions:

;; Direction is one of:
;; - "l"
;; - "r"
;; - "n"
;; interp. x-direction of a moving object on the screen:
;; "l" means "left"  - a moving object is moving from right to left
;; "r" means "right" - a moving object is moving from left to right
;; "n" means "neutral" - x-coord of a moving object doesn't change

;; <examples are redundant for enumerations>

#;
(define (fn-for-direction d)
  (cond [(string=? "l" d) (...)]
        [(string=? "r" d) (...)]
        [(string=? "n" d) (...)]))

;; Template rules used:
;; - one of: 3 cases
;; - atomic distinct: "l"
;; - atomic distinct: "r"
;; - atomic distinct: "n"


;; Type is one of:
;; - 1
;; - 2
;; - 3
;; interp. the type of a moving object, where
;; 1 represents a player's tank
;; 2 represents an enemy spaceship
;; 3 represents a player's missile

;; <examples are redundant for enumerations>

#;
(define (fn-for-type t)
  (cond [(= 1 t) (...)]
        [(= 2 t) (...)]
        [(= 3 t) (...)]))

;; Template rules used:
;; - one of: 3 cases
;; - atomic distinct: 1
;; - atomic distinct: 2
;; - atomic distinct: 3


(define-struct moving-object (x y dir type))
;; Moving-object is (make-moving-object Natural[0, WIDTH] Natural[0, HEIGHT] Direction Type)
;; interp. a moving object where:
;; - x is its x-position
;; - y is its y-position
;; - dir is its Direction
;; - type is its Type

(define MOVING1 (make-moving-object (/ WIDTH 2) HEIGHT "n" 1))    ; a player at its starting position
(define MOVING2 (make-moving-object 75 30 "n" 3))                 ; a missile
(define MOVING3 (make-moving-object 110 47 "l" 2))                ; enemy spaceship


#;
(define (fn-for-moving-object m)
  (... (moving-object-x m)             ; Natural[0, WIDTH]
       (moving-object-y m)             ; Natural[0, HEIGHT]
       (fn-for-direction
        (moving-object-dir m))         ; Direction
       (fn-for-type
        (moving-object-type m))))      ; Type

;; Template rules used:
;; - Compound: 4 fields
;; - Reference: dir field is Direction
;; - Reference: type field is Type


;; ListOfMoving is one of:
;; - empty
;; - (cons Moving-object ListOfMoving)
;; interp. a list of moving objects

(define LOM1 empty)
(define LOM2 (cons MOVING1 empty))
(define LOM3 (cons MOVING1 (cons MOVING3 (cons MOVING2 empty))))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
          (... (fn-for-moving-object (first lom))
               (fn-for-lom (rest lom)))]))

;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Moving-object ListOfMoving)
;; - reference: (first lom) is  Moving-object
;; - self-reference: (rest lom) is ListOfMoving


;; =================
;; Functions:

;; ListOfMoving -> ListOfMoving
;; start the game with (main (cons MOVING1 empty))

(define (main lom)
  (big-bang lom                                        ; ListOfMoving
            (on-tick advance-all)                      ; ListOfMoving -> ListOfMoving
            (to-draw   render-all)                     ; ListOfMoving -> Image
            (on-key control-tank)                      ; ListOfMoving KeyEvent -> ListOfMoving
            (stop-when end? last-picture)))            ; ListOfMoving -> Boolean (end) -> Image (last-picture)

;; ListOfMoving -> ListOfMoving
;; Produce a filtered and ticked list of all moving objects, create up to 3 enemies if there are less on the screen

(check-random (advance-all (list (make-moving-object CTR-X HEIGHT "r" 1)
                                 (make-moving-object WIDTH 40 "r" 2)
                                 (make-moving-object 0 30 "l" 2)
                                 (make-moving-object 30 70 "n" 3)
                                 (make-moving-object 40 2 "n" 3)))
              (list (make-moving-object (random WIDTH) 0 "r" 2)                    ; new enemy generated
                    (make-moving-object (+ CTR-X PLAYER-SPEED) HEIGHT "r" 1)       ; player moves right
                    (make-moving-object WIDTH 40 "l" 2)                            ; enemy switches direction to left
                    (make-moving-object 0 30 "r" 2)                                ; enemy switches direction to right
                    (make-moving-object 30 (- 70 MISSILE-SPEED) "n" 3)))           ;1 missile goes "up", 1 filtered out
(check-random (advance-all (list (make-moving-object CTR-X HEIGHT "l" 1)
                                (make-moving-object 30 30 "l" 2)
                                (make-moving-object 50 50 "r" 2)
                                (make-moving-object 100 100 "r" 2)
                                (make-moving-object 100 100 "n" 3)))
             (list (make-moving-object (random WIDTH) 0 "r" 2)                     ; 1 enemy is generated because 1 got hit by a missile
                   (make-moving-object (- CTR-X PLAYER-SPEED) HEIGHT "l" 1)        ; player moves left
                   (make-moving-object (- 30 ENEMY-SPEED)
                                       (+ 30 ENEMY-SPEED) "l" 2)                   ; enemy moves left and down
                   (make-moving-object (+ 50 ENEMY-SPEED)
                                       (+ 50 ENEMY-SPEED) "r" 2)                   ; enemy moves right and down
                   (make-moving-object 100 (- 100 MISSILE-SPEED) "n" 3)))          ; 1 enemy dead, missile continues its flight
                                
;(define (advance-all lom) empty)   ;stub

(define (advance-all lom)
  (make-enemies (onscreen-only (tick-all lom))))

;; ListOfMoving -> ListOfMoving
;; make enemies up to 3 if less enemies are on screen
(check-expect (make-enemies (list MOVING1 (make-moving-object 33 44 "l" 2)
                                  (make-moving-object 44 33 "r" 2)
                                  (make-moving-object 22 33 "l" 2)))
              (list MOVING1 (make-moving-object 33 44 "l" 2)
                            (make-moving-object 44 33 "r" 2)
                            (make-moving-object 22 33 "l" 2)))                  ;3 enemies on screen, no need to create more

(check-random (make-enemies (list (make-moving-object 77 22 "n" 1)
                                  (make-moving-object 55 33 "l" 2)
                                  (make-moving-object 33 55 "l" 2)))
              (list (make-moving-object (random WIDTH) 0 "r" 2)
                    (make-moving-object 77 22 "n" 1)
                    (make-moving-object 55 33 "l" 2)
                    (make-moving-object 33 55 "l" 2)))                           ; 2 enemies on screen, 1 more is generated


;(define (make-enemies lom) lom)   ;stub

(define (make-enemies lom)
  (cond [(enough-enemies? lom) lom]
        [else
         (make-enemies (cons (make-moving-object (random WIDTH) 0 "r" 2) lom))]))

;; ListOfMoving -> Boolean
;; produce true if there are 3 enemies on the screen
(check-expect (enough-enemies? (list MOVING1)) false)
(check-expect (enough-enemies? (list MOVING1 MOVING2
                                    (make-moving-object 0 3 "r" 2))) false)
(check-expect (enough-enemies? (list (make-moving-object 0 3 "r" 2)
                                      MOVING1
                                     (make-moving-object 16 16 "n" 3)
                                     (make-moving-object 3 0 "l" 2)
                                     (make-moving-object 6 55 "r" 2))) true)
(check-expect (enough-enemies? (list MOVING1 (make-moving-object 16 16 "n" 3)
                                     (make-moving-object 20 20 "n" 3)
                                     (make-moving-object 30 40 "l" 2)
                                     (make-moving-object 50 30 "n" 3)
                                     (make-moving-object 100 100 "r" 2))) false)

;(define (enough-enemies? lom) false)   ;stub

(define (enough-enemies? lom)
  (= 3 (count-enemies lom)))

;; ListOfMoving -> Natural
;; count the number of enemies on the screen.
(check-expect (count-enemies empty) 0)
(check-expect (count-enemies (list MOVING1)) 0)
(check-expect (count-enemies (list MOVING1 MOVING2 MOVING3)) 1)
(check-expect (count-enemies (list (make-moving-object 8 5 "l" 2)
                                    MOVING1
                                   (make-moving-object 100 100 "n" 3)
                                   (make-moving-object 10 15 "r" 2)
                                   (make-moving-object 6 33 "l" 2))) 3)      

;(define (count-enemies lom) 0)   ;stub

(define (count-enemies lom)
  (cond [(empty? lom) 0]
        [else
         (if (= 2 (moving-object-type (first lom)))
             (+ 1 (count-enemies (rest lom)))
             (count-enemies (rest lom)))]))

;; ListOfMoving -> ListOfMoving
;; tick all moving objects, changing their x and y coords depending on their Type and Direction
(check-expect (tick-all empty) empty)
(check-expect (tick-all (list MOVING1)) (list MOVING1))
(check-expect (tick-all (list (make-moving-object 20 30 "r" 2) (make-moving-object 30 20 "l" 2)
                              MOVING1 (make-moving-object 60 60 "n" 3)))
              (list (make-moving-object (+ 20 ENEMY-SPEED) (+ 30 ENEMY-SPEED) "r" 2)                ; enemy moves right and down
                    (make-moving-object (- 30 ENEMY-SPEED) (+ 20 ENEMY-SPEED) "l" 2)                ; enemy moves left and down
                    MOVING1 (make-moving-object 60 (- 60 MISSILE-SPEED) "n" 3)))                    ; player doesn't move, missile goes up
(check-expect (tick-all (list (make-moving-object 30 HEIGHT "l" 1)
                              (make-moving-object WIDTH 20 "r" 2)
                              (make-moving-object 0 40 "l" 2)
                              (make-moving-object ENEMY-SPEED 70 "l" 2)))
              (list (make-moving-object (- 30 PLAYER-SPEED) HEIGHT "l" 1)                          ; player moves left
                    (make-moving-object WIDTH 20 "l" 2)                                            ; enemy changes Direction to left
                    (make-moving-object 0 40 "r" 2)                                                ; enemy changes Direction to right
                    (make-moving-object 0 (+ 70 ENEMY-SPEED) "l" 2)))                              ; enemy reached the left edge
(check-expect (tick-all (list (make-moving-object 30 HEIGHT "r" 1)
                              (make-moving-object (- WIDTH ENEMY-SPEED) 50 "r" 2)
                              (make-moving-object 20 20 "n" 3)))
              (list (make-moving-object (+ 30 PLAYER-SPEED) HEIGHT "r" 1)                          ; player moves right
                    (make-moving-object WIDTH (+ 50 ENEMY-SPEED) "r" 2)                            ; enemy reached the right edge
                    (make-moving-object 20 (- 20 MISSILE-SPEED) "n" 3)))                           ; missile goes "up"

;(define (tick-all lom) lom)   ;stub

; <template from ListOfMoving>

(define (tick-all lom)
  (cond [(empty? lom) empty]
        [else
          (cons (tick-one (first lom))
                (tick-all (rest lom)))]))

;; Moving-object -> Moving-object
;; tick a given moving object, depending on its type and direction
(check-expect (tick-one MOVING1) (tick-player MOVING1))
(check-expect (tick-one MOVING2) (tick-missile MOVING2))
(check-expect (tick-one MOVING3) (tick-enemy MOVING3))

;(define (tick-one m) m)  ;stub


(define (tick-one m)
  (cond [(= 1 (moving-object-type m)) (tick-player m)]
        [(= 2 (moving-object-type m)) (tick-enemy m)]
        [(= 3 (moving-object-type m)) (tick-missile m)]))

;; Moving-object -> Moving-object
;; tick a moving object, given that its Type is 1 (tick a player's tank)
(check-expect (tick-player (make-moving-object 20 HEIGHT "l" 1))
              (move-player-left (make-moving-object 20 HEIGHT "l" 1)))
(check-expect (tick-player (make-moving-object 30 HEIGHT "r" 1))
              (move-player-right (make-moving-object 30 HEIGHT "r" 1)))
(check-expect (tick-player MOVING1) MOVING1)
                         
;(define (tick-player m) m)   ;stub

(define (tick-player m)
  (cond [(string=? "l" (moving-object-dir m)) (move-player-left m)]
        [(string=? "r" (moving-object-dir m)) (move-player-right m)]
        [(string=? "n" (moving-object-dir m)) m]))

;; Moving-object -> Moving-object
;; move a player's tank to the left, or to (1/2 width of the tank) x-coord if it's bumping into the left edge of the screen
(check-expect (move-player-left (make-moving-object 30 HEIGHT "l" 1))
              (make-moving-object (- 30 PLAYER-SPEED) HEIGHT "l" 1))
(check-expect (move-player-left (make-moving-object (+ PLAYER-SPEED HALF-PLAYER-W)
                                                    HEIGHT "l" 1))
              (make-moving-object HALF-PLAYER-W
                                  HEIGHT "l" 1))
(check-expect (move-player-left (make-moving-object (+ HALF-PLAYER-W 1)
                                                    HEIGHT "l" 1))
              (make-moving-object HALF-PLAYER-W HEIGHT "l" 1))                     ; Player tries to move out of the screen, but can't
(check-expect (move-player-left (make-moving-object HALF-PLAYER-W
                                                    HEIGHT "l" 1))
              (make-moving-object HALF-PLAYER-W HEIGHT "l" 1))                     ; Player already at left edge, stays there

;(define (move-player-left m) m)   ;stub

(define (move-player-left m)
  (if (> HALF-PLAYER-W (- (moving-object-x m) PLAYER-SPEED))
      (make-moving-object HALF-PLAYER-W
                          HEIGHT
                          (moving-object-dir m)
                          (moving-object-type m))
      (make-moving-object (- (moving-object-x m) PLAYER-SPEED)
                          HEIGHT
                          (moving-object-dir m)
                          (moving-object-type m))))

;; Moving-object -> Moving-object
;; move a player's tank to the right, or to (WIDTH - 1/2 width of the tank) if it's bumping into the right edge of the screen
(check-expect (move-player-right (make-moving-object 30 HEIGHT "r" 1))
              (make-moving-object (+ 30 PLAYER-SPEED) HEIGHT "r" 1))
(check-expect (move-player-right (make-moving-object (- (- WIDTH HALF-PLAYER-W) 1)
                                                     HEIGHT "r" 1))
              (make-moving-object (- WIDTH HALF-PLAYER-W)
                                  HEIGHT "r" 1))                                 ; Player tries to move out of the screen, but can't
(check-expect (move-player-right (make-moving-object (- WIDTH HALF-PLAYER-W)
                                                     HEIGHT "r" 1))
              (make-moving-object (- WIDTH HALF-PLAYER-W)
                                  HEIGHT "r" 1))                                 ; Player already at right edge, stays there

;(define (move-player-right m) m)   ;stub

(define (move-player-right m)
  (if (< (- WIDTH HALF-PLAYER-W) (+ (moving-object-x m) PLAYER-SPEED))
      (make-moving-object (- WIDTH HALF-PLAYER-W)
                          HEIGHT
                          (moving-object-dir m)
                          (moving-object-type m))
      (make-moving-object (+ (moving-object-x m) PLAYER-SPEED)
                          HEIGHT
                          (moving-object-dir m)
                          (moving-object-type m))))

;; Moving-object -> Moving-object
;; move a missile "up" by MISSILE-SPEED (subtract MISSILE-SPEED from y-coord of the missile)
(check-expect (tick-missile (make-moving-object 20 20 "n" 3))
              (make-moving-object 20 (- 20 MISSILE-SPEED) "n" 3))

;(define (tick-missile m) m)   ;stub

(define (tick-missile m)
  (make-moving-object (moving-object-x m)
                      (- (moving-object-y m) MISSILE-SPEED)
                      (moving-object-dir m)
                      (moving-object-type m)))

;; Moving-object -> Moving-object
;; Tick a given enemy, given its direction, change direction if needed.
(check-expect (tick-enemy (make-moving-object 50 50 "l" 2))
              (make-moving-object (- 50 ENEMY-SPEED) (+ 50 ENEMY-SPEED) "l" 2))
(check-expect (tick-enemy (make-moving-object 50 50 "r" 2))
              (make-moving-object (+ 50 ENEMY-SPEED) (+ 50 ENEMY-SPEED) "r" 2))
(check-expect (tick-enemy (make-moving-object ENEMY-SPEED 50 "l" 2))
              (make-moving-object 0 (+ 50 ENEMY-SPEED) "l" 2))                    ; Enemy reaches the left edge
(check-expect (tick-enemy (make-moving-object (- WIDTH ENEMY-SPEED) 50 "r" 2))
              (make-moving-object WIDTH (+ 50 ENEMY-SPEED) "r" 2))                ; Enemy reaches the right edge
(check-expect (tick-enemy (make-moving-object 0 50 "l" 2))
              (make-moving-object 0 50 "r" 2))                                    ; Switch direction to right
(check-expect (tick-enemy (make-moving-object WIDTH 50 "r" 2))
              (make-moving-object WIDTH 50 "l" 2))                                ; Switch direction to left

;(define (tick-enemy m) m)   ;stub

(define (tick-enemy m)
  (cond [(change-dir? m) (change-dir m)]
        [else
         (if (string=? "l" (moving-object-dir m))
             (make-moving-object (- (moving-object-x m) ENEMY-SPEED)
                                 (+ (moving-object-y m) ENEMY-SPEED)
                                 "l" 2)
             (make-moving-object (+ (moving-object-x m) ENEMY-SPEED)
                                 (+ (moving-object-y m) ENEMY-SPEED)
                                 "r" 2))]))

;; Moving-object -> Boolean
;; produce true if enemy needs to change direction (it moves right AND its (x-dir + its speed) > WIDTH
;;                                              OR (it moves left AND its (x-dir - its speed) < 0)
(check-expect (change-dir? (make-moving-object 30 30 "l" 2)) false)
(check-expect (change-dir? (make-moving-object 30 30 "r" 2)) false)
(check-expect (change-dir? (make-moving-object ENEMY-SPEED 40 "l" 2)) false)
(check-expect (change-dir? (make-moving-object (- WIDTH ENEMY-SPEED) 40 "r" 2)) false)
(check-expect (change-dir? (make-moving-object 0 30 "l" 2)) true)
(check-expect (change-dir? (make-moving-object WIDTH 20 "r" 2)) true)
(check-expect (change-dir? (make-moving-object 0 30 "r" 2)) false)
(check-expect (change-dir? (make-moving-object WIDTH 30 "l" 2)) false)
 
;(define (change-dir? m) false)   ;stub

(define (change-dir? m)
  (or (and (> (+ (moving-object-x m) ENEMY-SPEED) WIDTH)
           (string=? "r" (moving-object-dir m)))
      (and (< (- (moving-object-x m) ENEMY-SPEED) 0)
           (string=? "l" (moving-object-dir m)))))

;; Moving-object -> Moving-object
;; change Direction of a given enemy spaceship without changing anything else.
(check-expect (change-dir (make-moving-object 0 30 "l" 2))
              (make-moving-object 0 30 "r" 2))
(check-expect (change-dir (make-moving-object WIDTH 20 "r" 2))
              (make-moving-object WIDTH 20 "l" 2))

;(define (change-dir m) m)   ;stub

(define (change-dir m)
  (cond [(string=? "l" (moving-object-dir m))
         (make-moving-object (moving-object-x m) (moving-object-y m)
                             "r" 2)]
        [(string=? "r" (moving-object-dir m))
         (make-moving-object (moving-object-x m) (moving-object-y m)
                             "l" 2)]))

;; ListOfMoving -> ListOfMoving
;; produce a list of moving objects with missiles and enemy spaceships that are onscreen? and not dead?
(check-expect (onscreen-only empty) empty)
(check-expect (onscreen-only (list MOVING1)) (list MOVING1))
(check-expect (onscreen-only (list MOVING1 MOVING2 MOVING3))
              (list MOVING1 MOVING3 MOVING2))
(check-expect (onscreen-only (list (make-moving-object 10 0 "n" 3)
                                   MOVING1))
              (list MOVING1 (make-moving-object 10 0 "n" 3)))                ; Missile still on screen
(check-expect (onscreen-only (list MOVING2 MOVING1 MOVING3
                                   (make-moving-object 10 -1 "n" 3)))
              (list MOVING1 MOVING3 MOVING2))                                ; one missile gets filtered out
(check-expect (onscreen-only (list MOVING1
                                   (make-moving-object 30 30 "l" 2)
                                   (make-moving-object 15 24 "n" 3)
                                   (make-moving-object 100 100 "r" 2)))
              (list MOVING1 (make-moving-object 100 100 "r" 2)
                    (make-moving-object 15 24 "n" 3)))                      ; one enemy got hit by missile, filtered out               

;(define (onscreen-only lom) lom)   ;stub

(define (onscreen-only lom)
  (cond [(empty? lom) empty]
        [else
         (if (and (onscreen? (first (sort-by-type lom)))
                  (not (dead? (first (sort-by-type lom))
                              (rest (sort-by-type lom)))))
             (cons (first (sort-by-type lom)) (onscreen-only (rest (sort-by-type lom))))
             (onscreen-only (rest (sort-by-type lom))))]))

;; Moving-object -> Boolean
;; produce true if a given moving object is on screen (its y-coord is >= 0)
(check-random (onscreen? (make-moving-object (random WIDTH)
                                             0 "r" 2)) true)                ; Enemy just generated, considered onscreen
(check-expect (onscreen? (make-moving-object 10 100 "n" 3)) true)
(check-expect (onscreen? MOVING1) true)
(check-expect (onscreen? (make-moving-object 20 0 "n" 3)) true)
(check-expect (onscreen? (make-moving-object 10 -1 "n" 3)) false)

;(define (onscreen? m) false)   ;stub

(define (onscreen? m)
  (>= (moving-object-y m) 0))

;; Moving-Object ListOfMoving -> Boolean
;; produce true if a moving object is 'dead' (when missile hits enemy, enemy is 'dead')
;; ASSUME the list is sorted by type
(check-expect (dead? MOVING1 empty) false)
(check-expect (dead? (make-moving-object 10 HEIGHT "l" 1)
                     (list (make-moving-object 10 5 "r" 2)
                           (make-moving-object 12 5 "n" 3))) false)          ; Player is never "dead"
(check-expect (dead? (make-moving-object 100 100 "l" 2)
                     (list (make-moving-object CTR-X HEIGHT "n" 1)
                           (make-moving-object 50 50 "n" 3)
                           (make-moving-object 110 110 "r" 2)
                           (make-moving-object 20 30 "n" 3)
                           (make-moving-object 100 100 "n" 3))) true)        ; the last missile kills the enemy
(check-expect (dead? (make-moving-object 100 100 "n" 3)
                     (list (make-moving-object 100 100 "n" 3))) false)       ; missiles don't kill each other
(check-expect (dead? (make-moving-object 100 100 "l" 2)
                     (list (make-moving-object 100 100 "r" 2))) false)       ; enemies don't kill each other
(check-expect (dead? (make-moving-object 10 10 "n" 3)
                     (list (make-moving-object 10 10 "n" 1))) false)         ; player doesn't kill a missile
(check-expect (dead? (make-moving-object 10 10 "l" 2)
                     (list (make-moving-object 10 10 "n" 1))) false)         ; player doesn't kill enemy spaceship
  

;; NOTE: this function operates on a sorted list, so
;; when enemy gets hit by missile, enemy is filtered out
;; but missile stays onscreen and "pierces through" the enemy -
;; this is the best consistent version I could come up with.

;(define (dead? m lom) false)   ;stub

(define (dead? m lom)
  (cond [(or (empty? lom)
             (= 1 (moving-object-type m))) false]
        [(or (= 1 (moving-object-type (first lom)))
             (= (moving-object-type m) (moving-object-type (first lom)))
             (not (hit? m (first lom))))
         (dead? m (rest lom))]
        [else true]))

;; Moving-object Moving-object -> Boolean
;; produce true if a missile and an enemy spaceship collide
(check-expect (hit? (make-moving-object 10 20 "n" 3)
                    (make-moving-object 27 28 "l" 2)) true)
(check-expect (hit? (make-moving-object 30 30 "n" 3)
                    (make-moving-object 13 38 "l" 2)) true)
(check-expect (hit? (make-moving-object 30 30 "n" 3)
                    (make-moving-object 13 22 "l" 2)) true)
(check-expect (hit? (make-moving-object 30 30 "n" 3)
                    (make-moving-object 47 22 "l" 2)) true)
(check-expect (hit? (make-moving-object 30 30 "n" 3)
                    (make-moving-object 50 30 "l" 2)) false)
(check-expect (hit? (make-moving-object 30 30 "n" 3)
                    (make-moving-object 30 45 "l" 2)) false)
(check-expect (hit? (make-moving-object 27 28 "l" 2)
                    (make-moving-object 10 20 "n" 3)) true)
(check-expect (hit? (make-moving-object 13 38 "l" 2)
                    (make-moving-object 30 30 "n" 3)) true)
(check-expect (hit? (make-moving-object 13 22 "l" 2)
                    (make-moving-object 30 30 "n" 3)) true)
(check-expect (hit? (make-moving-object 47 22 "l" 2)
                    (make-moving-object 30 30 "n" 3)) true)
(check-expect (hit? (make-moving-object 50 30 "l" 2)
                    (make-moving-object 30 30 "n" 3)) false)
(check-expect (hit? (make-moving-object 30 45 "l" 2)
                    (make-moving-object 30 30 "n" 3)) false)

;(define (hit? m1 m2) false)   ;stub

(define (hit? m1 m2)
  (cond [(= 2 (moving-object-type m2))
         (and (>= (moving-object-x m1)
                  (- (moving-object-x m2) (+ HALF-ENEMY-W HALF-MISSILE-W)))
              (<= (moving-object-x m1)
                  (+ (moving-object-x m2) (+ HALF-ENEMY-W HALF-MISSILE-W)))
              (<= (moving-object-y m1)
                  (+ (moving-object-y m2) HALF-ENEMY-H HALF-MISSILE-H))
              (>= (moving-object-y m1)
                  (- (moving-object-y m2) (+ HALF-ENEMY-H HALF-MISSILE-H))))]
        [(= 3 (moving-object-type m2))
         (and (>= (moving-object-x m2)
                  (- (moving-object-x m1) (+ HALF-ENEMY-W HALF-MISSILE-W)))
              (<= (moving-object-x m2)
                  (+ (moving-object-x m1) (+ HALF-ENEMY-W HALF-MISSILE-W)))
              (<= (moving-object-y m2)
                  (+ (moving-object-y m1) (+ HALF-ENEMY-H HALF-MISSILE-H)))
              (>= (moving-object-y m2)
                  (- (moving-object-y m1) (+ HALF-ENEMY-H HALF-MISSILE-H))))]))
        
         
;; ListOfMoving -> Image
;; render a  list of moving objects onto the screen
(check-expect (render-all empty) MTS)
(check-expect (render-all (list (make-moving-object CTR-X HEIGHT "n" 1)))
              (place-image/align PLAYER-IMG CTR-X HEIGHT
                                 "center"
                                 "bottom" MTS))
(check-expect (render-all (list (make-moving-object CTR-X HEIGHT "l" 1)
                                (make-moving-object 75 100 "n" 3)))
              (place-image/align PLAYER-IMG CTR-X HEIGHT
                                 "center"
                                 "bottom"
                                 (place-image/align MISSILE-IMG 75 100
                                                    "center"
                                                    "bottom" MTS)))
(check-expect (render-all (list (make-moving-object 33 55 "l" 2)
                                (make-moving-object 55 33 "n" 3)
                                (make-moving-object CTR-X HEIGHT "r" 1)))
              (place-image/align ENEMY-IMG 33 55
                                 "center"
                                 "bottom"
                                 (place-image/align MISSILE-IMG 55 33
                                                    "center"
                                                    "bottom"
                                                    (place-image/align PLAYER-IMG
                                                                       CTR-X
                                                                       HEIGHT
                                                                       "center"
                                                                       "bottom" MTS))))


;(define (render-all lom) MTS)   ;stub

(define (render-all lom)
  (cond [(empty? lom) MTS]
        [else
         (render-on (first lom) (render-all (rest lom)))]))

;; Moving-object Image -> Image
;; render a moving object on the given image
(check-expect (render-on (make-moving-object CTR-X HEIGHT "l" 1) MTS)
              (place-image/align PLAYER-IMG CTR-X HEIGHT "center" "bottom" MTS))
 
;(define (render-on m img) MTS)   ;stub

(define (render-on m img)
  (place-image/align (choose-image m) (moving-object-x m)
               (moving-object-y m) "center" "bottom" img))

;; Moving-object -> Image
;; Choose appropriate image depending on the Type of the given Moving-object
(check-expect (choose-image (make-moving-object CTR-X HEIGHT "n" 1)) PLAYER-IMG)
(check-expect (choose-image (make-moving-object 75 62 "n" 3)) MISSILE-IMG)
(check-expect (choose-image (make-moving-object 44 33 "r" 2)) ENEMY-IMG)

;(define (choose-image m) empty-image)  ;stub

(define (choose-image m)
  (cond [(= 1 (moving-object-type m)) PLAYER-IMG]
        [(= 3 (moving-object-type m)) MISSILE-IMG]
        [(= 2 (moving-object-type m)) ENEMY-IMG]))

;; ListOfMoving KeyEvent -> ListOfMoving
;; move player's tank left or right when left or right arrows are clicked; shoot missiles when space bar is clicked.
(check-expect (control-tank (list (make-moving-object CTR-X HEIGHT "n" 1)) "left")
              (list (make-moving-object CTR-X HEIGHT "l" 1)))
(check-expect (control-tank (list (make-moving-object 55 HEIGHT "l" 1)) "right")
              (list (make-moving-object 55 HEIGHT "r" 1)))
(check-expect (control-tank (list (make-moving-object 73 HEIGHT "l" 1)) "b")
              (list (make-moving-object 73 HEIGHT "l" 1)))
(check-expect (control-tank (list (make-moving-object 75 75 "n" 3)
                                  (make-moving-object 32 32 "n" 3)
                                  (make-moving-object CTR-X HEIGHT "r" 1)
                                  (make-moving-object 33 22 "r" 2)) "left")
              (list (make-moving-object CTR-X HEIGHT "l" 1)
                    (make-moving-object 33 22 "r" 2)
                    (make-moving-object 75 75 "n" 3)
                    (make-moving-object 32 32 "n" 3)))     
(check-expect (control-tank (list (make-moving-object 73 HEIGHT "n" 1)) " ")
              (list (make-moving-object 73 (- HEIGHT 20) "n" 3)
                    (make-moving-object 73 HEIGHT "n" 1)))
 
;(define (control-tank lom me) lom)   ;stub

(define (control-tank lom me)
  (cond [(key=? "left" me)
         (cons (switch-dir-to-left (first (sort-by-type lom)))
               (rest (sort-by-type lom)))]
        [(key=? "right" me)
         (cons (switch-dir-to-right (first (sort-by-type lom)))
               (rest (sort-by-type lom)))]
        [(key=? " " me)
         (cons (make-moving-object (moving-object-x (first (sort-by-type lom)))
                                   (- HEIGHT 20) "n" 3) lom)]
        [else lom]))

;; ListOfMoving -> ListOfMoving
;; sort moving objects in a list by type: player's tank first, then enemies, then missiles.
(check-expect (sort-by-type empty) empty)
(check-expect (sort-by-type (list (make-moving-object 22 HEIGHT "n" 1)
                                  (make-moving-object 33 44 "n" 3)
                                  (make-moving-object 44 33 "n" 3)))
              (list (make-moving-object 22 HEIGHT "n" 1)
                    (make-moving-object 33 44 "n" 3)
                    (make-moving-object 44 33 "n" 3)))                        ; list already sorted, no changes
(check-expect (sort-by-type (list (make-moving-object 33 44 "n" 3)
                                  (make-moving-object 22 HEIGHT "n" 1)
                                  (make-moving-object 44 33 "n" 3)))
              (list (make-moving-object 22 HEIGHT "n" 1)
                    (make-moving-object 33 44 "n" 3)
                    (make-moving-object 44 33 "n" 3)))
(check-expect (sort-by-type (list (make-moving-object 33 44 "n" 3)
                                  (make-moving-object 44 33 "n" 3)
                                  (make-moving-object CTR-X HEIGHT "n" 1)))
              (list (make-moving-object CTR-X HEIGHT "n" 1)
                    (make-moving-object 33 44 "n" 3)
                    (make-moving-object 44 33 "n" 3)))
(check-expect (sort-by-type (list (make-moving-object 22 33 "n" 3)
                                  (make-moving-object 22 33 "r" 2)
                                  (make-moving-object 33 22 "l" 2)
                                  (make-moving-object 33 HEIGHT "l" 1)))
              (list (make-moving-object 33 HEIGHT "l" 1)
                    (make-moving-object 22 33 "r" 2)
                    (make-moving-object 33 22 "l" 2)
                    (make-moving-object 22 33 "n" 3)))      

;(define (sort-by-type lom) lom)   ;stub

(define (sort-by-type lom)
  (cond [(empty? lom) empty]
        [else
         (insert (first lom) (sort-by-type (rest lom)))]))

;; Moving-object ListOfMoivng -> ListOfMoving
;; insert a given moving object in the right place in the list, type 1 comes before type 2 which comes before type 3.
(check-expect (insert (make-moving-object CTR-X HEIGHT "n" 1)
                      (list (make-moving-object 22 33 "n" 3)
                            (make-moving-object 33 22 "n" 3)))
              (list (make-moving-object CTR-X HEIGHT "n" 1)
                    (make-moving-object 22 33 "n" 3)
                    (make-moving-object 33 22 "n" 3)))
(check-expect (insert (make-moving-object 22 33 "n" 3)
                      (list (make-moving-object CTR-X HEIGHT "l" 1)
                            (make-moving-object 33 22 "n" 3)))
              (list (make-moving-object CTR-X HEIGHT "l" 1)
                    (make-moving-object 22 33 "n" 3)
                    (make-moving-object 33 22 "n" 3)))
(check-expect (insert (make-moving-object 20 30 "l" 2)
                      (list (make-moving-object CTR-X HEIGHT "n" 1)
                            (make-moving-object 30 20 "r" 2)
                            (make-moving-object 40 30 "n" 3)))
              (list (make-moving-object CTR-X HEIGHT "n" 1)
                    (make-moving-object 20 30 "l" 2)
                    (make-moving-object 30 20 "r" 2)
                    (make-moving-object 40 30 "n" 3)))
(check-expect (insert (make-moving-object 30 30 "n" 3)
                      (list MOVING1 MOVING3
                            (make-moving-object 20 20 "l" 2)
                            (make-moving-object 40 40 "n" 3)))
              (list MOVING1 MOVING3
                    (make-moving-object 20 20 "l" 2)
                    (make-moving-object 30 30 "n" 3)
                    (make-moving-object 40 40 "n" 3)))

;(define (insert m lom) lom)   ;stub

(define (insert m lom)
  (cond [(empty? lom) (cons m empty)]
        [else
         (if (> (moving-object-type m) (moving-object-type (first lom)))
             (cons (first lom) (insert m (rest lom)))
             (cons m lom))]))

;; Moving-object -> Moving-object
;; Switches direction of a player's tank to "l"eft
(check-expect (switch-dir-to-left (make-moving-object CTR-X HEIGHT "n" 1))
              (make-moving-object CTR-X HEIGHT "l" 1))

;(define (switch-dir-to-left m) m)   ;stub

(define (switch-dir-to-left m)
  (make-moving-object (moving-object-x m)
                      (moving-object-y m)
                      "l"
                      (moving-object-type m)))

;; Moving-object -> Moving-object
;; Switches direction of a player's tank to "r"ight
(check-expect (switch-dir-to-right (make-moving-object CTR-X HEIGHT "n" 1))
              (make-moving-object CTR-X HEIGHT "r" 1))

;(define (switch-dir-to-right m) m)   ;stub

(define (switch-dir-to-right m)
  (make-moving-object (moving-object-x m)
                      (moving-object-y m)
                      "r"
                      (moving-object-type m)))

;; ListOfMoving -> Boolean
;; stop the game when any of the enemies reaches the bottom of the screen
(check-expect (end? empty) false)
(check-expect (end? (list (make-moving-object 0 55 "l" 2)
                          (make-moving-object 55 0 "r" 2)
                          (make-moving-object 77 (- HEIGHT ENEMY-SPEED) "r" 2))) false)
(check-expect (end? (list (make-moving-object 55 0 "r" 2)
                          (make-moving-object 77 (+ HEIGHT 1) "r" 2)
                          (make-moving-object 0 33 "l" 2))) true)
(check-expect (end? (list (make-moving-object 33 0 "r" 2)
                          (make-moving-object 77 77 "l" 2)
                          (make-moving-object 32 (+ HEIGHT 2) "l" 2))) true)      

;(define (end? lom) false)   ;stub

(define (end? lom)
  (cond [(empty? lom) false]
        [(false? (lower? (first lom)))
         (end? (rest lom))]       
        [else true]))

;; Moving-Object -> Boolean
;; produce true if object's y coord is > HEIGHT
(check-expect (lower? (make-moving-object 77 HEIGHT "l" 2)) false)
(check-expect (lower? (make-moving-object 33 (+ HEIGHT 1) "r" 2)) true)

;(define (lower? m) false)   ;stub

(define (lower? m)
  (> (moving-object-y m) HEIGHT))

;; ListOfMoving -> Image
;; Render "GAME OVER" onto the screen with the final world state.
(check-expect (last-picture (list (make-moving-object 33 55 "l" 2)
                                  (make-moving-object 77 (+ HEIGHT 1) "r" 2)))
              (place-image GAME-OVER-IMG CTR-X CTR-Y (render-all
                                                     (list (make-moving-object 33 55 "l" 2)
                                                           (make-moving-object 77 (+ HEIGHT 1) "r" 2)))))      

;(define (last-picture lom) MTS)   ;stub

(define (last-picture lom)
  (place-image GAME-OVER-IMG CTR-X CTR-Y (render-all lom)))
