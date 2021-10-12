#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require 2htdp/batch-io)
(require lang/posn)
(require "image.rkt")



;;; Symbol constants

;; all the speed value can get the real time value according to this value
;; value/FLASH-SPEED seconds
;; which means 
(define BASE-MOVE 3)
(define BASE-MOVE-BULLET 10)
; move how many pixels in a trigger
(define FLASH-SPEED 200)
(define LEVEL-TIME 8)
; how much time it need to level up

; how many picture this programme produces during one second
(define PLAYER-BULLET-RANGE 1)
(define BULLET-PRODUCE-SPEED 8)
(define BULLET-MOVE-SPEED 1)
(define MONSTER-PRODUCE-SPEED 2)
(define MONSTER-SIZE 50)
(define MONSTER-COLOR-RANGE 255)
(define MONSTER-PRODUCE-Y -50)
(define MONSTER-MOVE-SPEED 6)
(define ROTATE-RADIUS 50)
(define MONSTER-BASE-SCORE 16)

;; socre record file
(define RECORD-FILE "score.txt")

;; monster type
(define MONSTER-TYPE-RANGE 7)
(define SQUARE 1)
(define CIRCLE 2)
(define TRIANGLE 3)
(define STAR 4)
(define RHOMBUS 5)
(define RADIAL-STAR 6)
(define PULLED-REGULAR-POLYGON 7)

;; orientation
(define LEFT 0)
(define RIGHT 1)

;;; Struct definition
(struct posn (x y) #:transparent)

(struct player (x y vel-x vel-y hp name left right up down bullets
                  orientation make-bullet? bullet-time))
; player descripes what attributes the player in the game have
; (player Number Number Number Number Number String)
; x is x-coordinate
; y is y-coordinate
; vel-x is horizontal velocity
; vel-y is vertical velocity
; hp is hit point
; name is the string input by the player

(struct player-bullet (x y vel-x vel-y damage lv))
; (bullet Number Number Number Number Number)
; x is x-coordinate
; y is y-coordinate
; vel-y is vertical velocity
; damage determines how much damage a bullet can deal
; lv descripes what a bullet looks like

(struct monster (origin final vel-x vel-y hp type col size orbit rotate))
; (monster Posn Posn Number Number Number Number Number Number)
; vel-x vel-y hp are the same as player struct
; origin is the start position of monster
; final are used to determine its position in the world with specific orbit
; type decides what a monster looks like and how much hp it has
; orbit decides how a monster move
; col decides the color of the monster


(struct board (x y width height col))


(struct display-state (lv1 lv2 lv3))
; display-state determines what the display mode is according to its lv
; (display-state Number Number Number)

(struct produce-speed (bullet monster monster-bullet))

(struct world (player monsters player-bullets display-state world-time score
                      produce-speed score-record playerone playertwo boards
                      monster-players bullets score1))
; (world Player List-of-Monster List-of-Player-bullets Display-state Number)




;;; Initial state
(define Player (player (/ BCK-WIDTH 2) (* BCK-HEIGHT 19/20) 5 5 3 "" #f #f #f #f '()
                       LEFT #f 0))
(define Monsters '())
(define Player-bullets '())
(define Display-state (display-state 1 1 1))
(define World-time 1)
(define Score 0)
(define Score1 0)
(define Produce-speed (produce-speed BULLET-PRODUCE-SPEED MONSTER-PRODUCE-SPEED 0))
(define Score-record (read-words/line
                      (if (file-exists? RECORD-FILE)
                          RECORD-FILE
                          (write-file RECORD-FILE " "))))
(define Playerone (player (* WORLD-WIDTH 1/10) (- WORLD-HEIGHT (/ PLAYER-HEIGHT 2))
                          8 0 5 "playerone" #f #f #f #f '() RIGHT #f 0))
(define Playertwo (player (* WORLD-WIDTH 9/10) (- WORLD-HEIGHT (/ PLAYER-HEIGHT 2))
                          8 0 5 "playertwo" #f #f #f #f '() LEFT #f 0))
(define Monster-player '())
(define Bullets '())
(define Boards `(,(board 150 (* WORLD-HEIGHT 1/4) 180 20 0)
                 ,(board (- WORLD-WIDTH 150) (* WORLD-HEIGHT 1/4) 180 20 0)
                 ,(board 150 (* WORLD-HEIGHT 13/16) 180 20 0)
                 ,(board (- WORLD-WIDTH 150) (* WORLD-HEIGHT 13/16) 180 20 0)
                 ,(board (/ WORLD-WIDTH 2) (* WORLD-HEIGHT 3/16) 500 20 0)
                 ,(board (* WORLD-WIDTH 3/8) (* WORLD-HEIGHT 3/8) 180 20 0)
                 ,(board (* WORLD-WIDTH 5/16) (* WORLD-HEIGHT 5/8) 180 20 0)
                 ,(board (* WORLD-WIDTH 11/16) (* WORLD-HEIGHT 3/8) 180 20 0)
                 ,(board (* WORLD-WIDTH 5/8) (* WORLD-HEIGHT 5/8) 180 20 0)
                 ,(board (/ WORLD-WIDTH 2) (* WORLD-HEIGHT 13/16) 500 20 0)
                 ))
(define Boards-challenge
  `(,(board 150 (* WORLD-HEIGHT 1/4) 250 20 0)
    ,(board (- WORLD-WIDTH 150) (* WORLD-HEIGHT 1/4) 250 20 0)
    ,(board 150 (* WORLD-HEIGHT 13/16) 250 20 0)
    ,(board (- WORLD-WIDTH 150) (* WORLD-HEIGHT 13/16) 250 20 0)
    ,(board (/ WORLD-WIDTH 2) (* WORLD-HEIGHT 3/16) 500 20 0)
    ,(board (* WORLD-WIDTH 1/4) (* WORLD-HEIGHT 3/8) 400 20 0)
    ,(board (* WORLD-WIDTH 3/16) (* WORLD-HEIGHT 5/8) 400 20 0)
    ,(board (* WORLD-WIDTH 12/16) (* WORLD-HEIGHT 3/8) 400 20 0)
    ,(board (* WORLD-WIDTH 11/16) (* WORLD-HEIGHT 5/8) 400 20 0)
    ,(board (/ WORLD-WIDTH 2) (* WORLD-HEIGHT 13/16) 500 20 0)
     ))
  

(define World (world Player Monsters Player-bullets Display-state World-time Score
                     Produce-speed Score-record Playerone Playertwo
                     Boards Monster-player Bullets Score1))




;;; Handler function
(define (render ws)
  (local ((define pl-b (world-player-bullets ws))
          (define pl (world-player ws))
          (define mons (world-monsters ws))
          (define wtime (world-world-time ws))
          (define score (world-score ws))
          (define stl-1 (display-state-lv1 (world-display-state ws)))
          (define stl-2 (display-state-lv2 (world-display-state ws)))
          (define stl-3 (display-state-lv3 (world-display-state ws)))
          (define hp (player-hp pl))
          (define los (world-score-record ws))
          (define los1 (if (empty? los) '(" ") (first los)))
          (define los2 (if (empty? (rest los)) '(" ") (first (rest los))))
          (define pl1 (world-playerone ws))
          (define pl2 (world-playertwo ws))
          (define pl1-bullets (player-bullets pl1))
          (define pl2-bullets (player-bullets pl2))
          (define boards (world-boards ws))
          (define mps (world-monster-players ws))
          (define bullets (world-bullets ws))
          (define pl1-b (player-bullets pl1))
          (define score1 (world-score1 ws))
          

         
          
          (define (render-player player background)
            (place-image PLAYER
                         (player-x player) (player-y player)
                         background))
          (define (render-player-bullets pbs background)
            (foldr (lambda (x ima)
                     (if (not (empty? ima))
                         (place-image (bullet-lv-to-img (player-bullet-lv x))
                                      (player-bullet-x x)
                                      (player-bullet-y x)
                                      ima)
                         ima))
                   background pbs))
          (define (render-bullets bullets ima)
            (render-player-bullets bullets ima))
          (define (render-hp hp ima)
            (place-image/align (hp->image hp)
                               5 (- BCK-HEIGHT 20)
                               "left" "center"
                         ima))
          (define (hp->image hp)
            (if (zero? hp)(square 1 "solid" (make-color 0 0 0 0))
                (beside heart (square 5 "solid" (make-color 0 0 0 0))
                        (hp->image (sub1 hp)))))
          (define (playerone-which-ima? play)
            (local ((define ori (player-orientation play)))
              (if (= ori LEFT) PLAYERONE-LEFT PLAYERONE-RIGHT)))
          (define (playertwo-which-ima? play)
            (local ((define ori (player-orientation play)))
              (if (= ori LEFT) PLAYERTWO-LEFT PLAYERTWO-RIGHT)))
          (define (render-playerone player ima)
            (place-image (render-player-hp player
                                           (playerone-which-ima? player) eye-col)
                         (player-x player) (- (player-y player) (/ HP-TEXT-HEIGHT 2))
                         ima))
          (define (render-playertwo player ima)
            (place-image (render-player-hp player
                                           (playertwo-which-ima? player) eye-col2)
                         (player-x player) (- (player-y player) (/ HP-TEXT-HEIGHT 2))
                         ima))
          
          (define (render-mps mps ima)
            (if (empty? mps) ima
                (foldl (lambda (x ima) (render-playertwo x ima)) ima mps)))
          
          (define (render-boards boards ima)
            (foldr (lambda (x bck)
                     (if (not (empty? bck))
                         (place-image (board->image x)
                                      (board-x x)
                                      (board-y x)
                                      bck)
                         ima))
                   ima boards))
          (define (board->image board)
            (make-rec (board-width board) (board-height board)
                      eye-col))
          (define (render-player-hp play ima col)
            (above (make-hp-text (player-hp play) col)
                   ima))
          
                      
       
                         

          (define (render-score score ima)
            (place-image (text (string-append "score:"
                                              (number->string score))
                                              14 "yellow")
                         (/ BCK-WIDTH 2) (/ BCK-HEIGHT 2)
                         ima))
          (define (render-score1 score1 ima)
            (place-image (text (string-append "击杀:"
                                              (number->string score1))
                                              20 "white")
                         50 25 ima))
          
          (define (rotate-monster mon)
            (if (= 1 (monster-rotate mon))
                (rotate wtime
                        (monster->image mon))
                (monster->image mon)))
          (define (assign-hp mon)
            (local ((define hp (monster-hp mon))
                    (define texthp
                      (text (number->string hp) 10 "white")))
              (if (> hp 1)
                  (overlay/offset texthp
                              0 0
                              (rotate-monster mon))
                  (rotate-monster mon))))
          (define (render-monsters mons bac)
            (foldr (lambda (mons ima)
                     (place-image (assign-hp mons)
                                  (posn-x (monster-final mons))
                                  (posn-y (monster-final mons))
                                  ima))
                   bac mons))
          (define (render-main-menu-option lv2)
            (local ((define (make-menu-list lst lv)
                      (if (= lv 1)
                          (cons (make-menu (first lst) menu-bck-1)
                                (rest lst))
                          (cons (first lst) (make-menu-list (rest lst) (sub1 lv)))))
                    (define new-menu-list (make-menu-list text-list lv2))
                    (define (above-ima lst)
                      (if (empty? (rest lst)) (first lst)
                          (above (first lst) (above-ima (rest lst))))))
              (above-ima new-menu-list)))
          (define bck-with-head
            (place-image (scale/xy 3 3 PLAYER)
                         (/ BCK-WIDTH 2) 200
                         bck-origin0))
          (define (render-main-menu lv2)
            (overlay/offset (render-main-menu-option lv2)
                            0 0
                            bck-with-head))
          (define (render-pause lv3 ima)
            (if (= lv3 1)
                (overlay/offset pause-1
                                0 0
                                ima)
                (overlay/offset pause-2
                                0 0
                                ima)))
          (define (render-image above down)
            (overlay/offset above 0 0 down))
          (define (render-score-record lst n)
                   (if (or (empty? lst) (= n 0))
                       (square 1 "solid" (make-color 0 0 0 0))
                       (above (text (first lst)
                                    (+ 12 (* 3 n)) "yellow")
                              (render-score-record (rest lst) (sub1 n)))))
            
          (define menu-ima (render-main-menu stl-2))

          (define base-game-image
            (render-player-bullets pl-b
                                   (render-player pl
                                                  (render-monsters mons bck-origin0))))
          (define base-vs-image
            (render-player-bullets
             pl1-bullets
             (render-player-bullets
              pl2-bullets
              (render-playerone
               pl1
               (render-playertwo
                pl2
                (render-boards
                 boards pvp-bck))))))
          (define base-challenge-image
            (render-score1
             score1
             (render-player-bullets
               pl1-bullets
               (render-bullets
                 bullets
                 (render-playerone
                   pl1
                   (render-mps
                     mps
                     (render-boards
                       boards pvp-bck)))))))
                           
            
          
          (define game-image-1
            (render-hp hp
                       (render-score score base-game-image)))
          (define game-image-2 pvp-bck)
          (define game-pause (render-pause stl-3 base-game-image))
          (define pvp-game-pause (render-pause stl-3 base-vs-image))
          (define challenge-game-pause (render-pause stl-3 base-challenge-image))
          (define score-record-ima
            (above (text "消方最高分" 25 "white")
                   (square 10 "solid" (make-color 0 0 0 0))
                   (render-score-record los1 6)))
          (define score1-record-ima
            (above (text "挑战最高分" 25 "white")
                   (square 10 "solid" (make-color 0 0 0 0))
                   (render-score-record los2 6)))
          (define total-score-record-ima
            (beside score-record-ima
                    (square 10 "solid" (make-color 0 0 0 0))
                    score1-record-ima))
            

          (define (render-total ima)
            (overlay/offset ima
                            0 0
                            bck-origin))
            

          (define (render-which? lv1 lv2 lv3)
            (cond
              [(= 1 lv1) (render-total menu-ima)]
              [(= 2 lv1)
               (render-total
                (cond
                 [(= 1 lv2) game-image-1]
                 [(= 2 lv2) game-pause]
                 [(= 3 lv2) (render-image game-over-origin game-image-1)]))]
              [(= 4 lv1) (render-total total-score-record-ima)]
              [(= 3 lv1)
               (cond
                 [(= 1 lv2) base-vs-image]
                 [(= 2 lv2) pvp-game-pause]
                 [(= 3 lv2) (render-image game-over-origin base-vs-image)]
                 [else base-vs-image])]
              [(= 6 lv1)
               (cond
                 [(= 1 lv2) base-challenge-image]
                 [(= 2 lv2) challenge-game-pause]
                 [(= 3 lv2) (render-image game-over-origin base-challenge-image)]
                 [else base-challenge-image])]
              [(= 5 lv1) (render-total readme)])))
    (render-which? stl-1 stl-2 stl-3)))

(define (bullet-lv-to-img lv)
           (cond
             [(= lv 1) bullet-v33]
             [(= lv 2) bullet-v3]
             [(= lv 3) bullet-v44]))

; Player-bullet -> Number
; get the size of the bullet
(define (get-bullet-size bullet)
  (image-width (bullet-lv-to-img (player-bullet-lv bullet))))

; Monster -> Ima
; according to the monster's attributes, produces its image
(define (monster->image monster)
  (local ((define ms (monster-size monster))
          (define mt (monster-type monster))
          (define mc (monster-col monster))
          (define hp (monster-hp monster))
          ; number -> function
          ; according to the number, decides which type of gigure to produce
          (define (which-type? type)
            (cond
              [(= SQUARE type) (lambda (x y z)
                            (square x y z))]
              [(= CIRCLE type) (lambda (x y z)
                            (circle x y z))]
              [(= STAR type) (lambda (x y z)
                            (star x y z))]
              [(= TRIANGLE type) (lambda (x y z)
                                   (triangle x y z))]
              [(= RHOMBUS type) (lambda (x y z)
                                  (rhombus x (* 2 x) y z))]
              [(= RADIAL-STAR type) (lambda (x y z)
                                      (radial-star (ceiling (* x 1/3))
                                                   (* x 1/3) (* x 2/3) y z))]
              [(= PULLED-REGULAR-POLYGON type)
               (lambda (x y z)
                 (pulled-regular-polygon (* x 2/3)
                                         (+ 7 (modulo (floor x) 4))
                                         (* (+ 15 (modulo (floor x) 10)) 0.1)
                                         (+ 80 (floor x)) y z))]))
          (define (which-size? size)
            (/ MONSTER-SIZE size))
          (define (which-color? col)
            (local ((define (my-color n)
                      (color (* 50 (remainder n 6))
                             (* 20 (remainder n 13))
                             (* 70 (remainder n 3)))))
              (my-color col))))
    ((which-type? mt) (which-size? ms) "solid" (which-color? mc))))

            
(define (mrandom n)
  (add1 (random n)))
  


(define (tock ws)
  (local ((define pl (world-player ws))
          (define pl-b (world-player-bullets ws))
          (define wt (world-world-time ws))
          (define monsters (world-monsters ws))
          (define ps (world-produce-speed ws))
          (define ps-b (produce-speed-bullet ps))
          (define ps-m (produce-speed-monster ps))
          (define score (world-score ws))
          (define pd (world-display-state ws))
          (define stl-1 (display-state-lv1 pd))
          (define stl-2 (display-state-lv2 pd))
          (define stl-3 (display-state-lv3 pd))
          (define hp (player-hp pl))
          (define los (world-score-record ws))
          (define pl1 (world-playerone ws))
          (define pl2 (world-playertwo ws))
          (define boards (world-boards ws))
          (define pl1-hp (player-hp pl1))
          (define pl2-hp (player-hp pl2))
          (define pl1-b (player-bullets pl1))
          (define mps (world-monster-players ws))
          (define wb (world-bullets ws))
          
          

          (define (new-player-bullet-main-1 player)
            (player-bullet (player-x player)
                           (- (player-y player) 20)
                           0
                           BULLET-MOVE-SPEED
                           0 1))
          (define (new-player-bullet-main-2 player)
            (player-bullet (player-x player)
                           (- (player-y player) 20)
                           0
                           BULLET-MOVE-SPEED
                           0 2))
          
          (define (new-player-bullet-left player)
            (player-bullet (- (player-x player) 30)
                           (- (player-y player) 10)
                           0
                           BULLET-MOVE-SPEED
                           0 2))
          (define (new-player-bullet-right player)
            (player-bullet (+ (player-x player) 30)
                           (- (player-y player) 10)
                           0
                           BULLET-MOVE-SPEED
                           0 2))
          (define (new-monster wt score)
            (local ((define ms (/ (+ ROTATE-RADIUS MONSTER-SIZE) 2))
                    (define initial-pos
                      (posn (+ ms (random (- BCK-WIDTH MONSTER-SIZE ROTATE-RADIUS)))
                            MONSTER-PRODUCE-Y))
                    (define quo (add1 (quotient wt (* LEVEL-TIME FLASH-SPEED))))
                    (define ranquo (mrandom (if (<= quo 4) quo 4)))
                    (define size (mrandom (if (<= quo 3) quo 3)))
                    (define hp (ceiling (* 1/2 (- 4 size)))))
                    (monster initial-pos initial-pos 0
                             (ceiling (/ MONSTER-MOVE-SPEED
                                         (add1 (* 2(quotient score 100))))) hp
                             (mrandom MONSTER-TYPE-RANGE)
                             (mrandom MONSTER-COLOR-RANGE)
                             size
                             (mrandom 5)
                             (random 3))))

          (define (origin->final pos orbit)
            (local ((define x (posn-x pos))
                    (define y (posn-y pos)))
             (cond
              [(= orbit 1) pos]
              [(= orbit 2) (posn (+ x
                                    (* ROTATE-RADIUS (sin (/ (+ y wt) 50))))
                                  y)]
              [(= orbit 3) (posn (+ x
                                    (* ROTATE-RADIUS
                                       (sin (/ (+ x wt) 25))))
                                 (+ y
                                    (* ROTATE-RADIUS
                                       (cos (/ (+ x wt) 25)))))]
              [(= orbit 4) (posn (+ x (* ROTATE-RADIUS (sin (/ (+ x wt) 30))))
                                 (+ y (* ROTATE-RADIUS (sin (/ (+ x wt) 40)))))]
              [(= orbit 5) (posn (+ x (* ROTATE-RADIUS (cos (/ (+ y wt) 40))))
                                 (+ y (* ROTATE-RADIUS (sin (/ (+ y wt) 40)))))])))
          
          ; lIST-of-Monster -> Number
          ; computes the scores produces by the monsters whose hp equal to zero
          (define (total-new-score mons)
            (if (empty? mons) 0
                (+ (if (monster-hit-bullets (first mons) pl-b)
                       (monster->score (first mons))
                       0)
                   (total-new-score (rest mons)))))
          ; Monster -> Number
          ; given a monster, produces an number represents the score
           (define (monster->score mon)
            1)

          ; Bullet Monster -> Boolean
          ; whether a bullet and a monster are touching
          (define (hit-bullet-monster bullet monster)
            (if (> (distance (posn-x (monster-final monster))
                             (posn-y (monster-final monster))
                             (player-bullet-x bullet)
                             (player-bullet-y bullet))
                   (+ (/ MONSTER-SIZE 2) (/ (get-bullet-size bullet) 2)))
                #f #t))
          ; Monster Player -> Boolean
          ; whether a player and a monster are hitting
           (define (hit-monster-player mons pl)
            (if (>= (distance (posn-x (monster-final mons))
                             (posn-y (monster-final mons))
                             (player-x pl)
                             (player-y pl))
                   (+ (/ MONSTER-SIZE 2) (/ HEAD-HEIGHT 2)))
                #f #t))
          ; Monster List-of-Bullet -> Boolean
          ; if any one of the bullets is hit by the given monster
          (define (monster-hit-bullets mons bullets)
            (local ((define (hit? bullet)
                      (hit-bullet-monster bullet mons)))
              (ormap hit? bullets)))
          ; Bullet List-of-Monster -> Boolean
          ; if any one of the monsters is hit by the given bullet
          (define (bullet-hit-monsters bullet mons)
            (local ((define (hit? mon)
                      (hit-bullet-monster bullet mon)))
              (ormap hit? mons)))
          ; Monster Player List-of-Bullet -> Boolean
          ; judges if a monster has a hit with anything
          (define (monster-hit-anything? mons pl bls)
            (or (not (not-hit-bottom? mons))
                (hit-monster-player mons pl)
                (monster-hit-bullets mons bls)))
          ; Monster -> Boolean
          ; judges if a monster is hitting the bottom
          (define (not-hit-bottom? monster)
            (if (>= (posn-y (monster-final monster))
                    (- BCK-HEIGHT (/ MONSTER-SIZE 2)))
                #f #t))

          
          ; List-of-Monster Player -> Number
          ; computes the hp shoulb be decrease
          (define (player-hp-decrease-number mons player)
            (local ((define (monster-hit-pl/bottom? mon)
                      (or (hit-monster-player mon player)
                          (not (not-hit-bottom? mon)))))
              (length (filter monster-hit-pl/bottom? mons))))
          ; Player -> Player
          ; construct next player with hp changing
          (define (next-player pl)
                (struct-copy player pl
                         [hp (- (player-hp pl)
                                (player-hp-decrease-number monsters pl))]
                         [x (next-player-x pl)]
                         [y (next-player-y pl)]))
          (define (next-player-x player)
            (local ((define l (player-left player))
                    (define r (player-right player)))
              (cond
                [l (next-left player)]
                [r (next-right player)]
                [else (player-x player)])))
          (define (next-player-y player)
            (local ((define u (player-up player))
                    (define d (player-down player)))
              (cond
                [u (next-top player)]
                [d (next-bottom player)]
                [else (player-y player)])))
          (define (next-left player)
            (local ((define pl-x (player-x player))
                    (define pl-vlx (player-vel-x player)))
            (if (hit-left-edge? (- pl-x pl-vlx) HEAD-WIDTH)
                (/ HEAD-WIDTH 2)
                (- pl-x pl-vlx))))
          (define (next-right player)
            (local ((define pl-x (player-x player))
                    (define pl-vlx (player-vel-x player)))
            (if (hit-right-edge? (+ pl-x pl-vlx) HEAD-WIDTH)
                (- BCK-WIDTH (/ HEAD-WIDTH 2))
                (+ pl-x pl-vlx))))
          (define (next-top player)
            (local ((define pl-y (player-y player))
                    (define pl-vly (player-vel-y player)))
            (if (hit-top-edge? (- pl-y pl-vly) HEAD-HEIGHT)
                (/ HEAD-HEIGHT 2)
                (- pl-y pl-vly))))
          (define (next-bottom player)
            (local ((define pl-y (player-y player))
                    (define pl-vly (player-vel-y player)))
            (if (hit-bottom-edge? (+ pl-y pl-vly) HEAD-HEIGHT)
                (- BCK-HEIGHT (/ HEAD-HEIGHT 2))
                (+ pl-y pl-vly))))
          

          
          ; List-of-Monster -> List-of-Monster
          ; produces the Monsters whose hp is not zero
          (define (rest-monsters mons)
            (local ((define (not-zero-hp? mon)
                      (> (monster-hp mon) 0)))
              (filter not-zero-hp? mons)))
          ; List-of-Monster -> List-of-Monster
          ; set all the monsters' hp to zero
          (define (next-monsters-origin mons)
            (local ((define (hit-hp-decrease mon)
                      (if (monster-hit-anything? mon pl pl-b)
                          (if (monster-hit-bullets mon pl-b)
                              (struct-copy monster mon [hp (sub1 (monster-hp mon))])
                              (struct-copy monster mon [hp 0]))
                          mon)))
              (map hit-hp-decrease mons)))

          ; Monster -> Monster
          ; iterates monster without final change
          (define (next-monster-origin mon)
            (if (= 0 (modulo wt (monster-vel-y mon)))
                (struct-copy monster mon
                             [origin (struct-copy posn (monster-origin mon)
                                                  [y (+ (posn-y (monster-origin mon))
                                                        BASE-MOVE)])])
                mon))
          ; Monster -> Monster
          ; totally renew the monster
          (define (next-monster mon)
            (local ((define newmon (next-monster-origin mon)))
              (struct-copy monster newmon
                           [final (origin->final (monster-origin newmon)
                                                 (monster-orbit newmon))])))
       
            
          ; Time -> Boolean
          (define (create-bullet? worldtime)
            (= 0 (modulo worldtime (ceiling (/ FLASH-SPEED ps-b)))))
          ; Time -> Boolean
          (define (create-monster? wt)
            (= 0 (modulo wt (floor (/ FLASH-SPEED (monster-produce-speed0 ps-m wt))))))
          (define (monster-produce-speed0 p wtime)
            (local ((define psm (quotient wtime (* 2 LEVEL-TIME FLASH-SPEED))))
              (if (psm . <= . 3) (+ psm p) 5)))
            

          
          ; Bullet List-of-Monster -> Boolean
          ; judeges if a bullet is hit by anything
          (define (bullet-hit-anything? bullet mons)
            (or (not (not-hit-top? bullet))
                (bullet-hit-monsters bullet mons)))
          ; Bullet -> Boolean
          ; if a bullet is hitting the top
          (define (not-hit-top? bullet)
            (if (<= (player-bullet-y bullet) (- 0 RADIUS)) #f #t))
          ; List-of-Bullet -> List-of-Bullet
          ; get the bullets don't hit anything
          (define (rest-bullets pbs)
            (local ((define (not-hit? bullet)
                      (not (bullet-hit-anything? bullet monsters))))
            (filter not-hit? pbs)))
          ; Bullet -> Bullet
          ; renew the bullet
          (define (next-player-bullet pb)
            (if (= 0 (modulo (world-world-time ws) (player-bullet-vel-y pb)))
                (struct-copy player-bullet pb
                             [y (- (player-bullet-y pb) BASE-MOVE-BULLET)])
                pb))
          ; List-of-Bullet -> List-of-Bullet
          ; renew all the bullet and delete the one who hit
          (define (next-player-bullets pbs)
            (map next-player-bullet (rest-bullets pbs)))
          
          
          (define next-mons (map next-monster
                                 (rest-monsters (next-monsters-origin monsters))))
          (define next-pl-b (next-player-bullets pl-b))
          (define new-monsters
            (if (create-monster? wt)
                (cons (new-monster wt score)
                      next-mons)
                next-mons))
          (define new-player-bullets-3
            (if (create-bullet? (world-world-time ws))
                (cons (new-player-bullet-main-1 pl)
                      (cons (new-player-bullet-left pl)
                            (cons (new-player-bullet-right pl)
                                  next-pl-b)))
                next-pl-b))
          (define new-player-bullets-2
            (if (create-bullet? wt)
                (cons (new-player-bullet-left pl)
                      (cons (new-player-bullet-right pl)
                            next-pl-b))
                next-pl-b))
          (define new-player-bullets-1
            (if (create-bullet? (world-world-time ws))
                (cons (new-player-bullet-main-1 pl) next-pl-b)
                next-pl-b))
          (define new-player-bullets-0
            (if (create-bullet? wt)
                (cons (new-player-bullet-main-2 pl) next-pl-b)
                next-pl-b))
          (define next-pl (next-player pl))

          (define (which-bullet? score)
            (local ((define level score))
              (cond
                [(<= level 50) new-player-bullets-0]
                [(<= level 100) new-player-bullets-1]
                [(<= level 200) new-player-bullets-2]
                [else new-player-bullets-3])))
          (define game1-world-change
            (struct-copy world ws
                 [player next-pl]
                 [player-bullets (which-bullet? score)]
                 [world-time (add1 (world-world-time ws))]
                 [monsters new-monsters]
                 [score (+ (world-score ws) (total-new-score monsters))]))
          (define game2-world-change
            (struct-copy world ws
                         [playerone (next-combat-player pl1 boards pl2 wt 1)]
                         [playertwo (next-combat-player pl2 boards pl1 wt 3)]
                         [world-time (add1 wt)]
                         [boards (next-boards boards wt)]))
          (define game3-world-change
            (struct-copy world ws
                         [monster-players (new-monster-players
                                           (next-monster-players mps boards pl1-b) wt)]
                         [playerone (next-playerone pl1 boards wt wb mps)]
                         [bullets (new-challenge-bullets mps (next-bullets wb pl1))]
                         [score1 (+ (world-score1 ws) (get-hit-score mps))]
                         [world-time (add1 wt)]))
          (define game1-world-change/end
            (if (= hp 0)
                (struct-copy world ws
                             [display-state (struct-copy display-state pd
                                                         [lv2 3])])
                game1-world-change))
          (define game2-world-change/end
            (if (or (= 0 pl1-hp) (= 0 pl2-hp))
                (struct-copy world ws
                             [display-state (struct-copy display-state pd
                                                         [lv2 3])])
                game2-world-change))
          (define game3-world-change/end
            (if (= 0 pl1-hp)
                (struct-copy world ws
                             [display-state (struct-copy display-state pd
                                                         [lv2 3])])
                game3-world-change))
          
          
          (define (change-world lv1 lv2 lv3)
            (cond
              [(= lv1 2)
               (cond
                 [(= lv2 1) game1-world-change/end]
                 [else ws])]
              [(= lv1 3)
               (cond
                 [(= lv2 1) game2-world-change/end]
                 [else ws])]
              [(= lv1 6)
               (cond
                 [(= lv2 1) game3-world-change/end]
                 [else ws])]
              [else ws])))
  (change-world stl-1 stl-2 stl-3)))
 
(define (distance x1 y1 x2 y2)
  (sqrt (+ (sqr (- x1 x2))
           (sqr (- y1 y2)))))

(define (on-board? player board)
  (and (<= (abs (- (player-x player) (board-x board)))
           (+ (/ PLAYER-WIDTH 2) (/ (board-width board) 2)))
       (<= (abs (- (- (board-y board) (player-y player))
                  (+ (/ PLAYER-HEIGHT 2) (/ (board-height board) 2))))
          2)))
(define (on-ground? player)
  (= (player-y player)
     (- WORLD-HEIGHT (/ PLAYER-HEIGHT 2))))
(define (on-block? player boards)
  (or (on-ground? player)
      (ormap (lambda (x) (on-board? player x)) boards)))

(define (hit-player-bullet? play bullet)
  (and (<= (abs (- (player-x play) (player-bullet-x bullet)))
          (+ (/ PLAYER-WIDTH 2) (/ (get-bullet-size bullet) 2)))
       (<= (abs (- (player-y play) (player-bullet-y bullet)))
          (/ (+ PLAYER-HEIGHT (get-bullet-size bullet)) 2))))
(define (hit-player-bullets? play bullets)
  (ormap (lambda (x) (hit-player-bullet? play x)) bullets))
(define (next-player-hp play bullets)
  (local ((define decrease (length (filter
                                (lambda (x) (hit-player-bullet? play x)) bullets))))
    (- (player-hp play) decrease)))

(define (hit-board? play board)
  (local ((define pl-x (player-x play))
          (define pl-y (player-y play))
          (define bo-x (board-x board))
          (define bo-y (board-y board))
          (define width (board-width board))
          (define height (board-height board)))
    (and (<= (abs (- pl-x bo-x))
             (+ (/ PLAYER-WIDTH 2) (/ width 2)))
         (<= (abs (- pl-y bo-y))
             (+ (/ PLAYER-HEIGHT 2) (/ height 2))))))
(define (move-board boar wt width)
  (struct-copy board boar
               [x (+ width (* 50 (sin (+ (board-y boar) (/ wt 50)))))]))
(define (next-boards boards wt)
  (local ((define 1st (first boards))
          (define 2nd (first (rest boards)))
          (define 2rest (rest (rest boards))))
    (cons (move-board 1st wt 150)
          (cons (move-board 2nd wt (- WORLD-WIDTH 150))
                2rest))))
(define (hit-board-from-upside? play board)
  (local ((define pl-y (player-y play))
          (define pl-vly (player-vel-y play))
          (define new-y (+ pl-y pl-vly))
          (define bo-y (board-y board))
          (define dis (- bo-y (/ (board-width board) 2))))
    (and (hit-board? play board)
         (< (+ pl-y (/ PLAYER-HEIGHT 2)) dis)
         (> (+ new-y (/ PLAYER-HEIGHT 2)) dis)))) 
(define (get-board-y board)
  (- (board-y board) (/ PLAYER-HEIGHT 2) (/ (board-width board) 2)))
  
(define (next-combat-down play boards)
  (local ((define pl-y (player-y play))
          (define pl-vly (player-vel-y play))
          (define new-y (+ pl-y pl-vly))
          (define hit-boards (filter (lambda (x) (hit-board-from-upside?
                                                  play x))
                                     boards)))
    (cond
      [(hit-bottom-edge? new-y PLAYER-HEIGHT)
       (- WORLD-HEIGHT (/ PLAYER-HEIGHT 2))]
      [(empty? hit-boards) new-y]
      [else (get-board-y (first hit-boards))])))

  
(define (next-vel-y play boards)
  (local ((define hit-boards (filter (lambda (x) (hit-board-from-upside?
                                                  play x))
                                     boards))
          (define new-play (struct-copy player play [y (next-combat-down play boards)])))
    
    (if (and (>= (player-vel-y play) 0) (on-block? new-play boards))
        0
        (if (< (player-vel-y play) 4)
            (if (< (player-vel-y play) 0)
                (+ 1/4 (player-vel-y play))
                (add1 (player-vel-y play)))
            4))))



(define (next-combat-player play boards play2 wt lv)
  (local ((define pl-x (player-x play))
          (define pl-y (player-y play))
          (define pl-vlx (player-vel-x play))
          (define pl-vly (player-vel-y play))
          (define up (player-up play))
          (define bullets (player-bullets play2))
          (define make (player-make-bullet? play))
          (define bt (player-bullet-time play))
          (define lob (player-bullets play))
          (define (change-bullet-time x)
            (if x (add1 bt) 0)))
    (struct-copy player play
                 [x (next-combat-player-x play)]
                 [y (next-combat-down play boards)]
                 [vel-y (next-vel-y play boards)]
                 [hp (next-player-hp play bullets)]
                 [bullets (new-bullets (next-bullets lob play2) play lv wt)]
                 [bullet-time (change-bullet-time make)] )))

(define (next-playerone play boards wt bullets mps)
  (local ((define pl-x (player-x play))
          (define pl-y (player-y play))
          (define pl-vlx (player-vel-x play))
          (define pl-vly (player-vel-y play))
          (define up (player-up play))
          (define make (player-make-bullet? play))
          (define bt (player-bullet-time play))
          (define lob (player-bullets play))
          (define (change-bullet-time x)
            (if x (add1 bt) 0)))
     (struct-copy player play
                 [x (next-combat-player-x play)]
                 [y (next-combat-down play boards)]
                 [vel-y (next-vel-y play boards)]
                 [hp (next-player-hp play bullets)]
                 [bullets (new-bullets (next-playerone-bullets lob mps) play 1 wt)]
                 [bullet-time (change-bullet-time make)])))
    

  
(define (bullet-hit-edge/player? bull play)
  (local ((define b-x (player-bullet-x bull)))
    (or (hit-left-edge? b-x RADIUS)
        (hit-right-edge-meta? b-x RADIUS)
        (hit-player-bullet? play bull))))

(define (bullet-hit-edge/mps? bull mps)
  (ormap (lambda (x) (bullet-hit-edge/player? bull x)) mps))

(define (rest-playerone-bullets bulls mps)
  (filter (lambda (x) (not (bullet-hit-edge/mps? x mps))) bulls))

(define (next-playerone-bullets bulls mps)
  (map next-bullet (rest-playerone-bullets bulls mps)))


(define (bullet-not-hit-edge/player? bull play)
  (not (bullet-hit-edge/player? bull play)))

(define (rest-bullets bulls play)
  (filter (lambda (x) (bullet-not-hit-edge/player? x play)) bulls))

(define (next-bullet bull)
  (struct-copy player-bullet bull
               [x (+ (player-bullet-x bull) (player-bullet-vel-x bull))]))

(define (next-bullets bulls play)
  (map next-bullet (rest-bullets bulls play)))
               
(define (next-combat-player-x play)
  (local ((define l (player-left play))
          (define r (player-right play)))
    (cond
      [l (next-combat-left play)]
      [r (next-combat-right play)]
      [else (player-x play)])))

(define (next-monster-player-x play)
  (local ((define ori (player-orientation play))
          (define pl-x (player-x play))
          (define pl-vlx (player-vel-x play))
          (define next-left (- pl-x pl-vlx))
          (define next-right (+ pl-x pl-vlx)))
    (if (= ori LEFT)
        (if (hit-left-edge? next-left PLAYER-WIDTH)
            (/ PLAYER-WIDTH 2)
            next-left)
        (if (hit-right-edge-meta? next-right PLAYER-WIDTH)
            (- WORLD-WIDTH (/ PLAYER-WIDTH 2))
            next-right))))

(define (next-combat-left play)
  (local ((define pl-x (player-x play))
          (define pl-vlx (player-vel-x play)))
    (if (hit-left-edge? (+ pl-x pl-vlx) PLAYER-WIDTH)
        (/ PLAYER-WIDTH 2)
        (if (hit-right-edge-meta? (+ pl-x pl-vlx) PLAYER-WIDTH)
        (- WORLD-WIDTH (/ PLAYER-WIDTH 2))
        (+ pl-x pl-vlx)))))

(define (next-combat-right play)
  (local ((define pl-x (player-x play))
          (define pl-vlx (player-vel-x play))
          (define move (+ pl-x pl-vlx)))
    (if (hit-right-edge-meta? move PLAYER-WIDTH)
        (- WORLD-WIDTH (/ PLAYER-WIDTH 2))
        (if (hit-left-edge? (+ pl-x pl-vlx) PLAYER-WIDTH)
        (/ PLAYER-WIDTH 2)
        move))))
(define (new-bullet play lv speed)
            (player-bullet (player-x play) (player-y play)
                           (if (= (player-orientation play) LEFT)
                               (- 0 speed) speed)
                           0 0 lv))


(define (new-bullets bullets play lv wt)
  (local ((define bt (player-bullet-time play))
          (define make? (player-make-bullet? play)))
    (if (= (modulo bt 40) 6)
        (cons (new-bullet play lv 20)
              bullets)
        bullets)))

(define (new-monster-player n)
  (local ((define x (+ 50 (random (- n 50))))
          (define y -100)
          (define vel-x 4)
          (define vel-y 0)
          (define hp (mrandom 3))
          (define ori (- (mrandom 2) 1)))
    (player x y vel-x vel-y hp "" #f #f #f #f '() ori #f 0)))
(define (new-monster-players mps wt)
  (local ((define (make? wt)
            (= 0 (modulo wt 300))))
    (if (and (make? wt) (<= (length mps) 4))
        (cons (new-monster-player WORLD-WIDTH) mps)
        mps)))
(define (rest-monster-players mps)
  (filter (lambda (x) (> (player-hp x) 0)) mps))
(define (get-hit-score mps)
  (length (filter (lambda (x) (<= (player-hp x) 0)) mps)))

(define (next-monster-player mp boards bullets)
  (local ((define ori (player-orientation mp))
          (define bt (player-bullet-time mp))
          (define pl-x (player-x mp))
          (define (change-ori pl ori)
            (if (= ori LEFT)
                (if (hit-left-edge? pl PLAYER-WIDTH)
                    RIGHT LEFT)
                (if (hit-right-edge-meta? pl PLAYER-WIDTH)
                    LEFT RIGHT))))
    (struct-copy player mp
                 [x (next-monster-player-x mp)]
                 [y (next-combat-down mp boards)]
                 [vel-y (next-mp-vel-y mp boards)]
                 [hp (next-player-hp mp bullets)]
                 [bullet-time (add1 bt)]
                 [orientation (change-ori pl-x ori)])))

(define (next-mp-vel-y mp boards)
  (local ((define mt (player-bullet-time mp)))
    (if (and (= 0 (modulo mt 150))
             (on-block? mp boards))
        -9
        (next-vel-y mp boards))))

          
(define (next-monster-players mps boards bullets)
  (map (lambda (x) (next-monster-player x boards bullets))
       (rest-monster-players mps)))
          
(define (new-challenge-bullets mps bullets)
  (local ((define (pl-time play)
            (player-bullet-time play))
          (define (make? ti)
            (= 0 (modulo ti 100)))
          (define (make-bullet mp)
            (if (make? (pl-time mp))
                (new-bullet mp 3 13)
                0))
          (define (make-bullets mps)
            (map make-bullet mps))
          (define (true-bullets mps)
            (filter (lambda (x) (not (number? x))) mps)))
    (foldr (lambda (x y) (cons x y))
           (true-bullets (make-bullets mps)) bullets)))
  
      





(define (keyh ws ke)
  (local ((define pl (world-player ws))
          (define dp (world-display-state ws))
          (define stl-1 (display-state-lv1 dp))
          (define stl-2 (display-state-lv2 dp))
          (define stl-3 (display-state-lv3 dp))
          (define score (world-score ws))
          (define score1 (world-score1 ws))
          (define los (world-score-record ws))
          (define los1 (if (empty? los) '(" ") (first los)))
          (define los2 (if (empty? (rest los)) '(" ") (first (rest los))))
          (define pl1 (world-playerone ws))
          (define pl2 (world-playertwo ws))
          (define boards (world-boards ws))
          (define pl1-x (player-x pl1))
          (define pl1-y (player-y pl1))
          (define pl2-x (player-x pl2))
          (define pl2-y (player-y pl2))
          (define pl1-bullets (player-bullets pl1))
          (define pl2-bullets (player-bullets pl2))
          (define pl1-ori (player-orientation pl1))
          (define pl2-ori (player-orientation pl2))

          (define (menu-move ke)
            (cond
              [(or (string=? ke "down") (string=? ke "s"))
               (struct-copy display-state dp
                            [lv2 (if (= stl-2 6) 1 (add1 stl-2))])]
              [(or (string=? ke "up") (string=? ke "w"))
               (struct-copy display-state dp
                            [lv2 (if (= stl-2 1) 6 (sub1 stl-2))])]
              [(string=? ke "\r") (menu-change stl-2)]
              [else dp]))
          (define (menu-change lv2)
            (cond
              [(= 1 lv2)
               (struct-copy display-state dp
                            [lv1 2]
                            [lv2 1])]
              [(= 6 lv2) (struct-copy display-state dp
                                      [lv1 0]
                                      [lv2 1])]
              [(= 2 lv2) (struct-copy display-state dp
                                      [lv1 3]
                                      [lv2 1])]
              [(= 3 lv2) (struct-copy display-state dp
                                      [lv1 6]
                                      [lv2 1]
                                      )]
              [(= 4 lv2) (struct-copy display-state dp
                                      [lv1 4])]
              [(= 5 lv2) (struct-copy display-state dp
                                      [lv1 5])]
              [else dp]))
          (define (game-display-change ke)
            (cond
              [(string=? ke "escape") (struct-copy display-state dp
                                                   [lv2 2]
                                                   [lv3 1])]
              [else dp]))
          (define (game-pause-change ke)
            (cond
              [(string=? ke "down") (struct-copy display-state dp
                                                 [lv3 (if (= stl-3 1) 2 1)])]
              [(string=? ke "up") (struct-copy display-state dp
                                               [lv3 (if (= stl-3 2) 1 2)])]
              [(string=? ke "\r") (if (= stl-3 1) (struct-copy display-state dp
                                                               [lv2 1]
                                                               [lv3 1])
                                      (struct-copy display-state dp
                                                   [lv1 1]
                                                   [lv2 1]))]
              [else dp]))
          (define (game-over-change ke)
            (if (string=? ke "escape") (struct-copy display-state dp
                                               [lv1 1]
                                               [lv2 1])
                dp))
                                        
          (define (add-score-record lst score)
            (if (empty? lst)
                (cons (number->string score) lst)
                (if (ormap (lambda (x) (string=? x (number->string score))) lst)
                    lst
                     (if (> score (string->number (first lst)))
                     (cons (number->string score) lst)
                     (cons (first lst) (add-score-record (rest lst) score))))))
          (define (fixed-score-record lst)
            (if (> (length lst) 6)
                (reverse (rest (reverse lst)))
                lst))
         
          
          (define (player-move ke)
            (cond
              [(move-left ke)
               (struct-copy player
                            pl
                            [left #t])]
              [(move-right ke)
               (struct-copy player pl
                            [right #t])]
              [(move-up ke)
               (struct-copy player pl
                            [up #t])]
              [(move-down ke)
               (struct-copy player pl
                            [down #t])]
              [else pl]))
          (define (player1-move ke)
            (cond
              [(string=? "a" ke)
               (struct-copy player pl1
                            [left #t]
                            [vel-x -5]
                            [orientation LEFT])]
              [(string=? "d" ke)
               (struct-copy player pl1
                            [right #t]
                            [vel-x 5]
                            [orientation RIGHT])]
              [(string=? "w" ke)
               (struct-copy player pl1
                            [vel-y (if (on-block? pl1 boards) -10 (player-vel-y pl1))])]
              [(string=? "s" ke)
               (struct-copy player pl1
                            [vel-y (if (on-block? pl1 boards) 9 (player-vel-y pl1))])]
              [(string=? " " ke)
               (struct-copy player pl1
                            [make-bullet? #t])]
              [else pl1]))
          (define (player2-move ke)
            (cond
              [(or (string=? "left" ke)
                   (string=? "j" ke))
               (struct-copy player pl2
                            [left #t]
                            [vel-x -5]
                            [orientation LEFT])]
              [(or (string=? "right" ke)
                   (string=? "l" ke))
               (struct-copy player pl2
                            [right #t]
                            [vel-x 5]
                            [orientation RIGHT])]
              [(or (string=? "up" ke)
                   (string=? "i" ke))
               (struct-copy player pl2
                            [vel-y (if (on-block? pl2 boards) -10 (player-vel-y pl2))])]
              [(or (string=? "down" ke)
                   (string=? "k" ke))
               (struct-copy player pl2
                            [vel-y (if (on-block? pl2 boards) 9 (player-vel-y pl2))])]
              [(or (string=? "\r" ke)
                   (string=? ";" ke)
                   (string=? "b" ke))
               (struct-copy player pl2
                           [make-bullet? #t])]
              [else pl2]))
          (define (score-change ke)
            (if (string=? "escape" ke)
                (struct-copy display-state dp
                             [lv1 1])
                dp))

          (define (write->file lst)
            (write-file RECORD-FILE
                        (string-append (lst->string (first lst)) "\n"
                                       (lst->string (first (rest lst))))))
          (define (lst->string lst)
            (foldl (lambda (x y) (string-append y x))
                   " "
                   (map (lambda (x) (string-append x " ")) lst)))
          (define (world-change lv1 lv2 lv3)
            (cond
              [(= 1 lv1) (struct-copy world ws
                                      [display-state (menu-move ke)]
                                      [boards (if (= 6 (display-state-lv1
                                                        (menu-move ke)))
                                                  Boards-challenge Boards)])]
              [(= 2 lv1)
               (cond
                 [(= lv2 1)
                  (struct-copy world ws
                               [player (player-move ke)]
                               [display-state (game-display-change ke)]
                               )]
                 [(= lv2 2)
                  (if (and (string=? ke "\r") (= lv3 2))
                      World
                      (struct-copy world ws
                               [display-state (game-pause-change ke)]))]
                 [(= lv2 3) (if (string=? ke "escape")
                                (struct-copy world World
                                             [score-record
                                              (read-words/line
                                               (write->file
                                                (list
                                                 (fixed-score-record
                                                  (add-score-record los1 score))
                                                 los2)))])
                                ws)])]
              [(= 3 lv1)
               (cond
                 [(= lv2 1)
                  (struct-copy world ws
                               [playerone (player1-move ke)]
                               [playertwo (player2-move ke)]
                               [display-state (game-display-change ke)])]
                 [(= lv2 2)
                  (if (and (string=? ke "\r") (= lv3 2))
                      World
                      (struct-copy world ws
                                   [display-state (game-pause-change ke)]))]
                 [(= lv2 3) (if (string=? ke "escape")
                                World ws)])]
              [(= 6 lv1)
               (cond
                 [(= lv2 1)
                  (struct-copy world ws
                               [playerone (player1-move ke)]
                               [display-state (game-display-change ke)])]
                 [(= lv2 2)
                  (if (and (string=? ke "\r") (= lv3 2))
                      World
                      (struct-copy world ws
                                   [display-state (game-pause-change ke)]))]
                 [(= lv2 3) (if (string=? ke "escape")
                                (struct-copy world World
                                             (score-record
                                              (read-words/line
                                               (write->file
                                                (list los1
                                                      (fixed-score-record
                                                       (add-score-record los2 score1)))))))
                                ws)])]
                               
                  
              [(= 4 lv1) (struct-copy world ws
                                      [display-state (score-change ke)])]
              [(= 0 lv1) (write-file RECORD-FILE
                                     (foldl (lambda (x y) (string-append x y)) los " "))]
              [(= 5 lv1) (struct-copy world ws
                                      [display-state (score-change ke)])]
              [else ws])))
          
    (world-change stl-1 stl-2 stl-3)))

(define (keyr ws ke)
  (local ((define pl (world-player ws))
          (define pl1 (world-playerone ws))
          (define pl2 (world-playertwo ws))
          (define dp (world-display-state ws))
          (define stl-1 (display-state-lv1 dp))
          (define stl-2 (display-state-lv2 dp))
          (define stl-3 (display-state-lv3 dp))
    (define (player-move-stop ke)
      (cond
        [(move-left ke)
         (struct-copy player pl [left #f])]
        [(move-right ke)
         (struct-copy player pl [right #f])]
        [(move-up ke)
         (struct-copy player pl [up #f])]
        [(move-down ke)
         (struct-copy player pl [down #f])]
        [else pl]))
    (define (combat-player1-move-stop ke)
      (cond
        [(string=? "a" ke) (struct-copy player pl1 [left #f])]
        [(string=? "d" ke) (struct-copy player pl1 [right #f])]
        [(string=? " " ke) (struct-copy player pl1 [make-bullet? #f])]
        [else pl1]))
    (define (combat-player2-move-stop ke)
      (cond
        [(or (string=? "j" ke)
             (string=? "left" ke)) (struct-copy player pl2 [left #f])]
        [(or (string=? "l" ke)
             (string=? "right" ke)) (struct-copy player pl2 [right #f])]
        [(or (string=? ";" ke)
             (string=? "\r" ke)
             (string=? "b" ke))(struct-copy player pl2 [make-bullet? #f])]
        [else pl2]))
    (define (world-change lv1 lv2 lv3)
      (cond
        [(= lv1 2) (if (= lv2 1) (struct-copy world ws
                                              [player (player-move-stop ke)])
                       ws)]
        [(= lv1 3) (if (= lv2 1)
                       (struct-copy world ws
                                    [playerone (combat-player1-move-stop ke)]
                                    [playertwo (combat-player2-move-stop ke)])
                       ws)]
        [(= lv1 6) (if (= lv2 1)
                       (struct-copy world ws
                                    [playerone (combat-player1-move-stop ke)])
                       ws)]
        [else ws])))
    (world-change stl-1 stl-2 stl-3)))


(define (move-left ke)
  (or (string=? ke "a") (string=? ke "left")))
(define (move-right ke)
  (or (string=? ke "d") (string=? ke "right")))
(define (move-up ke)
  (or (string=? ke "w") (string=? ke "up")))
(define (move-down ke)
  (or (string=? ke "s") (string=? ke "down")))
                                           
(define (hit-top-edge? y leng)
  (<= y (/ leng 2)))
(define (hit-bottom-edge? y leng)
  (>= y (- BCK-HEIGHT (/ leng 2))))
(define (hit-left-edge? x leng)
  (<= x (/ leng 2)))
(define (hit-right-edge? x leng)
  (>= x (- BCK-WIDTH (/ leng 2))))
(define (hit-right-edge-meta? x leng)
  (>= x (- WORLD-WIDTH (/ leng 2))))


;;; Main function
(define (main ws)
  (local ((define (stl-equal-zero? ws)
            (zero? (display-state-lv1 (world-display-state ws))))
          (define dp (world-display-state ws))
          (define stl-1 (display-state-lv1 dp))
          (define stl-2 (display-state-lv2 dp)))
            
  (big-bang ws
            [to-draw render]
            [on-tick tock (/ 1 FLASH-SPEED)]
            [on-key keyh]
            [on-release keyr]
            [stop-when stl-equal-zero?]
            [close-on-stop 0]
            [display-mode 'normal]
            [name "消方"]
    )))

(main World)

