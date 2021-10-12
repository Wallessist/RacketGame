#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

(provide (all-defined-out))

(define WORLD-WIDTH 1400)
(define WORLD-HEIGHT 800)


; number number make-color -> image
(define (make-rec x y col)
  (rectangle x y "solid" col))
(define (make-cir r col)
  (circle r "solid" col))

(define head-col (make-color 42 39 91))
(define eye-col (make-color 243 119 255))
(define eye-col2 (make-color 252 242 131))
(define eye-side-col-v1 (make-color 161 84 200))
(define eye-side-col-v2 (make-color 120 73 193))

(define bck-col (make-color 85 75 155))

(define BCK-WIDTH 650)
(define BCK-HEIGHT WORLD-HEIGHT)

(define world-ima
  (make-rec WORLD-WIDTH WORLD-HEIGHT (make-color 85 75 155 200)))
(define pvp-bck
  (make-rec WORLD-WIDTH WORLD-HEIGHT bck-col))

(define MOUTH-WIDTH 10)
(define EYE-WIDTH 6)
(define EYE-HEIGHT 4)
(define HEAD-WIDTH 50)
(define HEAD-HEIGHT 40)
(define DISTANCE-EYE-HEAD 20)
(define DISTANCE-MOUTH-HEAD 30)
(define DISTANCE-EYE-EYE 18)
(define MENU-WIDTH 80)
(define MENU-HEIGHT 40)

(define (make-menu imaone imatwo)
  (overlay/offset imaone 0 0 imatwo))
(define bck-origin0 (make-rec BCK-WIDTH BCK-HEIGHT bck-col))
(define bck-origin
  (place-image bck-origin0
                  (/ WORLD-WIDTH 2)
                  (/ WORLD-HEIGHT 2)
                  world-ima))
(define bck-one (make-rec BCK-WIDTH BCK-HEIGHT (make-color 85 75 155 50)))
(define menu-bck-0 (make-rec MENU-WIDTH MENU-HEIGHT head-col))
(define menu-bck-1 (make-rec (+ 4 MENU-WIDTH) (+ 4 MENU-HEIGHT) "green"))
(define textone (text "消方" 18 "white"))
(define texttwo (text "对战" 18 "white"))
(define textthree (text "排行榜" 18 "white"))
(define textfour (text "退出游戏" 18 "white"))
(define textten (text "游戏说明" 18 "white"))
(define texteleven (text "挑战" 18 "white"))
(define text-menu-1 (make-menu textone menu-bck-0))
(define text-menu-2 (make-menu texttwo menu-bck-0))
(define text-menu-3 (make-menu textthree menu-bck-0))
(define text-menu-4 (make-menu textfour menu-bck-0))
(define text-menu-5 (make-menu textten menu-bck-0))
(define text-menu-6 (make-menu texteleven menu-bck-0))
(define text-list (list text-menu-1 text-menu-2 text-menu-6
                        text-menu-3 text-menu-5 text-menu-4))
(define interval (make-rec MENU-WIDTH MENU-HEIGHT (make-color 0 0 0 0)))

(define textfive (text "游戏暂停" 25 "gray"))
(define textsix (text "继续游戏" 18 "white"))
(define textseven (text "结束游戏" 18 "white"))
(define texteight (text "游戏结束" 25 "red"))
(define textnine (text "按esc返回主菜单" 18  "white"))
(define text-pause-1-origin (make-menu textsix menu-bck-0))
(define text-pause-2-origin (make-menu textseven menu-bck-0))
(define text-pause-1 (make-menu text-pause-1-origin menu-bck-1))
(define text-pause-2 (make-menu text-pause-2-origin menu-bck-1))
(define text-pause-list-1
  (above textfive
         interval
         text-pause-1
         text-pause-2-origin
         ))
(define text-pause-list-2
  (above textfive
         interval
         text-pause-1-origin
         text-pause-2
         ))
(define pause-1
  (overlay/offset text-pause-list-1
                 0 0
                 bck-one))
(define pause-2
  (overlay/offset text-pause-list-2
                 0 0
                 bck-one))
(define game-over-origin
  (overlay/offset (above texteight interval textnine)
                 0 0
                 bck-one))

(define FONTSIZE 15)
(define (make-hp-text hp col)
  (text (string-append "HP:" (number->string hp)) FONTSIZE col))
(define HP-TEXT-HEIGHT (image-height (make-hp-text 10 "white")))

(define readme
  (text "1.消方\n按方向键或wasd键控制方块，\n在图形落到底部前消灭他们\n\n2.对战\nwasd控制玩家1，空格发射炮弹；\n方向键控制玩家2，回车键发射炮弹;也可用ijkl控制，\";\"或b发射炮弹\n\n3.挑战\nwasd控制玩家,空格发射炮弹\n\n游戏中途均可按esc暂停游戏，\n菜单选项使用方向键或w,s移动，按回车确认，按esc返回上一级.\n" 15 "white"))


(define mouth (make-rec MOUTH-WIDTH EYE-HEIGHT eye-col))
(define mouth2 (make-rec MOUTH-WIDTH EYE-HEIGHT eye-col2))
(define eye-v0 (make-rec EYE-WIDTH EYE-HEIGHT eye-col))
(define eye-side-v1 (make-rec (+ EYE-WIDTH 1) (+ EYE-HEIGHT 1) eye-side-col-v1))
(define eye-side-v0 (make-rec (+ EYE-WIDTH 3) (+ EYE-HEIGHT 4) eye-side-col-v2))
(define eye-side
  (overlay/offset eye-side-v1
                  0 0
                  eye-side-v0))
(define eye-v1
  (overlay/offset eye-v0
                  0 0
                  eye-side))

(define eye-v2 (make-rec (+ EYE-WIDTH 3) (+ EYE-HEIGHT 4) eye-col2))
(define head-no-eyes (make-rec HEAD-WIDTH HEAD-HEIGHT head-col))
(define half-head (make-cir (/ HEAD-WIDTH 2) head-col))
(define head-no-eyes-pl
  (overlay/offset (square HEAD-WIDTH "solid" head-col)
                  (* HEAD-WIDTH 1/4) 0
                  half-head
                 ))
(define eyes
  (overlay/offset eye-v1
                  DISTANCE-EYE-EYE 0
                  eye-v1))
(define eyes2
  (overlay/offset eye-v2
                  DISTANCE-EYE-EYE 0
                  eye-v2))

(define head-v0
  (place-image eyes
               (/ HEAD-WIDTH 2) DISTANCE-EYE-HEAD
               head-no-eyes))
(define head-v1
  (place-image eyes
               (/ HEAD-WIDTH 2) DISTANCE-EYE-HEAD
               head-no-eyes-pl))
(define head-v2
  (place-image eyes2
               (/ HEAD-WIDTH 2) DISTANCE-EYE-HEAD
               head-no-eyes-pl))
(define PLAYER
  (place-image mouth
               (/ HEAD-WIDTH 2) DISTANCE-MOUTH-HEAD
               head-v0))
(define PLAYER-HEIGHT HEAD-HEIGHT)
(define PLAYER-WIDTH HEAD-WIDTH)

(define PLAYERONE-RIGHT
  (place-image mouth
               (/ HEAD-WIDTH 2) DISTANCE-MOUTH-HEAD
               head-v1))
(define PLAYERONE-LEFT
  (flip-horizontal PLAYERONE-RIGHT))
(define PLAYERTWO-RIGHT
  (place-image mouth2
               (/ HEAD-WIDTH 2) DISTANCE-MOUTH-HEAD
               head-v2))
(define PLAYERTWO-LEFT
  (flip-horizontal PLAYERTWO-RIGHT))






(define bullet-col-v1.1 (make-color 60 215 255))
(define bullet-col-v1.2 (make-color 63 255 255))
(define bullet-col-v1.3 (make-color 129 248 248))

(define BULLET-WIDTH-1 2)
(define BULLET-HEIGHT-1 4)
(define BULLET-WIDTH-2 3)
(define BULLET-HEIGHT-2 6)
(define BULLET-WIDTH-3 5)
(define BULLET-HEIGHT-3 8)

(define bullet-v1.1
  (make-rec BULLET-WIDTH-1 BULLET-HEIGHT-1 bullet-col-v1.1))
(define bullet-v1.2
  (make-rec BULLET-WIDTH-2 BULLET-HEIGHT-2 bullet-col-v1.2))
(define bullet-v1.3
  (make-rec BULLET-WIDTH-3 BULLET-HEIGHT-3 bullet-col-v1.3))
(define bullet-v1.4
  (triangle BULLET-WIDTH-3 "solid" bullet-col-v1.3))

(define bullet-v1.5
  (overlay/offset bullet-v1.1
                  0 0
                  (overlay/offset bullet-v1.2
                                  0 0
                                  bullet-v1.3)))
(define bullet
  (above bullet-v1.4 bullet-v1.5))



(define bullet-col-v2.1 (make-color 255 255 255))
(define bullet-col-v2.2 (make-color 172 255 255))

(define BULLET-RADIUS-v2.1 3)
(define BULLET-RADIUS-v2.2 5)
(define bullet-v2.1
  (make-cir BULLET-RADIUS-v2.1 bullet-col-v2.1))
(define bullet-v2.2
  (make-cir BULLET-RADIUS-v2.2 bullet-col-v2.2))
(define bullet-v2
  (overlay/offset bullet-v2.1
                  0 0
                  bullet-v2.2))
(define bullet-v3.1
  (make-cir BULLET-RADIUS-v2.2 eye-col))
(define bullet-v4.1
  (make-cir BULLET-RADIUS-v2.2 eye-col2))
(define bullet-v4
  (overlay/offset bullet-v2.1
                  0 0
                  bullet-v4.1))
(define bullet-v3
  (overlay/offset bullet-v2.1
                  0 0
                  bullet-v3.1))
(define bullet-v33
  (scale/xy 2 2 bullet-v3))
(define bullet-v44
  (scale/xy 2 2 bullet-v4))

(define heart
  (polygon (list (make-pulled-point 2/3 -70 0 0 2/3 70)
                 (make-posn -10 -10)
                 (make-pulled-point 1/2 -40 0 8 1/2 40)
                 (make-posn 10 -10))
           "solid" (make-color 255 0 0 180)))

                     
(define RADIUS
  (image-width bullet-v33))
