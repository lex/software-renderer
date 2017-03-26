#lang racket/gui

(define frame (new frame%
                   [label "hello world"]
                   [width 640]
                   [height 480]))

(new canvas% [parent frame]
             [paint-callback
              (lambda (canvas dc)
                (send dc set-scale 3 3)
                (send dc set-text-foreground "red")
                (send dc draw-text "hello world" 0 0))])

(send frame show #t)