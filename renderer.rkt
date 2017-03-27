#lang racket/gui

(define WIDTH 640)
(define HEIGHT 480)

(define-syntax-rule (swap x y)
  (let ([tmp x])
    (set! x y)
    (set! y tmp)))

(define frame (new frame%
                   [label "hello world"]
                   [width WIDTH]
                   [height HEIGHT]))

(define (create-pen r g b w)
  (new pen% [color (make-color r g b)] [width w]))

(define (pen-from-color color)
  (cond
    [(eq? color "red") (create-pen 255 0 0 1)]
    [(eq? color "black") (create-pen 0 0 0 1)]
    [else (create-pen 0 255 0 1)]))

(define (draw-line dc x0 y0 x1 y1)
  (define steep #false)

  (when (< (abs (- x0 x1)) (abs (- y0 y1)))
    (swap x0 y0)
    (swap x1 y1)
    (set! steep #true))

  (when (> x0 x1)
    (swap x0 x1)
    (swap y0 y1))

  (define dx (- x1 x0))
  (define dy (- y1 y0))
  (define derror-squared (* (abs dy) 2))
  (define error-squared 0)
  (define y y0)

  (send dc set-pen (pen-from-color "red"))

  (for ([x (in-range x0 (+ x1 1))])
    (if 'steep
        (send dc draw-point y x)
        (send dc draw-point x y))

    (set! error-squared (+ error-squared derror-squared))

    (when (> error-squared dx)
      (set! y (+ y (if (> y1 y0) 1 -1)))
      (set! error-squared (- error-squared (* dx 2))))))

(new canvas% [parent frame]
             [paint-callback
              (lambda (canvas dc)
                (send dc set-pen (pen-from-color "black"))
                (send dc set-brush "black" 'solid)
                (send dc draw-rectangle 0 0 WIDTH HEIGHT)
                (draw-line dc 13 20 80 40)
                (draw-line dc 40 50 80 90)
                (draw-line dc 100 10 50 60))])

(send frame show #t)
