#lang racket/gui

(struct vec (x y))
(struct vec3 (x y z))

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
    [(eq? color "orange") (create-pen 251 150 51 1)]
    [(eq? color "black") (create-pen 0 0 0 1)]
    [else (create-pen 0 255 0 1)]))

(define (read-model file-name)
  (define lines (file->lines file-name #:mode 'text))

  (define vertex-lines (filter (lambda (line) (string-prefix? line "v ")) lines))
  (define face-lines (filter (lambda (line) (string-prefix? line "f ")) lines))

  (define vertices (map (lambda (line)
                          (let ([splits (string-split (string-trim line "v "))])
                            (vec3
                             (string->number (list-ref splits 0))
                             (string->number (list-ref splits 1))
                             (string->number (list-ref splits 2))))) vertex-lines))

  (define faces (map (lambda (line)
                       (let ([splits (string-split (string-trim line "f "))])
                         (map (lambda (split)
                                (let ([face (string-split split "/")])
                                  (- (string->number (list-ref face 0)) 1))) splits))) face-lines))

  (cons vertices faces))

;; load face
(define model (read-model "african_head.obj"))
(define vertices (car model))
(define faces (cdr model))

(new canvas% [parent frame]
             [paint-callback
              (lambda (canvas dc)

                ;; draw background
                (send dc set-pen (pen-from-color "black"))
                (send dc set-brush "black" 'solid)
                (send dc draw-rectangle 0 0 WIDTH HEIGHT)

                ;; draw face
                (send dc set-pen (pen-from-color "orange"))

                (for-each (lambda (face)
                  (for ([i (in-range 0 3)])
                    (let* ([v0 (list-ref vertices (list-ref face i))]
                           [v1 (list-ref vertices (list-ref face (modulo (+ i 1) 3)))]
                           [x0 (/ (* (+ (vec3-x v0) 1) WIDTH) 2)]
                           [y0 (/ (* (+ (vec3-y v0) 1) HEIGHT) 2)]
                           [x1 (/ (* (+ (vec3-x v1) 1) WIDTH) 2)]
                           [y1 (/ (* (+ (vec3-y v1) 1) HEIGHT) 2)])
                      (send dc draw-line x0 y0 x1 y1)))) faces))])

(send frame show #t)
