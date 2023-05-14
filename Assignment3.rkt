#lang racket
(require 2htdp/image)


(define (count-down n)
  (if (> n 0)
      (begin
        (displayln n)
        (count-down (- n 1)))
      (displayln "End")
      )
  )

 

(define (count-up n)
  (if (> n 0)
      (begin
        (count-up (- n 1))
        (displayln n))
      (displayln "Start")
      )
  )

(define (line-of-stars n)
  (cond
    ((> n 0)
     (display "*")
     (line-of-stars (- n 1))
     
     )
    )
 )

(define (triangle-of-stars n)
  (cond
    ((> n 0)
    (triangle-of-stars (- n 1))
    (line-of-stars n)
    (display "\n")
    )
  )
 )


(define (flip-for-difference difference)
  (define (flip-helper diff-so-far flips)
    (define outcome (if (equal? (random 2) 0) #\h #\t))
    (set! flips (cons outcome flips))
    (if (= (abs (+ diff-so-far (if (char=? outcome #\h) 1 -1))) difference)
        (reverse flips)
        (flip-helper (+ diff-so-far (if (char=? outcome #\h) 1 -1)) flips)))
  (list->string (reverse (flip-helper 0 '()))))


(define (ccr radius difference)
  (cond ((> radius 0)
         (define (rgb) (random 0 256))
         (define (rc) (color (rgb) (rgb) (rgb)))
         (overlay (ccr (- radius difference) difference) (circle radius 'solid (rc))))
        ((= radius 0) empty-image)))


(define (cca radius difference color1 color2)
  (cond ((> radius 0)
         (let* ((current-color (if (even? (quotient radius difference)) color1 color2))
                (current-circle (circle radius 'solid current-color)))
           (overlay (cca (- radius difference) difference color1 color2)
                    current-circle)))
        ((= radius 0) empty-image)))


(define (ccs radius difference colors)
  (cond ((> radius 0)
         (let* ((current-color (list-ref colors (random (length colors))))
                (current-radius (+ radius (random (- difference) difference)))
                (current-circle (circle current-radius 'solid current-color)))
           (overlay (ccs (- radius difference) difference colors)
                    current-circle)))
        ((= radius 0) empty-image)))

( define ( random-color-tile )
   ( overlay
     ( square 40 "outline" "black" )
     ( square 40 "solid" ( random-color ) )
     )
)

( define ( random-color )
   ( color
     ( rgb-value ) ( rgb-value ) ( rgb-value )
     )
)

( define ( rgb-value ) ( random 256 ) )


(define (square-of-tiles n square)
  (cond
    [(<= n 0) empty-image]
    [else (overlay/align "left" "top"
            (square-of-tiles (- n 1) square)
            (random-color-tile square square))]))