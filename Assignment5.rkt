#lang racket
(require 2htdp/image)




(define (iota n)
  (if (= n 1)
      '(1)
      (append (iota (- n 1)) (list n))))


(define (same n obj)
  (cond
    [(zero? n) '()]
    [else (cons obj (same (- n 1) obj))]))


(define (alternator n lst)
  (if (= n 0)
      '()
      (cons (car lst) (alternator (- n 1) (append (cdr lst) (list (car lst)))))))


(define (sequence n x)
  (map (lambda (i) (* i x)) (iota n)))


(define (a-count lst)
  (cond
    [(empty? lst) empty]
    [else
      (append (iota (first lst))
              (a-count (rest lst)))]))


(define (r-count lst)
  (cond ((null? lst) '())
        (else (append (make-list (car lst) (car lst))
                      (r-count (cdr lst))))))



(define (zip lst1 lst2)
  (cond ((or (null? lst1) (null? lst2)) '())
        (else (cons (cons (car lst1) (car lst2))
                    (zip (cdr lst1) (cdr lst2))))))


(define (assoc obj a-list)
  (cond ((null? a-list) '())
        ((eq? obj (caar a-list)) (car a-list))
        (else (assoc obj (cdr a-list)))))



( define scale-zip-CM
( zip ( iota 7 ) '("C" "D" "E" "F" "G" "A" "B") )
)
( define scale-zip-short-Am
( zip ( iota 7 ) '("A/2" "B/2" "C/2" "D/2" "E/2" "F/2" "G/2") )
)
( define scale-zip-short-low-Am
( zip ( iota 7 ) '("A,/2" "B,/2" "C,/2" "D,/2" "E,/2" "F,/2" "G,/2") )
)
( define scale-zip-short-low-blues-Dm
( zip ( iota 7 ) '( "D,/2" "F,/2" "G,/2" "_A,/2" "A,/2" "c,/2" "d,/2" ) )
)
( define scale-zip-wholetone-C
( zip ( iota 7 ) '("C" "D" "E" "^F" "^G" "^A" "c") )
)


(define (nr->note n scale)
  (cdr (assoc n scale)))


(define (nrs->notes lst assoc-list)
  (map (lambda (n) (cdr (assoc n assoc-list))) lst))


(define (nrs->abc nl al)
  (string-join (nrs->notes nl al) " "))



(define (stella alist)
  (foldr overlay empty-image
         (map (lambda (square-info)
                (square (car square-info) 'solid (cdr square-info)))
              alist)))





(define pitch-classes '(c d e f g a b))
(define color-names '(blue green brown purple red gold orange))

(define (box color)
  (overlay (square 30 'solid color)
           (square 35 'solid 'black)))

(define boxes
  (list (box "blue")
        (box "green")
        (box "brown")
        (box "purple")
        (box "red")
        (box "gold")
        (box "orange")))

(define pc-a-list (zip pitch-classes color-names))
(define cb-a-list (zip color-names boxes))

(define (pc->color pc)
  (cdr (assoc pc pc-a-list)))

(define (color->box color)
  (cdr (assoc color cb-a-list)))

(define (play pitches)
  (foldr beside empty-image
         (map (lambda (c) (color->box c))
              (map (lambda (pitch) (pc->color pitch))
                   pitches))))

(define AI (text "A" 36 "orange"))
(define BI (text "B" 36 "red"))
(define CI (text "C" 36 "blue"))
(define DI (text "D" 36 "green"))
(define EI (text "E" 36 "purple"))
(define FI (text "F" 36 "brown"))
(define GI (text "G" 36 "magenta"))
(define HI (text "H" 36 "gold"))
(define II (text "I" 36 "teal"))
(define JI (text "J" 36 "pink"))
(define KI (text "K" 36 "turquoise"))
(define LI (text "L" 36 "darkorange"))
(define MI (text "M" 36 "orchid"))
(define NI (text "N" 36 "crimson"))
(define OI (text "O" 36 "black"))
(define PI (text "P" 36 "indianred"))
(define QI (text "Q" 36 "deepskyblue"))
(define RI (text "R" 36 "olive"))
(define SI (text "S" 36 "steelblue"))
(define TI (text "T" 36 "plum"))
(define UI (text "U" 36 "grey"))
(define VI (text "V" 36 "sienna"))
(define WI (text "W" 36 "cadetblue"))
(define XI (text "X" 36 "yellowgreen"))
(define YI (text "Y" 36 "maroon"))
(define ZI (text "Z" 36 "palegoldenrod"))


( define alphabet '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z) )
( define alphapic ( list AI BI CI DI EI FI GI HI II JI KI LI MI NI OI PI QI RI SI TI UI VI WI XI YI
ZI ) )

( define a->i ( zip alphabet alphapic ) )
( define ( letter->image letter )
 ( cdr ( assoc letter a->i ) )
)

(define (gcs lst)
  (foldr beside empty-image (map (lambda (l) (letter->image l)) lst)))



