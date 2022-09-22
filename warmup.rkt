#lang racket
;;
;; CIS352 (Spring '22) Racket Warmup
;;
(provide (all-defined-out)) ;; export all function names to tests

(define (square x) (* x x))

;; return the Euclidian distance between x0,y0 and x1,y1
(define (euclid-distance x0 y0 x1 y1)
  (sqrt(+(sqr(- x1 x0))
         (sqr(- y1 y0)))))

;; return #t iff the list l contains a number less than i
(define (contains-less-than l i)
  (match l
    ['() #f]
    [`(,a . ,b) #:when (< a i) #t]
    [`(,a . ,b) #f]))

;; valid "shapes" are (a) either rectangles with a lower-left x0/y0
;; and upper-right x1/y1, (b) circles with some center x/y and radius
;; y. 
(define (shape? s)
  (match s
    [`(circle ,x-enter ,y-center ,radius) #t]
    [`(rect ,x0 ,y0 ,x1 ,y1) (and (< x0 x1) (< y0 y1))]
    [_ #f]))

;; Calculate the area of a shape.
(define (shape-area s)
  (match s
    ;; hint: use pi
    [`(circle ,x-center ,y-center ,radius) (* 3.14 radius radius)]
    [`(rect ,x0 ,y0 ,x1 ,y1) (*(- x0 x1)(- y0 y1))]))

;; generate n spaces
(define (spaces n) (make-string n #\space))

;; Assume `l` is a list of cons cells (pairs) whose first element is a
;; column position (natural number) and whose right element is a
;; character to be rendered at that line. You will return a string
;; representing the line.
;;
;; Example:
;;    (draw-ascii-line '((3 . #\=) (4 . #\=) (5 . #\=) (7 . #\.)
;;                       (9 . #\=) (10 . #\=) (11 . #\=)))
;; > "   === . ==="
(define (draw-ascii-line l)
  ;; lst is a list of the remaining cons cells to be printed, e.g.,
  ;; '((3 . X) (4 . =) (5 . X))
  ;; This is a "helper" function
  (define (draw-next lst cur-pos)
    #|(if (empty? cur-pos)
          '()
          (if (lst (car cur-pos)))
             (cons (car cur-pos) (draw-next (cdr cur-pos)))))|#
             
    #|(cond [(empty? cur-pos) '()]
          (else
          [(lst (car cur-pos))]
          [cons(car cur-pos) (map lst(cdr cur-pos))]
          [else 'unknown]))|#

    (cdr (car lst)))
 

  ;; sort ascending, comparing only the column position by specifying
  ;; a comparison operator.
  (define sorted (sort l (lambda (x y) (< (car x) (car y)))))
  (draw-next sorted 0))
