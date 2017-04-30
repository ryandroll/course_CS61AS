#lang racket
(provide (all-defined-out))

;; quasiquote and unqote
(define a 1)
(define b 2)

`( a  ,b)
; => '(a 2), b依舊是那個變量b

(quasiquote (a (unquote  b)))
; => '(a 2),和`(a ,b)等價

`(,a  ,b)
; => '(1 2),從結果上來看等價於(list ab)

`( a  b)
; => '(ab),從結果上來看等價於(list ' a 'b)

;; pattern match for unknown length list
(match '(+ 2 5 3 4 5)
  [`(+ ,a ,b ...) b])

;; pattern match for calculator
(define (calc-1 exp)
  (match exp
    [(? number? x) x]
    [`(,op ,e1 ,e2 ...)
     (let ([v1 (calc-1 e1)]
           [v2 (map calc-1 e2)])
       (match op
         ['+ (+ v1 (foldr + 0 v2))]
         ['- (- v1 (foldr + 0 v2))]
         ['* (* v1 (foldr * 1 v2))]
         ['/ (/ v1 (foldr * 1 v2))]))]))
