#lang racket
(require "../misc.rkt")

(define (flatten* sexp)
  (cond
    ((null? sexp) '())
    ((atom? (car sexp)) (cons (car sexp) (flatten* (cdr sexp))))
    (else (append (flatten* (car sexp)) (flatten* (cdr sexp))))))

(define test '((1 ((2 4) 3)) 3))
(flatten* test)

;; pattern match method
;; '(3) == '(3 ()) match to `(,e1 ,e2)

(define (flatten*-p sexp)
  ;; (displayln sexp)
  (match sexp
    [(? null? sexp) sexp]
    [(? atom? sexp) `(,sexp)]
    [`(,e1 ,e2 ...) (append (flatten*-p e1) (flatten*-p e2))]))

(flatten*-p test)

(define (two-in-a-row-p lst)
  (displayln lst)
  (match lst
    [(? null? lst) #f]
    [`(,e1) #f]
    [`(,e1 ,e2 ...) (or (eq? e1 (car e2)) (two-in-a-row-p e2))]))

(define test-lst '(ao bd dd ww qq ee de))
(two-in-a-row-p test-lst)
