;;;; SICP Compatible
#lang racket
(require r5rs)
(require "../misc.scm")

#|
(define x (cons 1 2))

(set-car! x 2)

(set-cdr! x 3)
|#
(define x (cons '(a b) '(c d)))
(define y '(e f))

(define z (cons y (cdr x)))

(set-cdr! (cdr z) nil)
