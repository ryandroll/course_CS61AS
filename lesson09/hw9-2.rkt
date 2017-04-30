;;;; SICP Compatible
#lang scheme
(require rnrs/base-6)
(require rnrs/mutable-pairs-6)

;; #lang racket
;; (require r5rs)
;; (require "../misc.scm")
;; (require berkeley)

#|
> (define list1 (list (list 'a) 'b))
list1

> (define list2 (list (list 'x) 'y))
list2

> (set-cdr! ________ ________)
okay

> (set-cdr! ________ ________)
okay

> list1
((a x b) b)

> list2
((x b) y)
|#

(define list1 (list (list 'a) 'b))
(define list2 (list (list 'x) 'y))

(set-cdr! (car list2) (cdr list1))
(set-cdr! (car list1) (car list2))
