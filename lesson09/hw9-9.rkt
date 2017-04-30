;;;; SICP Compatible
#lang racket
(require r5rs)
(require "../misc.scm")
;; (require berkeley)
(require (only-in berkeley word))

#|
Write the procedure cxr-name.
Its argument will be a function made by composing cars and cdrs.
It should return the appropriate name for that function:

> (cxr-name (lambda (x) (cadr (cddar (cadar x)))))
CADDDAADAR
|#


;; support treat function as blackbox, can't use rex
;; find with binary-tree

(define x (cons 'a 'd))



(define (expand-tree tree)
  (set-car! tree (cons (word 'a (car tree))
                       (word 'd (car tree))))
  (set-cdr! tree (cons (word 'a (cdr tree))
                       (word 'd (cdr tree)))))

#|
(define (test-tree n)
  (define (recu-tree n tree)
    (cond
      ((= n 1) (tree))
      (else (test-tree (- n 1)
                       (set-car! tree (cons ))))
      )))
|#
