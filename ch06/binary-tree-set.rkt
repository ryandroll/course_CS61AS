#lang racket
(require berkeley)
(provide (all-defined-out))

;; binary-tree function

(define (entry tree)
  (car tree))

(define (left-branch tree)
  (cadr tree))

(define (right-branch tree)
  (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond
    ((null? set) #f)
    ((= x (entry set)) #t)
    ((< x (entry set)) (element-of-set? x (left-branch set)))
    ((> x (entry set )) (element-of-set? x (right-branch set)))))

(define (adjoint-set x set)
  (cond
    ((null? set) (make-tree x '() '()))
    ((= x (entry set)) set)
    ((< x (entry set)) (make-tree
                        (entry set)
                        (adjoint-set x (left-branch set))
                        (right-branch set)))
    ((> x (entry set)) (make-tree
                        (entry set)
                        (left-branch set)
                        (adjoint-set x (right-branch set))))))

;; union of union 好像蠻聰明
(define (union-set set1 set2)
  (cond
    ((null? set2) set1)
    (else (union-set (union-set (adjoint-set (entry set2) set1)
                                (left-branch set2))
                     (right-branch set2)))))


(define set1 '(4
               (3 () ())
               (5 () ())))

(define set2 '(5
               (3 () ())
               (8 () ())))
