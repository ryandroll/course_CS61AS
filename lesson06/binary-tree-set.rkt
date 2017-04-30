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

;; 接下來把 binary tree 轉成 list，先不管 SICP 怎麼寫
;; 以前學過 divide and conquer 利用這個想法，自己把它寫下來

;; Combine two sorted list

(define (combine-sorted lst1 lst2)
  (define (combine-recur lst1 lst2 new-list)
    (cond
      ((null? lst1) (append new-list lst2))
      ((null? lst2) (append new-list lst1))
      ((<= (car lst1) (car lst2)) (combine-recur
                                   (cdr lst1)
                                   lst2
                                   (append new-list (list (car lst1)))))
      ((> (car lst1) (car lst2)) (combine-recur
                                  lst1
                                  (cdr lst2)
                                  (append new-list (list (car lst2)))))))
  (combine-recur lst1 lst2 '()))

(define (tree->list tree)
  (cond
    ((null? tree) '())
    (else (combine-sorted (tree->list (left-branch tree))
                          (cons (entry tree) (tree->list (right-branch tree)))))))

(define (list->tree lst)
  (cond
    ((null? lst) '())
    (else (let ((mid (quotient (length lst) 2)))
            (make-tree (list-ref lst mid)
                       (list->tree (take lst mid))
                       (list->tree (drop lst (add1 mid))))))))

;; test
(tree->list (list->tree (build-list 10 values)))

;; list version

(define (intersection-sorted lst1 lst2)
  (define (inter-recur lst1 lst2 new-lst)
    (cond
      ((or (null? lst1) (null? lst2)) new-lst)
      ((= (car lst1) (car lst2))(inter-recur
                                 (cdr lst1)
                                 (cdr lst2)
                                 (append new-lst (list (car lst1)))))
      ((< (car lst1) (car lst2)) (inter-recur (cdr lst1) lst2 new-lst))
      ((> (car lst1) (car lst2)) (inter-recur lst1 (cdr lst2) new-lst))))
  (inter-recur lst1 lst2 '()))

(intersection-sorted '(1 2 3 4 5) '(3 4 5 6 7))
