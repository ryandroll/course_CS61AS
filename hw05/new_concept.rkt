#lang racket

(require berkeley)
(provide (all-defined-out))

;Exercise 8 - Complete the definition of subsets
;; 沒接觸過這遞迴形式，感覺有點奇特
;; 用 let + append 其實有點奸詐，好像是把把”狀態“化為參數給隱藏起來，這樣算是迭代風嗎？

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

;; 如果一般我的寫法會是這樣，注意其中重複的部分
;; 作為比較，這個 let 與下面參數傳遞的方法是等價的，其實只保留 car 對 cdr 沒做任何處理

(define (subsets-my s)
  (define (subset-rec rest s)
    (cond
      ((null? s) rest)
      (else (subset-rec (append rest (map (lambda (x) (cons (car s) x)) rest)) (cdr s)))))
  (subset-rec '(()) s))

;; 啊哈，反轉list可用

(define (reverse-my lst)
  (cond
    ((null? lst) '())
    (else (let ((rev-lst (reverse-my (cdr lst))))
            (append rev-lst (list (car lst)))))))
