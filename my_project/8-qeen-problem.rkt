#lang racket

;; 8-Queen
(require berkeley)
(provide (all-defined-out))

;; 學到CS61AS Nested Mappings 就可以解決這個問題，而且實作後更了解HOF of list
;; 最大的收穫就是，map一定會自帶一層list無法消除，巢狀map就會一層又一層
;; 要消除嵌套就要靠在map外面的fold類函數，用append消除
;;
;; 解決的方法是首先寫出函數可以創造n個排列組合的list，用map一個個元素加上去
;; 每增加一個就用filter檢查一次合不合規則，就可以把八皇后給做出來了
;; data struct: add new queen to first atom, easy for recursive
;; last line equl to 1

(define (ck-ur-ll atm lst)
  (cond
    ((null? lst) #t)
    ((= atm (add1 (car lst))) #f)
    (else (ck-ur-ll (sub1 atm) (cdr lst)))))

(define (ck-ul-lr atm lst)
  (cond
    ((null? lst) #t)
    ((= atm (sub1 (car lst))) #f)
    (else (ck-ul-lr (add1 atm) (cdr lst)))))

(define (ck-total atm lst)
         (and (not (member? atm lst))
              (ck-ul-lr atm lst)
              (ck-ur-ll atm lst)))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (n-queen n m list-of-list)
  (cond
    ((= n 0) list-of-list)
    (else (n-queen (sub1 n)
                  m
                  (filter (lambda (f) (ck-total (car f) (cdr f)))
                   (flatmap (lambda (x)
                              (map (lambda (y) (append (list x) y))
                                   list-of-list))
                            (enumerate-interval 1 m)))))))

;; map vs flatmap test
(map (lambda (y)
           (map (lambda (x) (cons x (list y)))
                (enumerate-interval 1 4)))
         (enumerate-interval 1 4))
