;;;; SICP Compatible
#lang scheme
(require rnrs/base-6)
(require rnrs/mutable-pairs-6)

#|
Write vector-filter
|#

;; 一定要檢查過一遍才知道要多大的新 vector
;; 再創造一個 vector 儲存 #t #f
;; 不對，我只要在檢查的時候把 #f 存入，再把不是 #f 寫入
;; AHA! 儲存一格中間變量 list 把檢查到的 #t 放入, 而不是 vector
;; 為了增快速度，使用 cons 往前加入
;; 最後檢查完在 cons 一個檢查到的數量，減少 length(list) 的計算

(define (vector-filter pred vec)
  (define (loop-check i t lst)
    (if (< i 0)
        (cons t lst)
        (if (pred (vector-ref vec i))
            (loop-check (sub1 i) (add1 t) (cons i lst))
            (loop-check (sub1 i) t lst))))
  (define (loop-write newvec i lst)
    (if (null? lst)
        newvec
        (begin (vector-set! newvec i (vector-ref vec (car lst)))
               (loop-write newvec (add1 i) (cdr lst)))))
  (let ((ch-lst  (loop-check (sub1 (vector-length vec)) 0 '())))
    (loop-write (make-vector (car ch-lst)) 0 (cdr ch-lst))))

(define v1 (vector 1 2 3 4 5 6 7 8 9 10))
(vector-filter odd? v1)
(vector-filter even? v1)
