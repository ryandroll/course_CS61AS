#lang racket

(require berkeley)
(provide (all-defined-out))

; Exercise 1 - Define fast-expt-iter

(define (fast-expt-iter-helper b n a)
  (cond ((= n 0) a)
        ((even? n) (fast-expt-iter-helper (square b) (/ n 2) a))
        ((odd? n) (fast-expt-iter-helper b (- n 1) (* a b)))
  )
)

(define (fast-expt-iter b n)
  (fast-expt-iter-helper b n 1)
)

; Exericse 2 - Define phi
#|

(define (phi)
  (fixed-point (lambda (x) (+ 1 (/ 1 x))))
)

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
  (cont-frac N D k)        next
          (try next))))
  (try first-guess))
|#
; Exercise 3 - Define cont-frac

;; Recursive version
#|
;; 我寫得答案，比較不好一點，沒有善用閉包與一級函數
;; 絲路大致上是正確的，我們只能先確定最後一個值，而不是第一個值，所以要再創要一個函數
;; 但是我沒有用一級函數傳遞正確的 N D
;; 基本上用閉包的概念他會向外尋找參數，不用再把它包進去
(define (cont-frac N D k)
  (define (cont-frac-rev N D k n)
    (cond
      ((= n k) (/ N D))
      (else (/ N (+ D (cont-frac-rev N D k (add1 n)))))))
  (cont-frac-rev N D k 1))
|#

;;
(define (cont-frac N D k)
  (define (cf i)
    (cond
      ((= k i) (/ (N k) (D k)))
      (else (/ (N i)
               (+ (D i) (cf (+ i 1.0)))))))
  (cf 1))

;; Iterative version
;; 其實在 scheme 中的 interative 有點難捕捉它的意思，因為還是用遞迴啊！
;; 我只能試圖理解它，就是除了interative改變，還有個中間 answer 值。
;; 到了現在加減乘除還是習慣中間表達式

(define (cont-frac-iter n d k)
  (define (cf-i counter answer)
    (cond
      ((= counter k) answer)
      (else (cf-i (+ counter 1)
                  (/ (n counter)
                     (+ (d counter) answer))))))
  (cf-i 1 (/ (n k) (d k))))

(define (e k)
  (define (d i)
    (cond
      ((= (remainder (+ i 1) 3) 0) (* 2 (+ (quotient i 3) 1)))
      ((= (remainder (+ i 1) 3) 1) 1)
      ((= (remainder (+ i 1) 3) 2) 1)))
  (+ (cont-frac (lambda (i) 1.0) d k) 2))




; Exercise 4 - Define next-perf
#|
(define (list-of-factors n)
  (define (lof k)
    (cond
      ((= k 1) '(1))
      ((= (remainder n k) 0) (cons k (lof (- k 1))))
      (else (lof (- k 1)))))
  (lof (quotient n 2)))

(define (sum l)
  (cond
    ((null? l) 0)
    (else (+ (car l) (sum (cdr l))))))

(define (next-perf n)
  (define (np k ans)
    (cond
      ((and (= k n) (= ans (sum (list-of-factors ans)))) ans)
      ((= ans (sum (list-of-factors ans))) (np (+ k 1) (+ ans 1)))
      (else (np k (+ ans 1)))))
  (np 1 6))
|#

(define (next-perf n)
  (define (n-p k idx)
    (cond
      ((and (= k n) (prime? (- (expt 2 (+ idx 1)) 1))) (* (- (expt 2 (+ idx 1)) 1)
                                                          (expt 2 idx)))
      ((prime? (- (expt 2 (+ idx 1)) 1)) (n-p (+ 1 k) (+ 1 idx)))
      (else (n-p k (+ 1 idx)))))
  (n-p 1 1))


; Exercise 5 - Explain what happens when the base cases are interchanged.

#|

Your explanation here

|#

; Exercise 6 - Give a formula relating b, n, counter and product in expt-iter.

#|

Formula for expt:

Formula for expt-iter:

|#
