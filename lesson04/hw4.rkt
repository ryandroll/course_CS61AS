#lang racket

(require berkeley)
(provide (all-defined-out))

; Exercise 1
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (make-interval a b) (cons a b))

; SICP 2.7 - Define upper-bound and lower-bound
(define (upper-bound interval)
  (if (> (car interval) (cdr interval))
      (car interval)
      (cdr interval)
  )
)

(define (lower-bound interval)
  (if (< (car interval) (cdr interval))
      (car interval)
      (cdr interval)
  )
)

; SICP 2.8 - Define sub-interval
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))
                 )
  )



; SICP 2.10 - Modify div-interval



; SICP 2.12 - Define make-center-percent and percent


; SICP 2.17 - Define last-pair
(define (last-pair lst)
  (cond
    ((null? (cdr lst)) (car lst))
    (else (last-pair (cdr lst)))))

; SICP 2.20 - Define same-parity
(define (same-parity . args)
  (if (even? (car args))
      (filter even? args)
      (filter odd? args)
      )
  )

; SICP 2.22 - Write your explanation in the comment block:

#|
Your explanation here
|#

; Exercise 2 - Define my-substitute
;; 明明在 tls 寫過，竟然忘記了，注意規則一與四
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define (substitute lst old new)
  (define (inner-rec e)
    (cond ((null? e) null)
          ((atom? (car e))
           (cond
             ((eq? old (car e)) (cons new (inner-rec (cdr e))))
             (else (cons (car e) (inner-rec (cdr e))))))
          (else (cons (inner-rec (car e)) (inner-rec (cdr e))))))
  (inner-rec lst))

; Exercise 3 - Define my-substitute2
;; 直接引用
(define (substitute2 lst old new)
  (cond
    ((null? old) lst)
    (else (substitute2 (substitute lst (car old) (car new))
                       (cdr old)
                       (cdr new)))))

; Exercise 4
;; 果然要顯示傳遞變數
;; 關鍵在於 command 跟 lst 都會隨時間變化，然後捕捉引數，把參數傳進去
; Exercise 5
(define (cxr-function command)
  (define (cxr-rec command lst)
    (cond
      ((eq? (first command) 'r) lst)
      ((eq? (first command) 'a) (car (cxr-rec (bf command) lst)))
      ((eq? (first command) 'd) (cdr (cxr-rec (bf command) lst)))
      (else (cxr-rec (bf command) lst))))
  (lambda (lst) (cxr-rec command lst)))

; Exercise 6
;; 想法實在太冗長
(define (last-my lst)
  (cond
    ((null? (cdr lst)) (car lst))
    (else (last-my (cdr lst)))))

(define (blast-my lst)
  (cond
    ((null? (cdr lst)) '())
    (else (cons (car lst) (blast-my (cdr lst))))))

(define (reverse-my-1 lst)
  (cond
    ((null? lst) '())
    ((null? (cdr lst)) lst)
    (else (append (cons (last lst) (reverse-my-1 (cdr (blast-my lst))))
                  (cons (car lst) '())))))

;; 這個方法好！！
;; 麻煩還是在空指標與nil的差別，可以用list不用cons
(define (reverse1 l)
  (if (null? l)
      nil
      (append (reverse1 (cdr l)) (list (car l)))))

(define (reverse-my-2 lst)
  (foldl cons '() lst))

; ((cxr-function 'caadaar) '(((sub (here there)) sub sub here) (((sub)))))
; (cadaar '(((sub (here there)) sub sub here) (((sub))))) ; 4 maximum
