#lang racket

(require berkeley)
(provide (all-defined-out))

;Exercise 1
;What are the result of the expressions? Make sure to comment your answer out.


; Exercise 2 Mobile

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

; a. Define left-branch, right-branch, branch-length, and
; branch-structure.

(define (left-branch mobile)
  (error "Not yet implemented"))

(define (right-branch mobile)
  (error "Not yet implemented"))

(define (branch-structure branch)
  (error "Not yet implemented"))

; b. Define total-weight.

(define (total-weight mobile)
  (error "Not yet implemented"))

; c. Define balanced?

(define (balanced? b-mobile)
  (error "Not yet implemented"))

; d. Redefine all the necessary procedures to work with the new
; constructors given below.
; Make sure that only one set of constructors is active at any time
; (otherwise Racket will complain about duplicate defintions).

;; (define (make-mobile left right)
;;   (cons left right))
;; (define (make-branch length structure)
;;   (cons length structure))


;Exercise 3a - Define square-tree

(define (square-tree d-l)
  (cond
    ((null? d-l) '())
    ((number? d-l) (square d-l))
    (else (cons (square-tree (car d-l))
                (square-tree (cdr d-l))))))

;Exercise 3b - Define tree-map
;; Acording to TLS we should ask three questions

(define atom?
  (lambda (x)
    (and (not (pair? x))
         (not (null? x)))))

(define (tree-map fn tree)
  (cond
    ((null? tree) '())
    ((atom? (car tree)) (cons (fn (car tree)) (tree-map fn (cdr tree))))
    (else (cons (tree-map fn (car tree)) (tree-map fn (cdr tree))))))

;Exercise 4 -  Complete the definition of accumulate-n
;; 啊哈，用其他 HOF

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (foldr op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;Exercise 5 - Complete the definitions of matrix-*-vector, transpose,
; and matrix-*-matrix.
;; It's a very good pratice for HOF for list.
;; Matrix to matrix  use the nested-map concept

(define (dot-product v w)
  (foldr + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product v x)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x)
           (map (lambda (y)
                  (dot-product x y))
                cols))
         m)))


;Exercise 6 - Give the property that op should satisfy:

#|

+ * and or 有結合率的東西

|#

;Exercise 7 - Define equal?

(define (my-equal? l1 l2)
  (cond
    ((and (null? l1) (null? l2)) #t)
    ((and (atom? l1) (atom? l2)) (eq? l1 l2))
    ((and (pair? l1) (pair? l2)) (and (my-equal? (car l1) (car l2))
                                      (my-equal? (cdr l1) (cdr l2))))
    (else #f)))

;Exercise 8 - Complete the definition of subsets
;; 用 let + append 其實有點奸詐，不過也不算是有"狀態"

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))


;Exercuse 9 - Modify the calc program

;; Racket calculator -- evaluate simple expressions

; The read-eval-print loop:

(define (calc)
  (display "calc: ")
  (flush-output)
  (print (calc-eval (read)))
  (calc))

; Evaluate an expression:

(define (calc-eval exp)
  (cond ((number? exp) exp)
	((list? exp) (calc-apply (car exp) (map calc-eval (cdr exp))))
	(else (error "Calc: bad expression:" exp))))

; Apply a function to arguments:

(define (calc-apply fn args)
  (cond ((eq? fn '+) (foldr + 0 args))
	((eq? fn '-) (cond ((null? args) (error "Calc: no args to -"))
			   ((= (length args) 1) (- (car args)))
			   (else (- (car args) (foldr + 0 (cdr args))))))
	((eq? fn '*) (foldr * 1 args))
	((eq? fn '/) (cond ((null? args) (error "Calc: no args to /"))
			   ((= (length args) 1) (/ (car args)))
			   (else (/ (car args) (foldr * 1 (cdr args))))))
	(else (error "Calc: bad operator:" fn))))
