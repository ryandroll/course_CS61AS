#lang racket

(require berkeley)
(provide (all-defined-out))

; Exercise 1 - Define dupls-removed
(define (dupls-removed sent)
  (cond ((empty? sent) '())
        ((member? (car sent) (cdr sent)) (dupls-removed (cdr sent)))
        (else (cons (car sent) (dupls-removed (cdr sent))))))

; Exercise 2 - Define count-word
(define (count-word sent wd)
  (cond
    ((null? sent) 0)
    ((equal? (car sent) wd) (+ 1 (count-word (cdr sent) wd)))
    (else (count-word (cdr sent) wd))))

; Exercise 3
(define (pigl wd)
  (if (pl-done? wd)
      (word wd 'ay)
      (pigl (word (bf wd) (first wd)))))

(define (pl-done? wd)
  (vowel? (first wd)))

(define (vowel? letter)
  (member? letter '(a e i o u)))

; Explain what would happen if you used new-if instead of if below.
#|
Your explanation here

|#

; Exercise 4 - Define squares
(define (squares sent)
  ; Your code here
  (cond
    ((null? sent) '())
    (else (cons ((lambda (x) (* x x)) (car sent))
                (squares (cdr sent))))))

; Exercise 5 - Define switch
(define (switch sent)
  (define (small-switch sent)
    (cond
      ((null? sent) '())
      ((or (eq? 'I (car sent)) (eq? 'me (car sent))) (cons 'you (small-switch (cdr sent))))
      ((eq? 'you (car sent)) (cons 'me (small-switch (cdr sent))))
      (else (cons (car sent) (small-switch (cdr sent))))))
  (if (eq? (car sent) 'you)
      (cons 'I (small-switch (cdr sent)))
      (small-switch sent))
  )

; Exercise 6 - Define ordered?
(define (ordered? sent)
  (define (small-orderd? fst lst)
    (cond
      ((null? lst) #t)
      ((> (car lst) fst) (small-orderd? (car lst) (cdr lst)))
      (else #f)))
  (small-orderd? (car sent) (cdr sent)))

; Exercise 7 - Define ends-e
(define (ends-e sent)
  (cond
    ((null? sent) '())
    ((eq? 'e (last (car sent))) (cons (car sent) (ends-e (cdr sent))))
    (else (ends-e (cdr sent)))))


; Exercise 8

#|

Your explanation here

|#
