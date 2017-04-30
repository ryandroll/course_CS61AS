#lang racket

;; A line starting with a semicolon is a "comment".  You write
;; comments in order to explain in English what your code does, and
;; Racket knows to ignore comments since they aren't part of the
;; program.

;; This tells Racket that you want to use words and sentences (which
;; are disabled by default).
(require berkeley)

;; This tells Racket that it should "know" about all the functions you
;; define in this file.  (Don't worry about this for now.)
(provide (all-defined-out))

;; Exercise 0 - Introduce yourself

#|

This is a comment that spans multiple lines.

1) What is your name?

2) What is your major?

3) Are you a returning student? (i.e. Did you take 61AS last semester?)

4) What made you to take 61AS?

5) Tell us interesting things about yourself.

|#

;; Make a followup on the "Hello World!" post on Piazza introducing yourself.


;; Exercise 1 - Define sum-of-squares
(define (square x) (* x x))

(define (sum-of-squares a b)
  (+ (square a) (square b)))


;; Exercise 2a - Define can-drive

(define (can-drive a)
  (if (< a 16)
      '(Not yet)
      '(Good to go)
  )
)

;; Exercise 2b - Define fizzbuzz
(define (fizzbuzz n)
  (cond ((and (= (remainder n 3) 0) (= (remainder n 5) 0)) 'fizzbuzz)
        ((= (remainder n 3) 0) 'fizz)
        ((= (remainder n 5) 0) 'buzz)
        (else n)))

;; Exercise 3 - Why did the Walrus cross the Serengeti?

#|
Your answer here


|#

(define (infinite-loop infinite-loop)
  (if (= 3 6)
  (/ 4 2)
  (infinite-loop)
  ))


;; Exercise 4 - new-if vs if

#|
Your answer here

|#