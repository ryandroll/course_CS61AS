#lang racket

(require berkeley)
(provide (all-defined-out))

;Exercise 0
;Write 5 expressions whose values are the number ten:
;1. Atom
10
;2. Compound Expression (3 Atoms)
(+ 3 7)
;3. Compound Expression (4 Atoms)
(+ 1 3 6)
;4. Compound Expression (1 Atom and 2 subexpressions)
(+ (+ 1 2) (+ 3 4))
;5. Any Other Kind Expression
(+ (* 2 3) 4)

;Exercise 1
(define (second wd)
  (first (bf wd)))

;1. Define first-two
(define (first-two wd)
  ; your code here
  (word (first wd)
        (first (bf wd))
  )
)

;;2. Define two-first
(define (two-first x y)
  ; your code here
  (word (first x)
        (first y)
  )
)

;;3. Define two-first-sent
(define (two-first-sent sent)
  ; your code here
  (word (first (first sent))
        (first (first (bf sent)))
  )
)

;Exercise 2 - Define teen?
(define (teen? num)
  ; your code here
  (if (and (>= num 13) (<= num 19))
      #t
      #f
  )
)

;Exercise 3 - Define indef-article
(define (indef-article wd)
  (if (member? (first wd) 'aeiou)
      (sentence 'an wd)
      (sentence 'a wd)
  )
)

;Exercise 4 - Define insert-and
(define (insert-and sent)
  ; your code here
  (se (bl sent) 'and (last sent))
)

;Exercise 5 - Define query
(define (query sent)
  ; your code here
  (se (first (bf sent))
      (first sent)
      (bl (bf (bf sent)))
      (word (last sent) '?))
)

;Exercise 6 - Define european-time and american-time
(define (european-time time)
  ; your code here
  (cond ((and (equal? '(am) (bf time)) (equal? '12 (first time))) '0)
        ((equal? '(am) (bf time)) (first time))
        ((and (equal? '(pm) (bf time)) (equal? '12 (first time))) '12)
        ((equal? '(pm) (bf time)) (+ '12 (first time)))
  )
)

(define (american-time time)
  ; your code here
  (cond ((= time 0) (se '12 'am))
        ((and(> time 0) (< time 12)) (se time 'am))
        ((= time 12) (se time 'pm))
        ((and(> time 12) (< time 24)) (se (- time 12) 'pm))
        (else 'huh?)
  )
)

;Exercise 7 - Define describe-time
(define (describe-time secs)
  ; your code here
  (cond ((< (/ secs 60) 1) (se secs 'seconds))
        ((< (/ secs 3600) 1) (if (eq? (remainder secs 60) 0)
                                      (se (/ secs 60) 'minutes)
                                      (se (/ (+ secs 0.0) 60) 'minutes)
                                   ))
        ((< (/ secs 86400) 1) (if (eq? (remainder secs 3600) 0)
                                      (se (/ secs 3600) 'hours)
                                      (se (/ (+ secs 0.0) 3600) 'hours)
                                   ))        
        ((< (/ secs 31557600) 1) (if (eq? (remainder secs 86400) 0)
                                      (se (/ secs 86400) 'days)
                                      (se (/ (+ secs 0.0) 86400) 'days)
                                   ))
        (else 'huh?)
  )
)

;Exercise 8 - Explain why superlative doesnt work:
(define (superlative adjective wd)
  (se (word adjective 'est) wd))

#|

Explanation here.
"word" will be binded to the new variable name rather than the orginal function
use the other name to bind the variable 

|#