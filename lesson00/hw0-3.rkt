#lang racket

(require berkeley)
(provide (all-defined-out))

; Exercise 1 - Define describe-time
(define (describe-time secs)
  ; your code here
 (cond ((>= (quotient secs 86400) 1) (se (quotient secs 86400) 'days (describe-time (remainder secs 86400))))
       ((>= (quotient secs 3600) 1) (se (quotient secs 3600) 'hours (describe-time (remainder secs 3600))))
       ((>= (quotient secs 60) 1) (se (quotient secs 60) 'minutes (describe-time (remainder secs 60))))
       ((>= secs 1) (se secs 'seconds))
       (else '())
 )
)

; Exercise 2 - Define remove-once
(define (remove-once wd sent)
  ; your code here
 (if (equal? wd (first sent))
     (bf sent)
     (se (first sent) (remove-once wd (bf sent)))
 )
)

; Exercise 3 - Define differences
(define (differences nums)
  ;your code here
 (if (equal? '() (bf(bf nums)))
     (- (first (bf nums)) (first nums))
     (se (- (first (bf nums)) (first nums)) (differences (bf nums)))
 )
)

; Exercise 4 - Define location
(define (location small big)
  ;your code here
  (define (re small big)
    (cond ((empty? big) 1)
      ((equal? small (first big)) 1)
      (else (+ 1 (re small (bf big))))
    )
  ) 
  (if ( > (re small big) (count big))
    #f
    (re small big)
  )
)

; Exercise 5 - Define initials
(define (initials sent)
  ; your code here
 (if (equal? '() (bf sent))
     (first(first sent))
     (se (first(first sent)) (initials (bf sent)))
 )
)
; Exercise 6 - Define copies
(define (copies num wd)
  ; your code here
 (if (= num 0)
     '()
     (se wd (copies (- num 1) wd))
 )
)

; Exercise 7 - Define gpa
(define (gpa grades)
  ; your code here
 (define (base-grade grade)
   (cond ((equal? 'A (first grade)) 4)
         ((equal? 'B (first grade)) 3)
         ((equal? 'C (first grade)) 2)
         ((equal? 'D (first grade)) 1)
         (else 0)
   )
 )
 (define (grade-modifier grade)
   (cond ((equal? '+ (last grade)) 0.33)
         ((equal? '- (last grade)) -0.33)
         (else 0)
   )
 )
 (define (sumgpa grades)
   (if (empty? grades)
       0
       (+ (base-grade (first grades)) (grade-modifier (first grades)) (sumgpa (bf grades)))
    )
 )
 (/ (sumgpa grades) (count grades))
)

; Exercise 8 - Define repeat-words
(define (repeat-words sent)
  ; your code here
 (define (copies num wd)
   (if (= num 0)
       '()
       (se wd (copies (- num 1) wd))
   )
 )
 (define (smrepeat-words sent)
   (cond ((equal? '() sent) '())
         ((number? (first sent)) (se (copies (first sent) (first (bf sent))) (smrepeat-words (bf (bf sent)))))
         (else (se (first sent) (repeat-words (bf sent))))
   )
 )
 (smrepeat-words sent) 
)

; Exercise 9 - Define same-shape?
(define (same-shape? sent1 sent2)
  ; your code here
 (cond ((and (equal? '() sent1) (equal? '() sent2)) #t)
       ((or (equal? '() sent1) (equal? '() sent2)) #f)
       ((equal? (count (first sent1)) (count (first sent2))) (same-shape? (bf sent1) (bf sent2)))
       (else #f)
 )
)
