(load "obj.scm")

; 1 - Modify the person class.
;; 看得我頭有點暈，好像有點搞懂了
;; 重點在於要再做創一個 local state
;; 然後注意 ask 跟 greet 都是在呼叫 say 這個方法，
;; 再用 ask 的呼叫過程中都要加 self 表示
;; 然後 'say 後的 () 括弧都是 say 方法中 stuff 的變量
;; 此 stuff 非 ask 中的 sutff

(define-class (person name)
  (instance-vars (last-thing '()))
  (method (repeat) last-thing)
  (method (say stuff)
          (set! last-thing stuff)
          stuff)
  (method (ask stuff) (ask self 'say (se '(would you please) stuff)))
  (method (greet) (ask self 'say (se '(hello my name is) name))))

; 2 - Determine which definition works as intended.
; In particular, make sure the repeat method works.
#|
(define-class (double-talker name)
  (parent (person name))
  (method (say stuff) (se (usual 'say stuff) (ask self 'repeat))) )
|#
#|
(define-class (double-talker name)
  (parent (person name))
  (method (say stuff) (se stuff stuff)) )

|#
(define-class (double-talker name)
  (parent (person name))
  (method (say stuff) (usual 'say (se stuff stuff))) )

#||#

#|
Definition number ?? works as intended.
Your explanation here.
第三個：因為要用 usual 複寫原來的 'say  其他函數才有作用
第二個使用其他方法會沒有有辦法使用
|#


; 3 - Write the random-generator class.
(define-class (random-generator range)
  (instance-vars (counting 0))
  (method (number)
          (set! counting (+ counting 1))
          (random range))
  (method (count) counting))

(define r10 (instantiate random-generator 10))

; 4 - Write the coke-machine class.
; For compatibility with the autograder, make sure that you display
; error messages.  That means you should say:
; (display "Not enough money") and
; (display "Machine empty") when appropriate.

(define-class (coke-machine capacity price)
  (instance-vars (cock-left 0) (cent-in 0))
  (method (fill coke) (set! coke (+ cock-left coke)))
  (method (deposit cent) (set! cent-in (+ cent-in cent)))
  (method (coke) (cond
                  ((<= capacity 0) (display "Machine empty\n"))
                  ((< cent-in price) (display "Not enough money\n"))
                  (else (begin
                          (display (- cent-in price))
                          (set! cock-left (- cock-left 1))
                          (set! cent-in 0))))))

;; Test
(define my-machine (instantiate coke-machine 80 70))
(ask my-machine 'fill 60)
(ask my-machine 'deposit 25)
(ask my-machine 'coke)
(ask my-machine 'deposit 25)
(ask my-machine 'deposit 25)
(ask my-machine 'coke)
;; return val is 5 cents change.

; 5 - Write the deck class.

(define ordered-deck
  (accumulate append '()
	      (map (lambda (suit)
		     (map (lambda (value) (word value suit))
			  '(A 2 3 4 5 6 7 8 9 10 J Q K)))
		   '(s d c h))))

(define (shuffle deck)
  (if (null? deck)
      '()
      (let ((card (nth (random (length deck)) deck)))
	(cons card (shuffle (remove card deck))) )))


; 6 - Write the miss-manners class.
#|
(instance-vars (counting 0))
  (method (number)
          (set! counting (+ counting 1))
          ())
(instance-vars (counting 0))
  (method (number)
          (set! counting (+ counting 1))
          ())
|#
