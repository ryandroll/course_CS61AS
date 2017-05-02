;;;; SICP Compatible
#lang racket
(require berkeley)

#|
Write the procedure cxr-name.
Its argument will be a function made by composing cars and cdrs.
It should return the appropriate name for that function:

> (cxr-name (lambda (x) (cadr (cddar (cadar x)))))
CADDDAADAR
|#

;; 想法一：
;; support treat function as blackbox, can't use rex
;; find with binary-tree
;; 想法二：
;; 利用流的概念？產生無窮無盡的表，然後根據位址回傳值
;; 簡單版？ cd?r
;; 想法三：
;; 某種 環型結構，可以包容無限的 car cdr 然後再想辦法把值傳遞出來
;; 無解＠＠還型結構跨過去無法求值，也無法計數
;; 想法四：
;; 利用錯誤處理發現錯誤，然後利用錯誤資訊

;; 輸入 cxr 字串，輸出list
;; 該 list 執行等效 cxr 時，不會拋出錯誤且結果為 '()

(define (cxr->lst* cxr-str)
  (cond
    ((eq? cxr-str 'c) '())
    ((eq? (last cxr-str) 'd) (cons '() (cxr->lst* (bl cxr-str))))
    ((eq? (last cxr-str) 'a) (cons (cxr->lst* (bl cxr-str)) '()))
    ((eq? (last cxr-str) 'r) (cxr->lst* (bl cxr-str)))))

;; 輸入兩個 cxr 字串，輸出一個等效的 cxr 字串
;; (cxr1 (cxr2 lst)) == (cxr3 lst)

(define (append-cxr cxr1 cxr2)
  (word 'c
        (bf (bl cxr1))
        (bf (bl cxr2))
        'r))

;; 捕捉例外訊息，利用拋出錯誤會顯示出處理失敗的 cxr 函數
(define (cxr-name fn)
  (define (loop cxr)
    (with-handlers
      ([exn:fail? (lambda (exn)
                    (let* ((exn-m (exn-message exn))
                           (add-cxr (regexp-match #px"c.{1,4}r" exn-m))
                           (new-cxr (append-cxr (car add-cxr) cxr)))
                      (loop new-cxr)))])
      (begin (fn (cxr->lst* cxr)) cxr)))
  (loop 'cr))

;; test
(define fn
  (lambda (x) (caadar (cdadr (caddr x)))))

(cxr-name fn)
