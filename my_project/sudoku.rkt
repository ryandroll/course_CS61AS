#lang racket

(require berkeley)
(provide (all-defined-out))

;; sudoku
;; 使用 list 未知填入 0
;; 需要下列函數
;; 遞回填入未知參數
;; 每行、每列、每個宮數字都要剛好不能重複
;; 把另外兩種情況換成每一行

;; 每行轉每列
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (foldr op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (transpose mat)
  (accumulate-n cons '() mat))

;; 宮轉成列、用模式匹配的暴力法、不過最大 bug 來自於此
;; 轉換的位置一直寫錯，有夠難發現

(define (subgrids mat)
  (match mat
    [`((,e11 ,e12 ,e13 ,e14 ,e15 ,e16 ,e17 ,e18 ,e19)
       (,e21 ,e22 ,e23 ,e24 ,e25 ,e26 ,e27 ,e28 ,e29)
       (,e31 ,e32 ,e33 ,e34 ,e35 ,e36 ,e37 ,e38 ,e39)
       (,e41 ,e42 ,e43 ,e44 ,e45 ,e46 ,e47 ,e48 ,e49)
       (,e51 ,e52 ,e53 ,e54 ,e55 ,e56 ,e57 ,e58 ,e59)
       (,e61 ,e62 ,e63 ,e64 ,e65 ,e66 ,e67 ,e68 ,e69)
       (,e71 ,e72 ,e73 ,e74 ,e75 ,e76 ,e77 ,e78 ,e79)
       (,e81 ,e82 ,e83 ,e84 ,e85 ,e86 ,e87 ,e88 ,e89)
       (,e91 ,e92 ,e93 ,e94 ,e95 ,e96 ,e97 ,e98 ,e99))

     `((,e11 ,e12 ,e13 ,e21 ,e22 ,e23 ,e31 ,e32 ,e33)
       (,e41 ,e42 ,e43 ,e51 ,e52 ,e53 ,e61 ,e62 ,e63)
       (,e71 ,e72 ,e73 ,e81 ,e82 ,e83 ,e91 ,e92 ,e93)
       (,e14 ,e15 ,e16 ,e24 ,e25 ,e26 ,e34 ,e35 ,e36)
       (,e44 ,e45 ,e46 ,e54 ,e55 ,e56 ,e64 ,e65 ,e66)
       (,e74 ,e75 ,e76 ,e84 ,e85 ,e86 ,e94 ,e95 ,e96)
       (,e17 ,e18 ,e19 ,e27 ,e28 ,e29 ,e37 ,e38 ,e39)
       (,e47 ,e48 ,e49 ,e57 ,e58 ,e59 ,e67 ,e68 ,e69)
       (,e77 ,e78 ,e79 ,e87 ,e88 ,e89 ,e97 ,e98 ,e99))
     ]))

;; 陣列與 list 互相轉換：陣列方便比較、list 方便修改數據
(define (mat->lst mat)
  (match mat
    [(? null? lst) '()]
    [`(,l1 ,l2 ...)
     (append l1 (mat->lst l2))]))

(define (lst->mat lst)
  (match lst
    [(? null? lst) '()]
    [`(,l1 ,l2 ,l3 ,l4 ,l5 ,l6 ,l7 ,l8 ,l9 ,l10 ...)
     (cons `(,l1 ,l2 ,l3 ,l4 ,l5 ,l6 ,l7 ,l8 ,l9)
           (lst->mat l10))]))

;; 類似 interpreter，expand, 與 screen 交互執行
;; 主體: list of list，screen 與輸出時轉換成 list of matrix
;; expand: 把 0 展開成 1-9，使用 list
;; screen: 過濾展開的 list of list 剩下合法的，使用陣列

;; expand
(define (expand-unit lst)
  (match lst
    [`(,l1 ...  0 ,l2 ...) (map (lambda (x) (append l1 `(,x) l2))
                                (enumerate-interval 1 9))]
    [_ 'else]))

(define (expand-lst lst-of-lst)
  (if (null? lst-of-lst)
      '()
      (append (expand-unit (car lst-of-lst))
              (expand-lst (cdr lst-of-lst)))))

;; screen
(define (good-lst? lst)
  (let ((alst (filter (lambda (x) (not (= 0 x))) lst)))
    (= (length alst) (set-count (list->set alst)))))

(define (good-mat? mat)
  (if (null? mat)
      #t
      (and (good-lst? (car mat))
           (good-mat? (cdr mat)))))

(define (good-sudoku? mat)
  (and (good-mat? mat)
       (good-mat? (subgrids mat))
       (good-mat? (transpose mat))))

(define (screen-lst lst-of-lst)
  (filter (lambda (x) (good-sudoku? (lst->mat x))) lst-of-lst))

;; expand screen 循環、直到沒有 0

(define (main-sudoku mat)
  (define (rec lst-of-lst)
    (if (member? 0 (car lst-of-lst))
        (begin
          (displayln (length lst-of-lst)) ; 測試運作情況
          (rec (screen-lst (expand-lst lst-of-lst))))
        (map lst->mat lst-of-lst)))
  (rec (list (mat->lst mat))))

(define quiz
  '((0 2 9 6 0 0 8 0 1)
    (0 6 0 0 0 0 0 0 0)
    (0 0 0 0 9 7 0 3 0)
    (0 0 0 8 5 2 0 0 0)
    (8 0 5 0 0 0 0 1 0)
    (6 0 0 0 0 0 5 4 0)
    (5 0 0 0 2 4 0 0 0)
    (0 8 4 1 0 0 3 0 2)
    (0 0 0 0 0 0 0 0 4)))

(main-sudoku quiz)
(main-sudoku (transpose quiz))
