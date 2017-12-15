#lang racket

;; World's hardest sudoku
(define quiz
  '((8 0 0 0 0 0 0 0 0)
    (0 0 3 6 0 0 0 0 0)
    (0 7 0 0 9 0 2 0 0)
    (0 5 0 0 0 7 0 0 0)
    (0 0 0 0 4 5 7 0 0)
    (0 0 0 1 0 0 0 3 0)
    (0 0 1 0 0 0 0 6 8)
    (0 0 8 5 0 0 0 1 0)
    (0 9 0 0 0 0 4 0 0)))

;; matrix 與 list 互相轉換：matrix 方便比較、list 方便修改數據
(define (mat->lst mat)
  (match mat
    [(? null? lst) '()]
    [`(,l1 ,l2 ...)
     (append l1 (mat->lst l2))]))

(define (make-dict ht lst1 lst2)
  (if (null? (cdr lst1))
      (hash-set! ht (car lst1) (car lst2))
      (begin (hash-set! ht (car lst1) (car lst2))
             (make-dict ht (cdr lst1) (cdr lst2)))))

(define sudoku-key
  '("a1A" "a2A" "a3A" "a4B" "a5B" "a6B" "a7C" "a8C" "a9C"
    "b1A" "b2A" "b3A" "b4B" "b5B" "b6B" "b7C" "b8C" "b9C"
    "c1A" "c2A" "c3A" "c4B" "c5B" "c6B" "c7C" "c8C" "c9C"
    "e1D" "e2D" "e3D" "e4E" "e5E" "e6E" "e7F" "e8F" "e9F"
    "f1D" "f2D" "f3D" "f4E" "f5E" "f6E" "f7F" "f8F" "f9F"
    "g1D" "g2D" "g3D" "g4E" "g5E" "g6E" "g7F" "g8F" "g9F"
    "d1G" "d2G" "d3G" "d4H" "d5H" "d6H" "d7I" "d8I" "d9I"
    "e1G" "e2G" "e3G" "e4H" "e5H" "e6H" "e7I" "e8I" "e9I"
    "f1G" "f2G" "f3G" "f4H" "f5H" "f6H" "f7I" "f8I" "f9I"))

(define sudoku-value
  (mat->lst quiz))

(define sudoku-dict (make-hash))
(make-dict sudoku-dict sudoku-key sudoku-value)
(hash-iterate-first sudoku-dict)

(define (units elm)
  (filter (lambda (x)
            (regexp-match? #rx"(string (string-ref elm 0)).." ) x)
          sudoku-key))



(define (test lst)
  (filter (lambda (x) (or (regexp-match? #rx"a.." x)
                          (regexp-match? #rx".1." x)
                          (regexp-match? #rx"..A" x)))
          lst)
  )


;; sudoku-dict

#|
(define (subgrids mat)
  (match mat
    [`((,a1A ,a2A ,a3A ,a4B ,a5B ,a6B ,a7C ,a8C ,a9C)
       (,b1A ,b2A ,b3A ,b4B ,b5B ,b6B ,b7C ,b8C ,b9C)
       (,c1A ,c2A ,c3A ,c4B ,c5B ,c6B ,c7C ,c8C ,c9C)
       (,e1D ,e2D ,e3D ,e4E ,e5E ,e6E ,e7F ,e8F ,e9F)
       (,f1D ,f2D ,f3D ,f4E ,f5E ,f6E ,f7F ,f8F ,f9F)
       (,g1D ,g2D ,g3D ,g4E ,g5E ,g6E ,g7F ,g8F ,g9F)
       (,d1G ,d2G ,d3G ,d4H ,d5H ,d6H ,d7I ,d8I ,d9I)
       (,e1G ,e2G ,e3G ,e4H ,e5H ,e6H ,e7I ,e8I ,e9I)
       (,f1G ,f2G ,f3G ,f4H ,f5H ,f6H ,f7I ,f8I ,f9I))

     `((,a1A ,a2A ,a3A ,a4B ,a5B ,a6B ,a7C ,a8C ,a9C)
       (,b1A ,b2A ,b3A ,b4B ,b5B ,b6B ,b7C ,b8C ,b9C)
       (,c1A ,c2A ,c3A ,c4B ,c5B ,c6B ,c7C ,c8C ,c9C)
       (,e1D ,e2D ,e3D ,e4E ,e5E ,e6E ,e7F ,e8F ,e9F)
       (,f1D ,f2D ,f3D ,f4E ,f5E ,f6E ,f7F ,f8F ,f9F)
       (,g1D ,g2D ,g3D ,g4E ,g5E ,g6E ,g7F ,g8F ,g9F)
       (,d1G ,d2G ,d3G ,d4H ,d5H ,d6H ,d7I ,d8I ,d9I)
       (,e1G ,e2G ,e3G ,e4H ,e5H ,e6H ,e7I ,e8I ,e9I)
       (,f1G ,f2G ,f3G ,f4H ,f5H ,f6H ,f7I ,f8I ,f9I))]))
|#
