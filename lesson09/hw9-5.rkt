;;;; SICP Compatible
#lang scheme
(require rnrs/base-6)
(require rnrs/mutable-pairs-6)

#|
Write vector-append
|#

(define (vector-append vec1 vec2)
  (let* ((l-v1 (vector-length vec1))
         (l-v2 (vector-length vec2))
         (l-nv (+ l-v1 l-v2)))
    (define (loop newvec i)
      (cond
        ((< i 0) newvec)
        ((< i l-v1) (begin (vector-set! newvec i (vector-ref vec1 i))
                           (loop newvec (sub1 i))))
        (else (begin (vector-set! newvec i (vector-ref vec2 (- i l-v1)))
                     (loop newvec (sub1 i))))))
    (loop (make-vector l-nv) (sub1 l-nv))))

(define v1 (vector 'a 'b 'c))
(define v2 (vector 'd 'e 'f))
(define v3 (vector-append v1 v2))
