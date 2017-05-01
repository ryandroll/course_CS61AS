; SICP compatible for stream

#lang racket
(require "../misc.scm")
(require "streams.scm")

(define (num-seq n)
  (cond
    ((odd? n) (cons-stream n (num-seq (+ 1 (* 3 n)))))
    ((even? n) (cons-stream n (num-seq (/ n 2))))))

(define (seq-length stm)
  (define (loop i)
    (cond
      ((= (stream-ref stm i) 1) (add1  i))
      (else (loop (add1 i)))))
  (loop 0))
