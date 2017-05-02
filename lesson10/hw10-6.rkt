; SICP compatible for stream

#lang racket
(require "../misc.scm")
(require "streams.scm")

(define (fract-stream num deno)
  (define (stream-loop rem)
    (cons-stream (quotient (* rem 10) deno)
                 (stream-loop (remainder (* rem 10) deno))))
  (stream-loop num))

(define (approximation stm dig)
  (define (appro-1-index stm dig)
    (if (< dig 0)
        '()
        (let ((rest (appro-1-index stm (sub1 dig))))
          (append rest (list (stream-ref stm dig))))))
  (appro-1-index stm (sub1 dig)))

(approximation (fract-stream 1 243) 20)
