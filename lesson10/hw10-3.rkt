; SICP compatible for stream

#lang racket
(require "../misc.scm")
(require "streams.scm")

#|

|#


(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low (stream-enumerate-interval (+ low 1) high))))

(delay (enumerate-interval 1 3))
(stream-enumerate-interval 1 3)
