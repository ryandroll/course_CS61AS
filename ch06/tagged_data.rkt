#lang racket

(require (planet dyoo/simply-scheme))
(provide (all-defined-out))

(define (contents tagged-data)
  (if (pair? tagged-data)
      (cdr tagged-data)
      (error "Not tagged data")))
