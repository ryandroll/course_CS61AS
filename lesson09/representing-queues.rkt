;;;; SICP Compatible to set-car! set-cdr!
#lang scheme
(require rnrs/base-6)
(require rnrs/mutable-pairs-6)

#|
Representing Queues
|#

(define (front-ptr queue)
  (car queue))
(define (rear-ptr queue)
  (cdr queue))

(define (set-front-ptr! queue item)
  (set-car! queue item))
(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (make-queue)
  (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond
      ((empty-queue? queue) (begin (set-front-ptr! queue new-pair)
                                   (set-rear-ptr! queue new-pair)
                                   queue))
      (else (begin (set-cdr! (rear-ptr queue) new-pair)
                   (set-rear-ptr! queue new-pair)
                   queue)))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))

(define q (make-queue))
(insert-queue! q 'a)
(insert-queue! q 'b)
(delete-queue! q)
(insert-queue! q 'c)
(insert-queue! q 'd)
(delete-queue! q)
