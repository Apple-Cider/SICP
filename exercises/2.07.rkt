#lang racket

(define (make-interval a b) (cons (min a b) (max a b)))

(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))