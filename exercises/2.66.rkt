#lang racket

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (make-record id name age) (list id name age))
(define (key record) (car record))
(define (display-record record)
  (display "id: ")
  (display (car record))
  (newline)
  (display "name: ")
  (display (cadr record))
  (newline)
  (display "age: ")
  (display (caddr record))
  (newline))

(define r-0 (make-record 0 "mike" 22))
(define r-1 (make-record 1 "susan" 46))
(define r-2 (make-record 2 "emily" 38))
(define r-3 (make-record 3 "niel" 32))
(define r-4 (make-record 4 "george" 26))
(define r-5 (make-record 5 "oscar" 68))
(define r-6 (make-record 6 "diana" 41))
(define my-records (make-tree r-3
                              (make-tree r-1
                                         (make-tree r-0 '() '())
                                         (make-tree r-2 '() '()))
                              (make-tree r-5
                                         (make-tree r-4 '() '())
                                         (make-tree r-6 '() '()))))

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((< given-key (key (entry set-of-records)))
         (lookup given-key (left-branch set-of-records)))
        ((> given-key (key (entry set-of-records)))
         (lookup given-key (right-branch set-of-records)))
        ((= given-key (key (entry set-of-records)))
         (entry set-of-records))))

(display-record (lookup 5 my-records))