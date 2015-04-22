#lang racket

(define (make-interval a b) (cons (min a b) (max a b)))

(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (old-mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

;interval equivalency
(define (equals? x y)
  (and (= (lower-bound x) (lower-bound y))
       (= (upper-bound x) (upper-bound y))))

;interval does not span to 0 or lower
(define (positive? interval)
  (> (lower-bound interval) 0))

;interval does not span above 0
(define (negative? interval)
  (not (> (upper-bound interval) 0)))

(define (mul-interval x y)
  (define case
    (if (negative? x)
        (if (negative? y)
            0
            (if (positive? y)
                1
                2))
        (if (positive? x)
            (if (negative? y)
                3
                (if (positive? y)
                    4
                    5))
            (if (negative? y)
                6
                (if (positive? y)
                    7
                    8)))))
  (cond ((= case 0) (make-interval (* (lower-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y))))
        ((= case 1) (make-interval (* (upper-bound x) (upper-bound y)) (* (lower-bound x) (lower-bound y))))
        ((= case 2) (make-interval (* (upper-bound x) (upper-bound y)) (* (upper-bound x) (lower-bound y))))
        ((= case 3) (make-interval (* (upper-bound x) (upper-bound y)) (* (lower-bound x) (lower-bound y))))
        ((= case 4) (make-interval (* (lower-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y))))
        ((= case 5) (make-interval (* (upper-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y))))
        ((= case 6) (make-interval (* (upper-bound x) (upper-bound y)) (* (lower-bound x) (upper-bound y))))
        ((= case 7) (make-interval (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (upper-bound y))))
        ((= case 8) (make-interval (min (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (lower-bound y)))
                                   (max (* (lower-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y)))))))

(define (print interval)
  (newline)
  (display "[")
  (display (lower-bound interval))
  (display ",")
  (display (upper-bound interval))
  (display "]"))

(define (random-interval maximum)
  (let ((parity (random 4))
        (a (random (+ maximum 1)))
        (b (random (+ maximum 1))))
    (cond ((= parity 0) (make-interval a b))
          ((= parity 1) (make-interval a (- b)))
          ((= parity 2) (make-interval (- a) b))
          ((= parity 3) (make-interval (- a) (- b))))))


(define (test-random-constructor n)
  (if (= n 0)
      (newline)
      ((lambda ()
        (print (random-interval 100))
        (test-random-constructor (- n 1))))))

;(test-random-constructor 10)


(define (test-mul n)
  (define (print-matched)
    (newline)
    (display "passed..."))
  (define (print-mismatch x y expected computed)
    (newline)
    (display "x: ")
    (print x)
    (newline)
    (display "y: ")
    (print y)
    (newline)
    (display "expected: ")
    (print expected)
    (newline)
    (display "computed: ")
    (print computed))
  (define (check f0 f1 expected computed)
    (if (equals? expected computed)
        (print-matched)
        (print-mismatch f0 f1 expected computed))
    (test-mul (- n 1)))
  (let ((a (random-interval 10))
        (b (random-interval 10)))
    (let ((c (old-mul-interval a b))
          (d (mul-interval a b)))
      (if (> n 0)
          (check a b c d)
          (newline)))))

(test-mul 10)

(define i0 (make-interval -2 -1))
(define i1 (make-interval -1 0))
(define i2 (make-interval -1 1))
(define i3 (make-interval 0 0))
(define i4 (make-interval 0 1))
(define i5 (make-interval 1 2))

(define (test-conditionals interval)
  (cond ((positive? interval) (display "positive"))
        ((negative? interval) (display "negative"))
        (else (display "mixed"))))