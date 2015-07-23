#lang racket

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))


(define (contains? symbol node)
  (define (set-contains? item set)
    (cond ((null? set) false)
          ((eq? item (car set)) true)
          (else (set-contains? item (cdr set)))))
  (set-contains? symbol (symbols node)))

(define (encode-symbol symbol tree)
  (if (contains? symbol tree)
       (if (leaf? tree)
           '()
           (if (contains? symbol (left-branch tree))
               (cons 0 (encode-symbol symbol (left-branch tree)))
               (cons 1 (encode-symbol symbol (right-branch tree)))))
       (error "bad tree -- ENCODE-SYMBOL" tree)))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (successive-merge elts)
  (if (= (length elts) 1)
      (car elts)
      (let ((new-node (make-code-tree (car elts) (cadr elts)))
            (remaining-elts (cddr elts)))
        (let ((reduced-elts (adjoin-set new-node remaining-elts)))
          (successive-merge reduced-elts)))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define 1950s-rock-tree
  (generate-huffman-tree
   (list (list 'A 2)
         (list 'BOOM 1)
         (list 'GET 2)
         (list 'JOB 2)
         (list 'NA 16)
         (list 'SHA 3)
         (list 'YIP 9)
         (list 'WAH 1))))

(define the-silhouettes-outcry
  '(GET A JOB SHA NA NA NA NA NA NA NA NA
        GET A JOB SHA NA NA NA NA NA NA NA NA
        WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
        SHA BOOM))

(define secret-rock (encode the-silhouettes-outcry 1950s-rock-tree))
secret-rock
(display "bits for encoding: ")
(display (length secret-rock))
(newline)
(display "bits for fixed-length encoding: ")
(display (* 3 (length the-silhouettes-outcry)))