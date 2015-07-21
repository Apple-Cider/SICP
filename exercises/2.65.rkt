#lang racket

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set) 
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

(define (balance-tree tree)
  (list->tree (tree->list-2 tree)))

(define (display-tree tree)
  (define (display-child parent child-tree)
    (cond ((not (null? child-tree))
           (let ((child (entry child-tree)))
             (display parent) (display "->") (display child)
             (newline)
             (display-child child (left-branch child-tree))
             (display-child child (right-branch child-tree))))))
  (cond ((not (null? tree))
         (display-child (entry tree) (left-branch tree))
         (display-child (entry tree) (right-branch tree)))
        (else (display "tree empty!"))))

(define (intersection-set-list set1 set2)
  (if (or (null? set1) (null? set2))
      '()    
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set-list (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set-list (cdr set1) set2))
              ((< x2 x1)
               (intersection-set-list set1 (cdr set2)))))))


;completely missed the point of including exercise 2.64's logic in this,
;  hence the still THETA(n^2) growth
(define (union-set set1 set2)
  (define (elts-adjoin-set elts set)
    (cond ((> (length elts) 2)
           (elts-adjoin-set (cdddr elts)
                            (balance-tree (adjoin-set (caddr elts)
                                                     (adjoin-set (cadr elts)
                                                                 (adjoin-set (car elts) set))))))
          ((= (length elts) 0) set)
          (else (elts-adjoin-set (cdr elts) (adjoin-set (car elts) set)))))
  (if (null? set2)
      set1
      (balance-tree (elts-adjoin-set (tree->list-2 set1) set2))))

(define tree-a (make-tree 1 '()
                          (make-tree 2 '()
                                     (make-tree 3 '()
                                                (make-tree 4 '()
                                                           (make-tree 5 '()
                                                                      (make-tree 6 '()
                                                                                 (make-tree 7 '() '()))))))))

(define tree-b (make-tree 3
                          (make-tree 1 '() '())
                          (make-tree 7
                                     (make-tree 5 '() '())
                                     (make-tree 9
                                                '()
                                                (make-tree 11 '() '())))))

(define tree-c (make-tree 4
                          (make-tree 2 '() '())
                          (make-tree 8
                                     (make-tree 6 '() '())
                                     (make-tree 10 '() '()))))
(define tree-d (make-tree 4
                          (make-tree 2
                                     (make-tree 1 '() '())
                                     (make-tree 3 '() '()))
                          (make-tree 6
                                     (make-tree 5 '() '())
                                     (make-tree 7 '() '()))))


;(display "unbalanced tree: ") (newline)
;(display-tree tree-a)

;(display "balanced tree: ") (newline)
;(display-tree (balance-tree tree-a))

(define tree-union-ab (union-set tree-a tree-b))
;(display-tree tree-union-ab)

(define tree-union-cd (union-set tree-c tree-d))
;(display-tree tree-union-cd)
