;2.17
(define (last-pair list) (if (null? (cdr list)) list (last-pair (cdr list))))
;2.18
(define (reverse lst) (iter lst '()))
(define (iter remained-items result)
 (if (null? remained-items)
  	 result
	 (iter (cdr remained-items) (cons (car remained-items) result))))
;2.19
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(define (cc amount coin-values)
 (cond  ((= amount 0) 1)
  		((or (< amount 0) (no-more? coin-values)) 0)
		(else (+ (cc amount (except-first-denomination coin-values))
			   	 (cc (- amount (first-denomination coin-values)) coin-values)))))
(define (no-more? vals) (null? vals))
(define (except-first-denomination vals) (cdr vals))
(define (first-denomination vals) (car vals))
;2.20 my version didn't work since we can't deliver pairs to this function
(define (same-parity a . lst) 
 (cond  ((null? lst) '())
  		((= (remainder (car lst) 2) (remainder a 2)) (cons (car lst) (same-parity (cons a (cdr lst)))))
		(else (same-parity (cons a (cdr lst))))))
(define (same-parity a . lst) (filter (if (even? a) even? odd?) (cons a lst)))
;mapping over list
(define (scale-list items factor) (if (null? items) '() (cons (* factor (car items)) (scale-list (cdr items) factor))))
(define (map proc items) (if (null? items) '() (cons (proc (car items)) (map proc (cdr items)))))
;2.21
(define (square-list items) (map (lambda (x) (square x)) items))
(define (square-list items) (if (null? items) '() (cons (square (car items)) (square-list (cdr items)))))
;2.22 obviously wrong. hard to explain
;2.23
(define (for-each proc lst) (if (null? lst) #t (begin (proc (car lst)) (for-each proc (cdr lst)))))
;hierarchical structure
(define (count-leaves x) 
 (cond  ((null? x) 0)
  		((not (pair? x)) 1)
		(else (+ (count-leaves (car x)) (count-leaves (cdr x))))))
;2.24 easy
;2.25 1. (cdr (car (cdr (cdr x))))  2. (car (Car x)) 3.easy
;2.26 1. (1 2 3 4 5 6) 2. ((1 2 3) 4 5 6) 3. ((1 2 3) (4 5 6))
;2.27
(define (deep-reverse tree)
 (cond	((null? tree) '())
  		((not (pair? tree)) tree)
		(else (reverse (list (deep-reverse (car tree)) (deep-reverse (cadr tree)))))))
;2.28
(define (fringe lst)
 (cond  ((null? lst) '())
  		((not (pair? lst)) (list lst))
		(else (append (fringe (car lst)) (fringe (cadr lst))))))
;2.29
(define (make-mobile left right) (list left right))
(define (make-branch length structure) (list length structure))

(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cadr mobile))
(define (branch-length branch) (car branch))
(define (branch-structure branch) (cadr branch))

(define (total-weight mobile) (+ (branch-weight (left-branch mobile)) (branch-weight (right-branch mobile))))
(define (branch-weight branch) (if (number? (branch-structure branch)) (branch-structure branch) (total-weight (branch-structure branch))))

(define (balanced? mobile) (if (pair? mobile) 
							(and (balanced? (branch-structure (left-branch mobile))) 
								 (balanced? (branch-structure (right-branch mobile))) 
								 (= (torque (left-branch mobile)) (torque (right-branch mobile))))
							#t))
(define (torque branch) (* (branch-length branch) (branch-weight branch)))
;mapping over tree
(define (scale-tree tree factor)
 (cond  ((null? tree) '())
  		((not (pair? tree)) (* factor tree))
		(else (cons (scale-tree (car tree) factor) (scale-tree (cdr tree) factor)))))
(define (map-scale-tree tree factor) 
 (map (lambda (sub-tree) (if (pair? sub-tree) (map-scale-tree sub-tree factor) (* factor sub-tree))) tree))
;2.30
(define (square-tree tree)
 (map (lambda (sub-tree) (if (pair? sub-tree) (square-tree sub-tree) (square sub-tree))) tree))
(define (square-tree tree) 
 (cond  ((null? tree) '()) 
		((not (pair? tree)) (square tree))
		(else (cons (square-tree (car tree)) (square-tree (cdr tree))))))
;2.31
(define (tree-map proc tree) (map (lambda (sub-tree) (if (pair? sub-tree) (tree-map proc sub-tree) (proc tree))) tree))
(define (square-tree square tree) (tree-map square tree))
;2.32
(define (subsets s) (if (null? s) (list '()) (let ((rest (subsets (cdr s))))
											  (append rest (map (lambda (x) (append (list (car s)) x)) rest)))))
;sequences as conventional interface
(define (sum-odd-squares tree)
 (cond 	((null? tree) 0)
  		((not (pair? tree)) (if (odd? tree) (square tree) 0))
		(else (+ (sum-odd-squares (car tree)) (sum-odd-squares (cdr tree))))))
(define (even-fibs n)
 (define (next k) (if (> k n) '() (let ((f (fib k)))
								   (if (even? f) (cons f (next (+ k 1))) (next (+ k 1))))))
  (next 0))
