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
