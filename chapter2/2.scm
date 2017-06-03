;2.17
(define (last-pair list) (if (null? (cdr list)) list (last-pair (cdr list))))
;2.18
(define (reverse lst) (iter lst '()))
(define (iter remained-items result)
 (if (null? remained-items)
  	 result
	 (iter (cdr remained-items) (cons (car remained-items) result))))
