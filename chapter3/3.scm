;3.12 (b) (b c d)
(define (append! x y) (set-cdr! (last-pair x) y) x)
(define (last-pair x) (if (null? (cdr x)) x (last-pair (cdr x))))
;3.13
(define (make-cycle x) (set-cdr! (last-pair x) x) x)
;3.14 a reverse function
(define (mystery x)
 (define (loop x y)
  (if (null? x) y
   (let ((temp (cdr x))) (set-cdr! x y) (loop temp x))))
 (loop x '()))
;3.15 i draw the box in my heart
;3.16
(define (count-pairs x) (if (not (pair? x)) 0 (+ (count-pairs (car x)) (count-pairs (cdr x)) 1)))
;3.17
(define (count-pairs x) (length (inner x '())))
(define (inner x memo-list)
 (if (and (pair? x) (false? (memq x memo-list)))
  (inner (car x) (inner (cdr x) (cons x memo-list))) memo-list))
;3.18 impressive
(define (loop? lst)
 (let ((identity (cons '() '())))
  (define (iter remain-list)
   	(cond   ((null? remain-list) #f)
	 		((eq? identity (car remain-list)) #t)
			(else (set-car! remain-list identity) (iter (cdr remain-list)))))
  (iter lst)))
;3.19
(define (loop-const? lst)
 (define (iter x y)
  (let ((x-walk (list-walk 1 x)) (y-walk (list-walk 2 y)))
     (cond  ((or (null? x-walk) (null? y-walk)) #f)
	  		((eq? x-walk y-walk) #t)
			(else (iter x-walk y-walk)))))
 (iter lst lst))
(define (list-walk step lst)
 (cond  ((null? lst) '())
  		((= step 0) lst)
		(else (list-walk (- step 1) (cdr lst)))))
;queue
(define make-queue '())
(define (insert-queue! queue item) (append! queue item))
