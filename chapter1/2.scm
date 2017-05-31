;iteration version of factorial
(define (factorial n) (fact-iter 1 1 n))
(define (fact-iter product counter max-count)
 (if (> counter max-count) 
  product
  (fact-iter (* counter product) (+ counter 1) max-count)))
;1.9 (define (add a b) (if (= a 0) b (+ (add (- a 1) b) 1))) is not tail recursive and the other one is.
;1.10  (A 1 10) = 1024; (A 2 4) = 65536; (A 3 3) = 65536;
(define (A x y)
 (cond  ((= y 0) 0)
  		((= x 0) (* 2 y))
		((= y 1) 2)
		(else (A (- x 1) (A x (- y 1))))))
(define (f n) (A 0 n));2n
(define (g n) (A 1 n));2^n
(define (h n) (A 2 n))
;1.11
(define (f-recursive n)
 (cond	((< n 3) n)
  		(else (+ (f-recursive (- n 1)) (* 2 (f-recursive (- n 2))) (* 3 (f-recursive(- n 3)))))))
(define (f-iter n) (f-iter-anxillary 2 1 0 0 n))
(define (f-iter-anxillary a b c i n)
 (if (= i n)
  		c
		(f-iter-anxillary (+ a (* 2 b) (* 3 c)) a b (+ i 1) n)))

;1.12
(define (pascal x y)
 (cond  ((= x 0) 1)
  		((equal? x y) 1)
		(else (+ (pascal (- x 1) (- y 1)) (pascal x (- y 1))))))
;1.13
;I have read the prove procudure
