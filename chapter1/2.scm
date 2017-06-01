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
;1.14 theory question
;1.15
(define (cube x) (* x x x))
(define (p x)(- (* 3 x) (* 4 (cube x))))
(define (sine angle)
 	(if (< (abs angle) 0.1)
	 	angle
		(p (sine (/ angle 3.0)))))
;1.16
(define (fast-expt b n) (fast-expt-ancillary b b (- n 1)))
(define (fast-expt-ancillary a b n)
 (cond  ((= n 0) a)
  		((even? n) (fast-expt-ancillary (* a a) b (/ n 2)))
		(else (fast-expt-ancillary (* a b) b (- n 1)))))
;1.17 similar to 16
;1.18 unable to finish since i didn't finish 1.17 :p
;1.19
(define (fib n) (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
 (cond  ((= count 0) b)
  		((even? count) (fib-iter 	a 
									b 
								(+ (square p) (square q)) 
								(+ (square q) (* 2 p q)) 
								(/ count 2)))
		(else (fib-iter (+ (* b q) (* a q) (* a p))
			   			(+ (* b p) (* a q))
						p
						q
						(- count 1)))))
;1.20 theoretical
;prime
(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
 (cond	((> (square test-divisor) n) n)
  		((divides? test-divisor n) test-divisor)
		(else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))
(define (prime? n) (= n (smallest-divisor n)))
;fermat
(define (expmod base exp m)
 (cond  ((= exp 0) 1)
  		((even? exp)
		 (remainder (square (expmod base (/ exp 2) m)) m))
		(else (remainder (* base (expmod base (- exp 1) m)) m))))
(define (fermat-test n)
 (define (try-it a)
  (= (expmod a n n) a))
 (try-it (+ 1 (random (- n 1)))))
(define (fast-prime? n times)
 (cond  ((= times 0) #t)
  		((fermat-test n) (fast-prime? n (- times 1)))
		(else #f)))
;1.21 too simple
;1.22
(define (timed-prime-test n) (newline) (display n) (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
 (if (prime? n) (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time) (display " *** ") (display elapsed-time))

