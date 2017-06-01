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
 (if (fast-prime? n 10) (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time) (display " *** ") (display elapsed-time))

