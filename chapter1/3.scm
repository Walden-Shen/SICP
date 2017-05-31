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
;1.18
