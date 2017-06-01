;abstraction
(define (sum term a next b)
 (if (> a b)
  	 0
	 (+ (term a) (sum term (next a) next b))))
(define (inc n) (+ n 1))
(define (cube x) (* x x x))
(define (sum-cubes a b) (sum cube a inc b))
(define (pi-sum a b)
 (define (pi-term n) (/ 1.0 (* n (+ n 2))))
 (define (pi-next n) (+ n 4))
 (sum pi-term a pi-next b))
;calculus
(define (integral f a b dx)
 (define (add-dx x) (+ x dx))
 (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))
;1.29
(define (new-sum term a next b count)
 (define (product count) (if (= (remainder count 2) 0) 2 4))
 (if (> a b)
  	 0
 	 (+ (* (product count) (term a)) (new-sum term (next a) next b (+ count 1)))))
(define (simpson f a b n)
 (define h (/ (- b a) n))
 (define (simpson-next x) (+ x h))
 (* (/ h 3.0) (- (new-sum f a simpson-next b 0) (f a) (f b))))
;1.30
(define (new-new-sum term a next b)
 (define (iter a result)
  (if   (> a b)
   		result
		(iter (next a) (+ result (term a))))
  (iter a 0)))

