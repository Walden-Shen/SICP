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

;1.29  my answer
;(define (new-sum term a next b count)
; (define (product count) (if (= (remainder count 2) 0) 2 4))
; (if (> a b)
;  	 0
; 	 (+ (* (product count) (term a)) (new-sum term (next a) next b (+ count 1)))))
;(define (simpson f a b n)
; (define h (/ (- b a) n))
; (define (simpson-next x) (+ x h))
; (* (/ h 3.0) (- (new-sum f a simpson-next b 0) (f a) (f b))))

;criteria answer
(define (simpson f a b n)
 (define h (/ (- b a) n))
 (define (y k) (f (+ a (* k h))))
 (define (factor k)
  (cond ((or (= k 0) (= k n)) 1)
   		((odd? k) 4)
		(else 2)))
 (define (term k) (* (factor k) (y k)))
 (define (next k) (+ k 1))
 (if (not (even? n))
  		(error "n can't be odd")
		(* (/ h 3) (sum term (exact->inexact 0) next n))))

;1.30
(define (new-new-sum term a next b)
 (define (iter a result)
  (if   (> a b)
   		result
		(iter (next a) (+ result (term a)))))
  (iter a 0))

;1.31
(define (product-recur term a next b)
 (if (> a b) 1 (* (term a) (product-recur term (next a) next b))))

(define (test-factorial n) (product-recur (lambda (x) x) 1 (lambda (x) (+ x 1)) n))

(define (test-pi n)
 (define (term-pi n) (if (= (remainder n 2) 0)
					  (/ (+ 2.0 (* 2 (/ n 2))) (+ 3 (* 2 (/ n 2))))
					  (/ (+ 2.0 (* 2 (/ (+ n 1) 2))) (+ 3 (* 2 (/ (- n 1) 2))))))
 (* 4 (product-recur term-pi 0 (lambda (x) (+ x 1)) n)))
;criteria version
(define (numer-term i)
 (cond	((= i 1) 2)
  		((even? i) (+ i 2))
		(else (+ i 1))))
(define (denom-term i)
 (if (odd? i)
  	 (+ i 2)
	 (+ i 1)))
(define (pi n)
 (* 4 (exact->inexact (/ (product-recur numer-term 1 (lambda (i) (+ i 1)) n)
					   	 (product-recur denom-term 1 (lambda (i) (+ i 1)) n)))))
;1.32
(define (accumulate combiner null-value term a next b)
 (if (> a b) null-value
  (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b)
 (define (iter result a) 
  (if (> a b) (combiner result null-value) (iter (combiner result (term a)) (next a))))
 (iter null-value a))

;1.33
(define (filtered-accumulate combiner filter? null-value term a next b)
 (cond  ((> a b) null-value)
  		((filter? a) (combiner (term a) (filtered-accumulate combiner filter? null-value term (next a) next b)))
		(else (filtered-accumulate combiner filter? null-value term (next a) next b))))

;*the variable's values are computed outside the let*
(define (f x y)
 (let ((a (+ 1 (* x y)))
	   (b (- 1 y)))
  (+ (* x (square a))
   	 (* y b)
	 (* a b))))

;1.34   the function will call (2 2) at last, which is meaningless
