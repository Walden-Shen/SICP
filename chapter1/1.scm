;1.1 too simple
;1.2 too simple
;1.3 
(define (square x) (* x x))
(define (answer a b c) (- (+ (square a) (square b) (square c)) (square (min a b c))))
;1.4
(define (a-plus-abs-b a b) ((if (> b 0) + -) a b))
;1.5 if applicative order : no error; 
;if normal order : error occurs since the (define (p) (p)) incurs infinite recursion

;newton
(define (sqrt-iter guess x) (if (good-enough? guess x) guess (sqrt-iter (improve guess x) x)))
(define (good-enough? guess x) (< (abs (- (square guess) x)) 0.001))
(define (improve guess x) (average guess (/ x guess)))
(define (average a b) (/ (+ a b) 2))
(define (sqrt x) (sqrt-iter 1.0 x))

;1.6  it will exceed the maximum recursion depth; since the argument in new if
	;will be evaluated first before the procedure , then the else-clause will be 
(define (new-if predicate then-clause else-clause) (cond (predicate then-clause) (else else-clause)))
(define (new-sqrt-iter guess x) (new-if (good-enough? guess x) guess (new-sqrt-iter (improve guess x) x)))
(define (new-sqrt x) (new-sqrt-iter 1.0 x))
;1.7
(define (good-sqrt-iter old-guess guess x) (if (good-good-enough? guess old-guess) guess (good-sqrt-iter guess (improve guess x) x)))
(define (good-sqrt x) (good-sqrt-iter 0.0 1.0 x))
(define (good-good-enough? old-guess guess) (> 0.01 (/ (abs (- guess old-guess)) old-guess)))
;1.8
(define (good-improve guess x) (/ (+ (/ x (square y)) (* 2 y)) 3))
