(define balance 100)
(define (withdraw amount) 
 (if (>= balance amount) (begin (set! balance (- balance amount)) balance) "Insufficient funds"))
(define new-withdraw (let ((balance 100)) (lambda (amount) (if (>= balance amount)
															(begin (set! balance (- balance amount)) balance)
															"Insufficient funds"))))
(define (make-withdraw balance) (lambda (amount) (if (>= balance amount) 
												  (begin (set! balance (- balance amount)) balance)
												  "Insufficient funds")))
;3.3
(define (make-account balance password)
 (let ((count 0))
	 (define (withdraw amount) (if (>= balance amount) 
								(begin (set! balance (- balance amount)) balance) "Insufficient funds"))
	 (define (deposit amount) (begin (set! balance (+ balance amount)) balance))
	 (define (display-password-error arg) (display "Incorrect password"))
	 (define (dispatch printed-pw m)
	  (cond ((not (eq? printed-pw password)) (begin 
											  (set! count (+ count 1)) (if (> count 7) (call-the-cops))
											  (display-password-error)))
			((eq? m 'withdraw) (begin (set! count 0) withdraw))
			((eq? m 'deposit) (begin (set! count 0) deposit))
			(else (error "unknown request " m))))
	 dispatch))
;3.1
(define (make-accumulator balance) (lambda (amount) (begin (set! balance (+ balance amount)) balance)))
;3.2
(define (make-monitored f) (let ((count 0)) (lambda (arg) (if (eq? arg 'how-many-calls?)
															count
															(begin (set! count (+ count 1)) (f arg))))))
;random
;(define rand (let ((x random-init)) (lambda () (set! x (rand-update x)) x)))
(define (estimate-pi trials) (sqrt (/ 6 (monte-carlo trials cesaro-test))))
(define (cesaro-test) (= (gcd (rand) (rand))))
(define (monte-carlo trials experiment)
 (define (iter trials-remaining trials-passed)
  (cond ((= trials-remaining 0) (/ trials-passed trials))
   		((experiment) (iter (- trials-remaining 1) (+ trials-passed 1)))
		(else (iter (- trials-remaining 1) trials-passed))))
 (iter trials 0))
;3.5
(define (random-in-range low high) (let ((range (- high low))) (+ low (random (exact->inexact range)))))
(define (estimate-integral predicate x1 x2 y1 y2 trials) (* (monte-carlo trials (lambda () (predicate (random-in-range x1 x2) (random-in-range y1 y2))))
															 (* (abs (- x1 x2))
																(abs (- y1 y2)))))
(define (get-pi trials) (exact->inexact (estimate-integral (lambda (x y) (< (+ (square x) (square y)) 1.0))
										 -1.0 1.0 -1.0 1.0 trials)))
