#lang racket

(provide (contract-out
		[domain?	(-> any/c boolean?)]
		[BVA 		(-> domain? domain?)]
		[robust 	(-> domain? domain?)]))

;What qualifies as a domain?
	(define (domain? D) (if (match D [(list (list a b _ ...) ...) #t] [_ #f])
				(andmap (lambda (d) (andmap integer? d)) D)
				#f))

;
;Domain transforming functions
;
	;Turn a list of ranges into a list of boundary value lists
	(define (BVA domain) 
		(let ([nom (lambda (x) (round (/ (+ (car x) (last x)) 2)))])
			(map (lambda (arg) (list (car arg) (+ 1 (car arg)) (nom arg) (- (last arg) 1) (last arg))) domain)))

	;Robustify a domain
	(define (robust domain) (map (lambda (x) (flatten (list (- (car x) 1) x (+ 1 (last x))))) domain))

