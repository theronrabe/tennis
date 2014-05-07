#lang racket
(require tennis)
(provide triangle)

(define domain '((5 205) (5 205) (5 205)))

(define (triangle a b c)
	(let ([isATriangle (and (< a (+ b c)) (< b (+ a c)) (< c (+ a b)))])
		(if isATriangle 
			(if (and (= a b) (= b c))
				"Equilateral"
				(if (and (not (= a b)) (not (= a c)) (not (= b c)))
					"Scalene"
					"Isosceles"))
			"Not a Triangle")))

(generateTester 'triangle #f #t domain)
(generateTester 'triangle #t #t domain)
