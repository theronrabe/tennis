#lang racket

(provide commission)
(require tennis)

(define domain '((0 77) (0 88) (0 99)))

(define (between x X) (and (<= (car X) x) (>= (cadr X) x)))

(define lockPrice 35)
(define stockPrice 25)
(define barrelPrice 20)

(define (commission locks stocks barrels)
	(let* ( [lockSales (* locks lockPrice)]
		[stockSales (* stocks stockPrice)]
		[barrelSales (* barrels barrelPrice)]
		[sales (+ lockSales stockSales barrelSales)])

		(if (and (between locks (car domain)) (between stocks (cadr domain)) (between barrels (caddr domain)))
			(if (> sales 1800) (+ 100 120 (* .2 (- sales 1800)))
				(if (> sales 1000) (+ 100 (* .15 (- sales 1000)))
				(* .1 sales)))
			"Invalid input.")))
			
			
(generateTester 'commission #f #f domain)
(generateTester 'commission #f #t domain)
(generateTester 'commission #t #f domain)
