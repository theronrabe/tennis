#lang racket
(provide nextdate)
(require tennis)

(define domain '((1 12) (1 31) (1801 2014)))

(define (leapyear year) (and (= 0 (modulo year 4)) (not (= 0 (modulo year 400)))))

(define (nextdate month day year)
	(let ([c1 (and (<= 1 day) (<= day 31))]
		[c2 (and (<= 1 month) (<= month 12))]
		[c3 (and (<= 1801 year) (<= year 2014))]
		[tomorrowDay 1] [tomorrowMonth month] [tomorrowYear year])
			(if (not c1) "Value of day not in range 1..31"
			(if (not c2) "Value of month not in range 1..12"
			(if (not c3) "Value of year not in range 1801..2014"
			(car (filter (lambda (x) (not (void? x))) (list
				(cond
					[(member month '(1 3 5 7 8 10)) (if (< day 31) (set! tomorrowDay (+ day 1)) (set! tomorrowMonth (+ month 1)))]
					[(member month '(4 6 9 11)) (if (< day 30)
									(set! tomorrowDay (+ day 1))
									(if (= day 30) (set! tomorrowMonth (+ month 1)) "Invalid Input"))]
					[(= month 12) (if (< day 31) (set! tomorrowDay (+ day 1)) (if (= year 2014)
													"Invalid Input"
													(set!-values (tomorrowYear tomorrowMonth) (values (+ year 1) 1))))]
					[(= month 2) (if (< day 28)
								(set! tomorrowDay (+ day 1))
								(if (= day 28) (if (leapyear year)
											(set! tomorrowDay 29)
											(set! tomorrowMonth 3))
										(if (and (leapyear year) (= day 29))
											(set! tomorrowMonth 3)
											"Invalid Input")))])
				(list tomorrowMonth tomorrowDay tomorrowYear)))))))))


(generateTester 'nextdate #f #t domain)
(generateTester 'nextdate #t #t domain)
