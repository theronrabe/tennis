#lang racket

(require "permutation.rkt")
(provide (contract-out
		[better-case	(-> combination? combination?)]
		[worst-case	(-> combination? combination?)]))

;
;Auxillary combinator, returns the set of nominals from a domain, not including the nominal of argument x.
;
	(define (otherNoms x xs) (if (pair? xs) (if (equal? x 0)
							;replace x's nominal with 'X, to preserve position
							(cons 'X (otherNoms -1 (cdr xs)))

							;else grab nominal
							(cons (car (drop (car xs) (floor (/ (length (car xs)) 2)))) (otherNoms (- x 1) (cdr xs))))
						null))

;
;Domain combinator functions
;
	;Turn a domain? into its worst-case set of combinations
	(define (worst-case domain)
		;Recursively calculate cross products
		(define (wc d) (if (pair? (cdr d)) (crossproduct (car d) (wc (cdr d))) (car d)))
		(remove-duplicates (wc domain)))

	;Turn a domain? into its single-fault set of combinations
	(define (better-case domain)
		;A partition of the output cases
		(define (portion i x) (cross (otherNoms i domain) x))

		;Remember that parameter position was preserved with 'X marking
		(define (fix-subportions p) (drop-right (replace 'X (last p) p) 1))

		;Create partitions and fix them
		(remove-duplicates (cdr (foldl (lambda (x acc) (cons (+ (car acc) 1) (append (map fix-subportions (portion (car acc) x)) (cdr acc))))
						'(0)
						domain))))
