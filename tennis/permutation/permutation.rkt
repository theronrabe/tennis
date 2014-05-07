#lang racket

(provide (contract-out
		[combination?	(-> any/c boolean?)]
		[cross		(-> any/c list? list?)]
		[crossproduct 	(-> list? list? list?)]
		[replace	(-> any/c any/c list? list?)]))

;What qualifies as a combination?
	(define (combination? C) (match C [(list (list _ ...) ...) #t] [_ #f]))

;
;Set combinator functions
;
	;Calculate a single cross
	(define (cross x ys) (foldl (lambda (y acc) (cons (flatten (list x y)) acc)) '() ys))

	;Calculate a cross product of two sets
	(define (crossproduct L1 L2) (foldl (lambda (l1 acc) (append (cross l1 L2) acc)) '() L1))

	;Replace x with y in list L
	(define (replace x y L) (map (lambda (z) (if (equal? x z) y z)) L))
