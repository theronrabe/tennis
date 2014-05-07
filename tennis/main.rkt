#lang racket

(require "domain/domain.rkt" "permutation/domainCases.rkt")
(provide (contract-out
		[generateTester	(-> symbol? boolean? boolean? domain? any)]))

;
;generateTestCases returns a list of executable test calls according to a domain and the worst-case? and robust? flags.
;
	(define (generateTestCases foo worst-case? robust? domain ns)
		(map (lambda (x) `(check-equal? ,(cons foo x) ',(apply (eval foo ns) x)))
			((if worst-case? worst-case better-case) (if robust? (robust (BVA domain)) (BVA domain)))))

;
;generateTester turns a specified function into an executable script that tests that function.
;
	(define (generateTester foo worst-case? robust? domain)
		;Set some local abstractions...
		(let* ( [path (path->string (find-system-path 'run-file))]
			[out (string-append "tests/" (symbol->string foo) (if robust? ".robBVA" ".BVA") (if worst-case? ".wc" ".bc") ".rkt")]
			[ns (module->namespace path)])

			;If a tester needs to be generated, create it and write its code
			(if (or (not (equal? path (string-replace path "BVA" ""))) (file-exists? out))
				(void)
				(with-output-to-file out
					(lambda () (begin (printf (string-append "#lang racket\n(require rackunit)\n(require \"../" path "\")\n\n"))
							(foldl (lambda (x acc) (begin (write x) (printf "\n")))
								0
								(generateTestCases foo worst-case? robust? domain ns))))))))
