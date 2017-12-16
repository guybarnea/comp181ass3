(define applic-lambda-simple-nil?
  (lambda (exp)
  	(if (and (> (length exp) 1) (list? exp) (eq? (car exp) 'applic) (list? (cadr exp)))
  		(let ((expr (cadr exp)))
  			 (if (and (> (length expr) 2) (eq? (car expr) 'lambda-simple) (null? (cadr expr)))	 
  			 (caddr expr)
  			 '()))
  			 '())))




(define remove-applic-lambda-nil-tag
	(lambda (exp)

  		(let ((body (applic-lambda-simple-nil? exp)))
  			(cond
  			((null? body) exp)
  			((list? body) (remove-applic-lambda-nil body))
  			(else body)))))

(define remove-applic-lambda-nil
  (lambda (lst)
    (map (lambda (el) 
	    (cond 
	    ((list? el) (remove-applic-lambda-nil el))
	    (else el))) (remove-applic-lambda-nil-tag lst))
))  

; (define box-set
;   ;; fill in the variable boxing details here
;   )

; (define pe->lex-pe
;   ;; fill in the lexical addressing details here
;   )

; (define annotate-tc
;   ;; fill in the tail-call annotation details here
;   )
