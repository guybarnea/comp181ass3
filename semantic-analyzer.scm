(load "tag-parser.scm")



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



(define lambda-args->lst
  (lambda (el)
    (let ((lambda-type (car el))
	  (args (cadr el)))
    (cond
      ((equal? lambda-type 'lambda-simple) args)
      ((equal? lambda-type 'lambda-opt) (append args (list (caddr el))))
      (else (list args))))   
))



(define get-lambda-body
  (lambda (el)
    (let ((lambda-type (car el)))
      (cond
	((equal? lambda-type 'lambda-opt) (cadddr el))
	(else (caddr el))))   
))


(define get-lambda-args
  (lambda (el)
    (let ((lambda-type (car el))
	  (args (cadr el)))
    (cond
      ((null? args) '(()))
      ((equal? lambda-type 'lambda-simple) `(,args))
      ((equal? lambda-type 'lambda-opt) `(,args ,(caddr el)))
      (else (list args))))   
))

(define lambda?
  (lambda (exp)
    (or 
      (equal? exp 'lambda-var)
      (equal? exp 'lambda-simple)
      (equal? exp 'lambda-opt))
))  

(define replace-bound
  (lambda (var major minor exp)
      (cond
	((null? exp) exp)
	((list? exp)
	    (cond
	      ((and (= 2 (length exp)) (equal? (car exp) 'var) (equal? (cadr exp) var))
		`(bvar ,var ,major ,minor))
	      ((and (lambda? (car exp)) (not (member var (lambda-args->lst exp)))) ; bound
		`(,(car exp) ,@(get-lambda-args exp) 
		  ,(replace-bound var (+ 1 major) minor (get-lambda-body exp))))
	      ((lambda? (car exp)) exp)
	      (else (map (lambda (x) (replace-bound var major minor x)) exp))))
	(else exp))
))	

(define replace-param
  (lambda (var minor exp)
      (cond
	((null? exp) exp)
	((list? exp)
	    (cond
	      ((and (= 2 (length exp)) (equal? (car exp) 'var) (equal? (cadr exp) var))
		`(pvar ,var ,minor))
	      ((and (lambda? (car exp)) (not (member var (lambda-args->lst exp)))) ; bound
		`(,(car exp) ,@(get-lambda-args exp) ,(replace-bound var 0 minor (get-lambda-body exp))))
	      ((lambda? (car exp)) exp)  	  
	      (else (map (lambda (x) (replace-param var minor x)) exp))))
	(else exp))
))	

(define replace-params
  (lambda (vars minor exp)
    (if (null? vars) exp
	(replace-params (cdr vars) (+ minor 1) (replace-param (car vars) (+ minor 1) exp)))
))

(define search-param-bound-vars
    (lambda (exp)
      (cond
	((null? exp) exp)
	((list? exp)
	    (cond 
	      ((and (not (null? exp)) (lambda? (car exp)))
		`(,(car exp) ,@(get-lambda-args exp)
		  ,(search-param-bound-vars (replace-params 
		      (lambda-args->lst exp)
		      -1 
		      (get-lambda-body exp)))))
	      (else (map search-param-bound-vars exp))))
	(else exp))
))

(define pe->lex-pe
    (lambda (exp)
      (letrec ((exp-tag (search-param-bound-vars exp))
	       (iter (lambda (exp-tag) 
		      (cond
			((null? exp-tag) exp-tag)
			((list? exp-tag)
			    (cond
			      ((and (= 2 (length exp-tag)) (equal? (car exp-tag) 'var))
				`(fvar ,(cadr exp-tag)))	  
			      (else (map iter exp-tag))))
			(else exp-tag)))))
	  (iter exp-tag))
))


; (define box-set
;   ;; fill in the variable boxing details here
;   )

; (define annotate-tc
;   ;; fill in the tail-call annotation details here
;   )
