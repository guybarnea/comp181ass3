(load "tag-parser.scm")

;;;;;;;;;;;;;;;;;;;;;; helper function ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    ((lambda-opt-var? el) (cadddr el))  	
	((equal? lambda-type 'lambda-opt) (cadddr el))
	(else (caddr el))))   
))


(define get-lambda-args
  (lambda (el)
    (let ((lambda-type (car el))
	  (args (cadr el)))
    (cond
      ((special-lambda? el) `(() ,(caddr el)))
      ((null? args) '(()))  
      ((equal? lambda-type 'lambda-simple) `(,args))
      ((equal? lambda-type 'lambda-opt) `(,args ,(caddr el)))
      (else (list args))))   
))

(define lambda?
  (lambda (exp)
    (or 
      (equal? exp 'lambda-simple)
      (equal? exp 'lambda-opt))
))  


(define var-type?
	(lambda (exp)
		(member (car exp) '(var fvar bvar pvar const))))

(define special-lambda?
	(lambda (exp)
		(and (equal? (car exp) 'lambda-opt) (null? (cadr exp)))))



;;;;;;;;;;;;;;;;;;;;;; applic-lambda-simple-nil ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;; box-set ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; (define box-set
;   ;; fill in the variable boxing details here
;   )


;;;;;;;;;;;;;;;;;;;;;; pe->lex-pe ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



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




;;;;;;;;;;;;;;;;;;;;;; annotate-tc ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define anotate-tc-run
  (lambda (exp in-tp)
      (cond
	((null? exp) exp)
	((list? exp)
	    (cond
	      ((lambda? (car exp))
		`(,(car exp) ,@(get-lambda-args exp) ,(anotate-tc-run (get-lambda-body exp) #t)))

	      ((equal? (car exp) 'applic)
			(if in-tp 
		  `(tc-applic ,@(anotate-tc-run (cdr exp) #f))
		  `(applic ,@(anotate-tc-run (cdr exp) #f))))	

	      ((equal? (car exp) 'if3)
	      	(let ((test (cadr exp))
	      		  (dit (caddr exp))
	      		  (dif (cadddr exp)))
		`(if3 ,(anotate-tc-run test #f) ,(anotate-tc-run dit in-tp)
		      ,(anotate-tc-run dif in-tp))))

	 ;      ((member (car exp) '(def set box-set))
		; `(,(car exp) ,(cadr exp) ,(anotate-tc-run (caddr exp) #f))) ------>>>> box-set not implemented

	      ((or (equal? (car exp) 'seq) (equal? (car exp) 'or))
		(let* ((op (car exp))
			   (reversed-lst (reverse (cadr exp)))
		       (last-el (car reversed-lst))
		       (list-without-last (reverse (cdr reversed-lst))))

		`(,op (,@(map (lambda (exp) (anotate-tc-run exp #f)) list-without-last) 
		     ,(anotate-tc-run last-el in-tp)))))

	      ((var-type? exp) exp)
	      (else (map (lambda (x) (anotate-tc-run x in-tp)) exp))))
	(else exp))
))

(define annotate-tc
  (lambda (exp)
    (anotate-tc-run exp #f)
))
