(load "qq.scm")
; ;;;;;;;;;;;;;;;;;;;;;; parse function ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *reserved-words*
'(and begin cond define do else if lambda
let let* letrec or quasiquote unquote
unquote-splicing quote set!))

(define handle-multi-begin
  (lambda (x)
    (cond ((null? x) '())
    ((and (list? (car x)) (equal? (caar x) 'seq))
     `(,@(cadar x) ,@(handle-multi-begin (cdr x))))
     (else `(,(car x) ,@(handle-multi-begin (cdr x)))))
))

(define handle-seq
  (lambda (exps)
    (if (equal? (length exps) 1)
	(parse (car exps))
	`(seq ,(handle-multi-begin (map parse exps)))))
)

(define flatten
  (lambda (x)
    (cond 
      ((null? x) '())
      ((pair? x) (append (flatten (car x)) (flatten (cdr x))))
      (else (list x)))
))  

(define contains-duplicates?
  (lambda (lst)
    (not (andmap 
      (lambda (x) 
	(equal? 
	  (- (length lst) 1) 
	  (length (filter (lambda (y) (not (equal? x y))) 
		    lst))
	)) 
    lst))
)) 

(define improper-list?
  (lambda (x) (and (pair? x) (not (list? x)))))

(define var?
  (lambda (var) (and (not (member var *reserved-words*)) (symbol? var))
))

(define check-simple-params?
(lambda (x) (and (list? x) (not (contains-duplicates? x)) (or (null? x) (and (list? x) (andmap var? x))))))


(define constant?
  (lambda (x) 
    (or 
      (and (symbol? x) (null? x)) 
      (vector? x) 
      (boolean? x) 
      (char? x) 
      (number? x) 
      (string? x) 
      (equal? (void) x)
    )
))

(define quote-const?
  (lambda (exp)
  	(and  (pair? exp) (eq? (car exp) 'quote))))

(define if?
  (lambda (exp)
  	(and (list? exp) (eq? (car exp) 'if) (= (length exp) 4))))

(define if-without-else?
  (lambda (exp)
  	(and (list? exp) (eq? (car exp) 'if) (= (length exp) 3))))

(define or-without-args?
  (lambda (exp)
  	 (and (list? exp) (= (length exp) 1) (eq? (car exp) 'or))))

(define or-with-one-args?
  (lambda (exp)
	 (and (list? exp) (= (length exp) 2) (eq? (car exp) 'or))))


(define or?
  (lambda (exp)
  	(and (list? exp) (> (length exp) 2) (eq? (car exp) 'or))))

(define lambda-simple?
  (lambda (exp)
  	(and (list? exp) (eq? (car exp) 'lambda) (> (length exp) 2) (check-simple-params? (cadr exp)))))

(define lambda-opt-im?
  (lambda (exp)
	(and (list? exp) (eq? (car exp) 'lambda) (> (length exp) 2) (or (improper-list? (cadr exp))))))

(define lambda-opt-var?
  (lambda (exp)
	(and (list? exp) (eq? (car exp) 'lambda) (> (length exp) 2) (var? (cadr exp)))))

(define lambda-var-improper?
  (lambda (exp)
	(and (list? exp) (eq? (car exp) 'lambda-im) (> (length exp) 2) (var? (cadr exp)))))

(define regular-define?
  (lambda (exp)
  	(and (list? exp) (eq? (car exp) 'define) (> (length exp) 2) (var? (cadr exp)))))

(define mit-style-define?
  (lambda (exp)
	(and (list? exp) (eq? (car exp) 'define) (> (length exp) 2) (list? (cadr exp)))))

(define mit-style-improper-define-one-arg?
	(lambda(exp)
			(and (list? exp) (eq? (car exp) 'define) (> (length exp) 2) (improper-list? (cadr exp)) (= (length-improper-left (cadr exp)) 1))))

;same as before for now...
(define mit-style-improper-define-more-than-one-arg?
	(lambda(exp)
		(and (list? exp) (eq? (car exp) 'define) (> (length exp) 2) (improper-list? (cadr exp)))))

(define length-improper-left
	(lambda(im-lst)
		(if (pair? im-lst)
			(+ 1 (length-improper-left (cdr im-lst)))
			0 )))
			
(define set?
  (lambda (exp)
	(and (list? exp) (eq? (car exp) 'set!) (> (length exp) 2) (var? (cadr exp)))))

(define applic?
  (lambda (exp)
  	(and (list? exp) (not (member (car exp) *reserved-words*)))))

(define let-without-bindings?
  (lambda (exp)
	(and (list? exp) (eq? (car exp) 'let) (> (length exp) 2) (null? (cadr exp)))))


(define let-with-bindings?
  (lambda (exp)
	(and (list? exp)  (eq? (car exp) 'let) (> (length exp) 2))))

(define let-star-without-bindings?
  (lambda (exp)
	(and (list? exp) (eq? (car exp) 'let*) (> (length exp) 2) (null? (cadr exp)))))

(define let-star-with-bindings?
  (lambda (exp)
	(and (list? exp) (eq? (car exp) 'let*) (> (length exp) 2))))

(define letrec-without-bindings?
  (lambda (exp)
  	(and (list? exp) (eq? (car exp) 'letrec) (> (length exp) 2) (null? (cadr exp)))))

(define letrec-with-bindings?
  (lambda (exp)
  	(and (list? exp) (eq? (car exp) 'letrec) (> (length exp) 2))))

(define and-without-args?
  (lambda (exp)
  	(and (list? exp) (eq? (car exp) 'and) (= (length exp) 1))))

(define and-with-one-arg?
  (lambda (exp)
  	(and (list? exp) (eq? (car exp) 'and) (= (length exp) 2))))

(define and?
  (lambda (exp)
  	(and (list? exp) (eq? (car exp) 'and) (> (length exp) 2))))

(define cond-with-one-exp?
  (lambda (exp)
  	(and (list? exp) (eq? (car exp) 'cond) (= (length exp) 2))))

(define cond?
  (lambda (exp)
  	(and (list? exp) (eq? (car exp) 'cond) (> (length exp) 2))))

(define cond-rec
	(lambda(lst-of-conds)
		(if (= (length lst-of-conds) 1)
			(if (eq? (caar lst-of-conds) 'else)
				`(begin ,@(cdar lst-of-conds))
				`(if ,(caar lst-of-conds) (begin ,@(cdar lst-of-conds))))
			`(if ,(caar lst-of-conds) (begin ,@(cdar lst-of-conds)) ,(cond-rec (cdr lst-of-conds))))))

(define else?
	(lambda(exp)
		(and (list? exp) (eq? (car exp) 'else) (= (length exp) 2))))

;get list and return the same list without the last element
(define get-all-but-last
	(lambda(lst)
		(if (> (length lst) 2)
			(cons (car lst) (get-all-but-last (cdr lst)))
			(cons (car lst) '()))))

(define begin-with-args?
	(lambda(exp)
		(and (list? exp) (eq? (car exp) 'begin) (> (length exp) 1))))

(define begin-without-args?
	(lambda(exp)
		(and (list? exp) (eq? (car exp) 'begin) (= (length exp) 1))))

(define quasiquote?
  (lambda (exp)
  	(and (list? exp) (eq? (car exp) 'quasiquote) (= (length exp) 2))))

(define parse
	(lambda (exp)
		(cond 
			;const
			((constant? exp) `(const ,exp))

			;quote-const
			((quote-const? exp) `(const ,@(cdr exp)))

			  ;var
			((var? exp) `(var ,exp))

			  ;if
			((if? exp)
				(let ((test (cadr exp))
					  (then (caddr exp))
					  (else (cadddr exp)))
					  `(if3 ,(parse test) ,(parse then) ,(parse else))))

			  ;if without else
			((if-without-else? exp)
				(let ((test (cadr exp))
					  (then (caddr exp)))
					   `(if3 ,(parse test) ,(parse then) ,(parse (void)))))


			  ;or-without-args
			  ((or-without-args? exp) (parse #f))

			  ;or-with-one-args
			  ((or-with-one-args? exp) (parse (cadr exp)))
			  ;or
			  ((or? exp) `(or ,(map parse (cdr exp))))

			  ;lambda-simple
			  ((lambda-simple? exp)
			  	(let ((args (cadr exp))
                     (body (cddr exp)))
			   `(lambda-simple ,args ,(handle-seq body))))

			  ;lambda-opt
			  ((lambda-opt-im? exp)
			  	(let* ((args-list (flatten (cadr exp)))
	      			   (rev-args-list (reverse args-list)))
				`(lambda-opt ,(reverse (cdr rev-args-list)) ,(car rev-args-list) ,(handle-seq (cddr exp)))))

			  ; lambda-var
			   ((lambda-opt-var? exp)
			   	(let ((arg (cadr exp))
			   		  (body (cddr exp)))
			   	`(lambda-opt () ,arg ,(handle-seq body))))

			   ; lambda-var-improper-more-than-one-arg
			   ((lambda-var-improper? exp)
			   	(let ((arg (cadr exp))
			   		  (body (cddr exp)))
			   	`(lambda-opt () ,arg  ,(handle-seq body))))

			  ;regular-define
			   ((regular-define? exp)
			   	(let ((var (cadr exp))
			   		  (val (cddr exp)))
			   	`(define ,(parse var) ,(handle-seq val))))

			   ;mit-style-define
			   ((mit-style-define? exp)
			   	(let* ((var (car (cadr exp)))
			   		   (args (cdr (cadr exp)))
			   		   (body (cddr exp)))
			   	(parse `(define ,var ,`(lambda ,args ,@body)))))

			   ;mit-style-improper-define
			   ((mit-style-improper-define-one-arg? exp)
			    (let* ((var (car (cadr exp)))
			   		   (args (cdr (cadr exp)))
			   		   (body (cddr exp)))
			   	(parse `(define ,var ,`(lambda-im ,args ,@body)))))

			   ;mit-style-improper-define-more-than-one-arg?
			   ((mit-style-improper-define-more-than-one-arg? exp)
			    (let* ((var (car (cadr exp)))
			   		   (args (cdr (cadr exp)))
			   		   (body (cddr exp)))
			   	(parse `(define ,var ,`(lambda ,args ,@body)))))

			   ;set!
			   ((set? exp)
			   	(let ((var (cadr exp))
			   		  (val (cddr exp)))
			   	 `(set ,(parse var) ,(handle-seq val))))

			   ;applic
			   ((applic? exp)
			   	(let ((first-body (car exp))
			   		  (rest-bodies (cdr exp)))
			   	`(applic ,(parse first-body) ,(map parse rest-bodies))))

			   ;let-without-bindings
			   ((let-without-bindings? exp)
			   	(let ((first-body (caddr exp))
			   		  (rest-bodies (cdddr exp)))
			   	(parse `((lambda () ,first-body ,@rest-bodies)))))

			   ;let-with-bindings
			   ((let-with-bindings? exp)
			   	(let* ((bindings (cadr exp))
			   		   (args-names (map car bindings))
			   		   (args-values (map cadr bindings)))
	      		(parse `((lambda ,args-names ,(caddr exp) ,@(cdddr exp)) ,@args-values))))

			   ;let-star-without-bindings
			   ((let-star-without-bindings? exp)
			   	(let ((first-body (caddr exp))
			   		  (rest-bodies (cdddr exp)))
			   	(parse `(let () ,first-body ,@rest-bodies))))

			   ;let-star-with-bindings
			   ((let-star-with-bindings? exp)
			   	(let ((handle-bindings (lambda (bindings body other-bodies)
				  (if (null? bindings)
				    `(,body ,@other-bodies)
				    `((let* ,bindings ,body ,@other-bodies)))))
			   		  (key (caaadr exp))
			   		  (val (cdaadr exp))
			   		  (other-bindings (cdadr exp))
			   		  (body (caddr exp))
			   		  (other-bodies (cdddr exp)))
				(parse `(let ((,key ,@val)) ,@(handle-bindings other-bindings body other-bodies)))))

			   ;letrec-without-bindings
			   ((letrec-without-bindings? exp)
			   	(let ((first-body (caddr exp))
			   		  (rest-bodies (cdddr exp)))
			   	(parse `(let () (let () ,first-body  ,@rest-bodies)))))

			   ;letrec-with-bindings
			   ((letrec-with-bindings? exp)
			   	(let* ((key (caaadr exp))
			   		  (val (cdaadr exp))
			   		  (other-bindings (cdadr exp))
			   		  (body (caddr exp))
			   		  (other-bodies (cdddr exp))
			   		  (args-names (append (list key) (map car other-bindings)))
			   		  (set-args (map (lambda (x) `(set! ,(car x) ,(cadr x))) (append (list (list key (car val))) other-bindings))))
			   		  (parse `((lambda ,args-names ,@set-args ,`(let () ,body ,@other-bodies)) ,@(make-list (length args-names) #f)))))


			   ;and-without-args
			   ((and-without-args? exp) (parse #t))

			   ;and-with-one-arg
			   ((and-with-one-arg? exp) (parse (cadr exp)))

			   ;and
			   ((and? exp)
			   (let ((first (cadr exp))
			   		(rest (cddr exp)))
			   `(if3 ,(parse first) ,(parse `(and ,@rest)) ,(parse #f))))

			   ((and? exp)
			   (let ((first (cadr exp))
			   		(rest (cddr exp)))
			   `(if3 ,(parse first) ,(parse `(and ,@rest)) ,(parse #f))))

			   ;cond-with-one-exp
			   ((cond-with-one-exp? exp)
			   	  (let ((cond (caadr exp))
			   	  		(body (cdadr exp)))
			   		(if (equal? (caadr exp) 'else)
		      		(handle-seq (cdadr exp))
		      `(if3 ,(parse cond) ,(handle-seq body) ,(parse (void))))))

			   	  ((begin-with-args? exp)
			   	  	(handle-seq (cdr exp)))

			   	  ((begin-without-args? exp)
			   	  	(parse (void)))

			   	  ((cond? exp) 
			   	  	(parse (cond-rec (cdr exp))))

			   	;quasi-quote
			   	((quasiquote? exp)
			   	  (parse (expand-qq (cadr exp))))

			(else "ERROR: no such expression\n")))
)