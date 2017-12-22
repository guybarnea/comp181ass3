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
        (and (list? exp) (not (null? exp))
         (or 	(eq? (car exp) 'lambda-simple)
          		(eq? (car exp) 'lambda-opt)))))  


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

;;;;;;;;;;;;;;;;;;;;;;;;;; box-set ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define getLambdaParameters
    (lambda (exp)
        (cond ((eq? (car exp) 'lambda-simple) (cadr exp))
              ((eq? (car exp) 'lambda-opt) (append (cadr exp) (list (caddr exp))))
              ;((eq? (car exp) 'lambda-var) (list (cadr exp)))
              (else (cadr exp)))))

(define getLambdaBody
    (lambda (exp)
        (cond ((eq? (car exp) 'lambda-simple) (caddr exp))
              ((eq? (car exp) 'lambda-opt) (cadddr exp))
              ;((eq? (car exp) 'lambda-var) (caddr exp))
              (else (caddr exp)))))
              
(define setLambdaBody
    (lambda (orgLambda newBody)
        (cond ((eq? (car orgLambda) 'lambda-simple) `(lambda-simple ,(cadr orgLambda) ,newBody))
              ((eq? (car orgLambda) 'lambda-opt) `(lambda-opt ,(cadr orgLambda) ,(caddr orgLambda) ,newBody))
              ;((eq? (car orgLambda) 'lambda-var) `(lambda-var ,(cadr orgLambda) ,newBody))
              (else #f))))
              
(define end '())

;eliminate-nested-defines-seq-pairs
; returns a list of defs, values and the rest of the body
(define endSeqPairs
    (lambda (exp defs values)
        (cond ((or (null? exp) (not (list? exp))) (list defs values exp))
              ((eq? (caar exp) 'define)
                (endSeqPairs (cdr exp)
                             (append defs (list (cadar exp)))
                             (append values (list (end (caddar exp))))))
              (else (list defs values exp)))))


;called only with lambda body
(define end-lambda-body
    (lambda (exp)
        (cond ((or (null? exp) (not (list? exp))) exp) ; (= (length exp) 1)
              ;     its a sequence       there aren't any nested defines
              ((and (eq? (car exp) 'seq) (not (= (length (car (endSeqPairs (cadr exp) '() '()))) 0)))
               (let ((esp (endSeqPairs (cadr exp) '() '()))) ; esp[0]=defs, esp[1]=values, esp[2]=rest
                    `(applic (lambda-simple 
                                ,(map cadr (car esp))
                                (seq ,(append (map (lambda (define val) `(set ,define ,val)) (car esp) (end (cadr esp)))
                                            (end (caddr esp))))
                            )
                            ,(map (lambda (_) '(const #f)) (car esp))
                    )
                ))
              ((eq? (car exp) 'define)
                (let ((esp (endSeqPairs (list exp) '() '()))) ; esp[0]=defs, esp[1]=values, esp[2]=rest
                    `(applic (lambda-simple 
                                ,(map cadr (car esp))
                                (seq ,(list (map (lambda (define val) `(set ,define ,val)) (car esp) (end (cadr esp)))
                                            (end (caddr esp))))
                             )
                             ,(map (lambda (_) '(const #f)) (car esp))
                     )
                ))
              (else (end exp)))))
        
              
;eliminate-nested-defines body
(define end
    (lambda (exp)
        (cond ((or (null? exp) (not (list? exp))) exp) ; (= (length exp) 1)
              ((lambda? exp) (setLambdaBody exp (end-lambda-body (getLambdaBody exp))))
              (else (map end exp)))))
              
(define eliminate-nested-defines 
    (lambda (exp)
        (cond ((or (null? exp) (not (list? exp))) exp)
              ((eq? (car exp) 'define)
                `(define ,(cadr exp) ,(end (caddr exp))))
              (else (end exp)))))

(define taggedSet?
    (lambda (exp)
        (and (list? exp)
             (not (null? exp)) 
             (eq? (car exp) 'set) 
             (list? (cadr exp)) 
             (= (length (cadr exp)) 2) 
             (eq? (caadr exp) 'var))))
             
(define isTheVar?
    (lambda (e v)
        (and (list? e) (= (length e) 2) (eq? (car e) 'var) (eq? (cadr e) v))))
        
(define pushExpToStartOfExp
    (lambda (exp expToPush)
        (cond ((or (null? exp) (not (list? exp))) expToPush)
              ((eq? (car exp) 'seq) `(seq ,(append (list expToPush) (cadr exp))))
              (else `(seq (,expToPush ,exp))))))
        
; var is 'a or 'b or so..
(define isLambdaAndVarIsRedeclared?
    (lambda (exp var)                                                         ;the list of parameters
        (and (lambda? exp) (not (= (length (filter (lambda (el) (eq? el var)) (getLambdaParameters exp))) 0)))))
            
; var is 'a or 'b or so..
(define isLambdaAndVarIs-NOT-Redeclared?
    (lambda (exp var)                                                    ;the list of parameters
        (and (lambda? exp) (= (length (filter (lambda (el) (eq? el var)) (getLambdaParameters exp))) 0))))

; var is 'a or 'b or so..
(define isLambdaAndVarIsRedeclaredOrIsSet?
    (lambda (exp var)
        (or (isLambdaAndVarIsRedeclared? exp var)
            (taggedSet? exp))))

; is exp fulfilling f untill there is lambda with this var inside of it which it ignores it and continue
; v   (var)  is 'a or 'b or so..
; f   (func) must receive an exp as a list and a value and checks if the list fullfills it needs
; cut (func) must receive an exp as a list and a value and cut the search in that branch if it fullfills it needs
(define fulfillFunc?
    (lambda (f v cut)
        (lambda (exp)
            (cond ((or (null? exp) (not (list? exp)) (cut exp v)) #f)
                  ((f exp v) #t)
                  (else (ormap (fulfillFunc? f v cut) exp))))))
               
; find if under an exp tree there is a var with that name (untill it get to a leaf or a lambda with redeclaration of that var)
(define isVarInsideExp?
    (lambda (exp var)
        ((fulfillFunc? isTheVar?
                       var
                       isLambdaAndVarIsRedeclared?)
         exp)))
                
; checks if the variable is bounded
; if there is a lambda in the exp (flatly - not as tree search), it search this lambda fully to find if the variable is called there
(define isVarBounded?
    (lambda (exp var)
        (let ((lambdaHandler (lambda (innerExp)
                                (and (isLambdaAndVarIs-NOT-Redeclared? innerExp var) 
                                     (isVarInsideExp? (getLambdaBody innerExp) var)))))
             (or (lambdaHandler exp)
                 (ormap lambdaHandler exp)))))
            
            
; checks flatly whether there is a set in the exp
(define isVarSet?
    (lambda (exp var)
        (and (taggedSet? exp)
             (eq? (cadadr exp) var))))

             
; find if under an exp tree there is a var with that name (untill it get to a leaf or a lambda with redeclaration of that var)
(define isVarInsideExp?
    (lambda (exp var)
        ((fulfillFunc? isTheVar?
                       var
                       isLambdaAndVarIsRedeclared?)
         exp)))             
             
             
; checks if the variable is called (excluding (set var to something))
; if there is a set in the exp (flatly - not as tree search), it search this set fully by himself to find if the variable is called there
(define isVarGet?
    (lambda (exp var)
        (let ((setHandler (lambda (innerExp)
                                (and (taggedSet? innerExp) 
                                     ((fulfillFunc? isVarGet? var isLambdaAndVarIsRedeclaredOrIsSet?) (caddr innerExp))))))
            (or (isTheVar? exp var)
                (ormap setHandler exp)))))
                
; search if all three of the requirments are fulfilled in the exp
(define isBoxNeeded?
    (lambda (exp var)
        (and ((fulfillFunc? isVarBounded? var isLambdaAndVarIsRedeclared?) exp)
             ((fulfillFunc? isVarSet? var isLambdaAndVarIsRedeclared?) exp)
             ((fulfillFunc? isVarGet? var isLambdaAndVarIsRedeclaredOrIsSet?) exp))))
     
(define replaceWithBox
    (lambda (var)
        (lambda (exp)
            (cond ((or (null? exp) (not (list? exp)) (isLambdaAndVarIsRedeclared? exp var)) exp)
                ((isTheVar? exp var) `(box-get (var ,var)))
                ((isVarSet? exp var) `(box-set (var ,var) ,((replaceWithBox var) (caddr exp))))
                (else (map (replaceWithBox var) exp))))))
                
; checks if a variable is need to be replaced and if so, replaces it.
(define lambdaCheckAndBox
    (lambda (exp var)
        (if (isBoxNeeded? exp var)
            (pushExpToStartOfExp ((replaceWithBox var) exp) `(set (var ,var) (box (var ,var))))
            exp)))
                
; replaces lst variables if needed inside expression
(define lambdaCheckAndBoxList
    (lambda (exp lst)
        (if (null? lst)
            exp
            (lambdaCheckAndBoxList (lambdaCheckAndBox exp (car lst)) (cdr lst)))))

(define box-set
    (lambda (exp)
        (cond ((or (null? exp) (not (list? exp))) exp)
              ((lambda? exp) (setLambdaBody exp 
                                            (box-set (lambdaCheckAndBoxList (getLambdaBody exp)
                                                                            (reverse (getLambdaParameters exp))))))
              (else (map box-set exp)))))

;;;;;;;;;;;;;;;;;;;;;; pe->lex-pe Guy;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define replace-bound
  (lambda (var major minor exp)
      (cond
	((null? exp) exp)
	((list? exp)
	    (cond
	      ((and (= 2 (length exp)) (equal? (car exp) 'var) (equal? (cadr exp) var))
		`(bvar ,var ,major ,minor))
	      ((and (lambda? exp) (not (member var (lambda-args->lst exp)))) ; bound
		`(,(car exp) ,@(get-lambda-args exp) 
		  ,(replace-bound var (+ 1 major) minor (get-lambda-body exp))))
	      ((lambda? exp) exp)
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
	      ((and (lambda? exp) (not (member var (lambda-args->lst exp)))) ; bound
		`(,(car exp) ,@(get-lambda-args exp) ,(replace-bound var 0 minor (get-lambda-body exp))))
	      ((lambda? exp) exp)  	  
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
     (cond ((null? exp) exp)
						((list? exp) 
							(cond ((and (not (null? exp)) (lambda?  exp))
							 `(,(car exp) ,@(get-lambda-args exp),(search-param-bound-vars (replace-params (lambda-args->lst exp) -1  (get-lambda-body exp)))))
	      (else (map search-param-bound-vars exp))))
	(else exp))
))

(define pe->lex-pe
    (lambda (exp)
      (letrec ( (exp-tag (search-param-bound-vars exp))
	       		(iter (lambda (exp-tag) (cond 	((null? exp-tag) exp-tag) 
	       										((list? exp-tag) 	(cond ((and (= 2 (length exp-tag)) (equal? (car exp-tag) 'var)) `(fvar ,(cadr exp-tag)))	  
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
	      ((lambda? exp) `(,(car exp) ,@(get-lambda-args exp) ,(anotate-tc-run (get-lambda-body exp) #t)))
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
        ((member (car exp) '(def box-set))
    `(,(car exp) ,(cadr exp) ,(anotate-tc-run (caddr exp) #f)))
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
