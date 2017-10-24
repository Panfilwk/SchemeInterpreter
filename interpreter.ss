; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form
      (empty-env))))

; eval-exp is the main component of the interpreter

(define eval-exp
  (lambda (exp env)
    (cases expression exp
      [lit-exp (datum) (if (pair? datum) (cadr datum) datum)]
      [var-exp (id)
        (apply-env env id; look up its value.
           identity-proc
           (lambda ()
              (apply-env global-env
                id
                identity-proc
                (lambda ()
                  (eopl:error 'apply-env "variable not found in environment: ~s"
                    id)))))] 
      [app-exp (rator rands)
        (let ([proc-value (eval-exp rator env)]
              [args (eval-rands rands env)])
          (apply-proc proc-value args))]
      [if-exp (test then other)
        (if (eval-exp test env) (eval-exp then env) (eval-exp other env))]
      [let-exp (vars vals bodies)
        (eval-bodies bodies (extend-env vars (eval-rands vals env) env))]
      [lambda-exp (args vargs bodies)
        (if (null? vargs)
          (lambda-proc args bodies env)
          (var-lambda-proc (cons vargs args) bodies env))]
      [while-exp (test bodies)
        (if (eval-exp test env)
          (begin
            (eval-bodies bodies env)
            (eval-exp exp env)))]
      [case-exp (test vals bodies)
        (let ([test-val (eval-exp test env)])
          (cond
            [(null? vals) (void)]
            [(eq? (car vals) 'else) (eval-exp (car bodies) env)]
            [(member test-val (car vals))
                (eval-exp (car bodies) env)]
            [else 
              (eval-exp (case-exp test (cdr vals) (cdr bodies)) env)]))]
      [set!-exp (var val)
        (set-box!
          (apply-env-ref env var
            identity-proc
            (lambda () (eopl:error 'apply-env-ref "variable not found in environment: ~s" var)))
          (eval-exp val env))]
      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

(define (identity-proc x) x)

; evaluate the list of operands, putting results into a list

(define eval-rands
  (lambda (rands env)
    (map (lambda (e) (eval-exp e env)) rands)))

; evaluate the bodies of a let or lambda, returning the last result

(define (eval-bodies bodies env)
  (if (null? (cdr bodies))
    (eval-exp (car bodies) env)
    (begin (eval-exp (car bodies) env) (eval-bodies (cdr bodies) env))))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value args)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args)]
      [lambda-proc (vars bodies env)
        (if (= (length vars) (length args))
          (eval-bodies bodies (extend-env vars args env))
          (eopl:error 'apply-proc "Incorrect number of arguments passed to procedure"))]
      [var-lambda-proc (vars bodies env)
        (let* ([numargs (- (length vars) 1)]
               [rest (del-first-n args numargs)])
          (eval-bodies bodies (extend-env vars (cons rest args) env)))]
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))

(define (del-first-n lst n)
  (if (zero? n)
    lst
    (del-first-n (cdr lst) (- n 1))))

(define *prim-proc-names* '(+ - * / add1 sub1 zero? cons list length car cdr cadr cddr cdar caar cadar
  not null? eq? equal? atom? list? pair? procedure? vector? number? symbol? = < <= > >=
  list->vector vector->list vector vector-ref set-car! set-cdr! vector-set! apply map void quotient))

(define global-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
     (map prim-proc      
          *prim-proc-names*)
     (empty-env)))

; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.

(define apply-prim-proc
  (lambda (prim-proc args)
    (case prim-proc
      [(+) (apply + args)]
      [(-) (apply - args)]
      [(*) (apply * args)]
      [(/) (/ (1st args) (2nd args))]
      [(add1) (+ (1st args) 1)]
      [(sub1) (- (1st args) 1)]
      [(zero?) (zero? (1st args))]
      [(cons) (cons (1st args) (2nd args))]
      [(list) (apply list args)]
      [(length) (length (1st args))]
      [(car) (car (1st args))]
      [(cdr) (cdr (1st args))]
      [(cadr) (cadr (1st args))]
      [(cddr) (cddr (1st args))]
      [(cdar) (cdar (1st args))]
      [(caar) (caar (1st args))]
      [(cadar) (cadar (1st args))]
      [(not) (not (1st args))]
      [(procedure?) (proc-val? (1st args))]
      [(pair?) (pair? (1st args))]
      [(symbol?) (symbol? (1st args))]
      [(vector?) (vector? (1st args))]
      [(list?) (list? (1st args))]
      [(null?) (null? (1st args))]
      [(eq?) (eq? (1st args) (2nd args))]
      [(equal?) (equal? (1st args) (2nd args))]
      [(atom?) (atom? (1st args))]
      [(number?) (number? (1st args))]
      [(=) (= (1st args) (2nd args))]
      [(<) (< (1st args) (2nd args))]
      [(<=) (<= (1st args) (2nd args))]
      [(>) (> (1st args) (2nd args))]
      [(>=) (>= (1st args) (2nd args))]
      [(vector->list) (vector->list (1st args))]
      [(list->vector) (list->vector (1st args))]
      [(list->vector) (list->vector (1st args))]
      [(vector) (apply vector args)]
      [(vector-ref) (vector-ref (1st args) (2nd args))]
      [(set-car!) (set-car! (1st args) (2nd args))]
      [(set-cdr!) (set-cdr! (1st args) (2nd args))]
      [(vector-set!) (vector-set! (1st args) (2nd args) (3rd args))]
      [(apply) (apply-proc (1st args) (2nd args))]
      [(map) (apply map (lambda x (apply-proc (1st args) x))
        (rest args))]
      [(void) (void)]
      [(quotient) (quotient (1st args) (2nd args))]
      [else (error 'apply-prim-proc 
            "Bad primitive procedure name: ~s" 
            prim-op)])))

(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (syntax-expand (parse-exp (read))))])
      ;; TODO: are there answers that should display differently?
      (eopl:pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
  (lambda (x) (top-level-eval (syntax-expand (parse-exp x)))))
