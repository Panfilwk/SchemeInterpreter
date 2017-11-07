; top-level-eval evaluates a form in the global environment

(define (top-level-eval form k)
    (cond 
        [(definition? form) (eval-def form k)]
        [(begin? form) 
        (cases expression form
            [begin-exp (forms) (eval-forms forms k)]
            [else (eopl:error 'top-level-eval "To begin or not to begin, that is the question")])]
        [(expression? form) (eval-exp form (empty-env) k)]
        [else (eopl:error 'top-level-eval "Invalid top-level statement: ~s" form)]))

; eval-exp is the main component of the interpreter

(define eval-exp
    (lambda (exp env k)
        (cases expression exp
            [lit-exp (datum)
                (apply-k
                    k
                    (if (pair? datum)
                        (cadr datum)
                        datum))]
            [var-exp (id)
                (apply-k
                    k
                    (apply-env env id; look up its value.
                        identity-proc
                        (lambda ()
                            (apply-env global-env
                                id
                                identity-proc
                                (lambda ()
                                    (eopl:error 'apply-env "variable not found in environment: ~s" id))))))]
            [app-exp (rator rands)
                (eval-exp rator env
                    (rator-k rands env k))]
            [if-exp (test then other)
                (eval-exp test env (test-k then other env k))]
            [let-exp (vars vals bodies)
                (eval-rands vals env (let-k vars bodies env k))]
            [lambda-exp (args vargs bodies)
                (apply-k
                    k
                    (if (null? vargs)
                        (lambda-proc args bodies env)
                        (var-lambda-proc (cons vargs args) bodies env)))]
            [while-exp (test bodies)
                (eval-exp test env (while-bodies-k bodies exp env k))]
            [case-exp (test vals bodies)
                (eval-exp test env (case-k vals bodies test env k))]
            [set!-exp (var val)
                (eval-exp val env
                    (set!-k var env k))]
            [begin-exp (bodies)
                (eval-bodies bodies env k)]
            [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

(define (identity-proc x) x)

; evaluate the list of operands, putting results into a list

(define (eval-rands rands env k)
    (map-cps (lambda (e k) (eval-exp e env k)) rands k))

; evaluate the bodies of a let or lambda, returning the last result

(define (eval-bodies bodies env k)
  (if (null? (cdr bodies))
      (eval-exp (car bodies) env k)
      (eval-exp (car bodies) env (bodies-k (cdr bodies) env k))))

(define (eval-forms forms k)
  (if (null? (cdr forms))
      (top-level-eval (car forms) k)
      (begin (top-level-eval (car forms) k) (eval-forms (cdr forms) k))))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.

(define (apply-proc proc-value args k)
    (cases proc-val proc-value
        [prim-proc (op) (apply-prim-proc op args k)]
        [lambda-proc (vars bodies env)
            (eval-bodies bodies (extend-env vars args env) k)]
        [var-lambda-proc (vars bodies env)
            (let* ([numargs (- (length vars) 1)]
                   [rest (list-tail args numargs)])
                (eval-bodies bodies (extend-env vars (cons rest args) env) k))]
        [call/cc-proc (k) (apply-k k (car args))]
        [else (error 'apply-proc "Attempt to apply bad procedure: ~s" proc-value)]))



(define *prim-proc-names* '(+ - * / add1 sub1 zero? cons list length car cdr cadr cddr cdar caar cadar display
    not null? eq? equal? atom? list? pair? procedure? vector? number? symbol? = < <= > >= assq call/cc newline exit-list
    list->vector vector->list vector vector-ref set-car! set-cdr! vector-set! apply map void quotient append eqv? list-tail))

(define (make-init-env)         ; for now, our initial global environment only contains 
    (extend-env            ; procedure names.  Recall that an environment associates
        *prim-proc-names*   ;  a value (not an expression) with an identifier.
        (map prim-proc      
              *prim-proc-names*)
        (empty-env)))

(define global-env (make-init-env))

(define (reset-global-env)
    (set! global-env (make-init-env)))

; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.

(define (apply-prim-proc prim-proc args k)
(case prim-proc
    [(map) (map-cps (lambda (x k) (apply-proc (1st args) (list x) k)) (2nd args) k)]
    [(apply) (apply-proc (1st args) (2nd args) k)]
    [(call/cc) (apply-proc (1st args) (list (call/cc-proc k)) k)]
    [(exit-list) (apply-proc (call/cc-proc (init-k)) (list args) k)]
        [else
            (apply-k
                k
                (case prim-proc
                    [(+) (apply + args)]
                    [(-) (apply - args)]
                    [(*) (apply * args)]
                    [(/) (/ (1st args) (2nd args))]
                    [(add1) (+ (1st args) 1)]
                    [(sub1) (- (1st args) 1)]
                    [(zero?) (zero? (1st args))]
                    [(cons) (cons (1st args) (2nd args))]
                    [(list) args]
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
                    [(void) (void)]
                    [(quotient) (quotient (1st args) (2nd args))]
                    [(append) (apply append args)]
                    [(eqv?) (eqv? (1st args) (2nd args))]
                    [(list-tail) (apply list-tail args)]
                    [(assq) (assq (1st args) (2nd args))]
                    [(display) (display (1st args))]
                    [(newline) (newline)]
                    [else (error 'apply-prim-proc 
                        "Bad primitive procedure name: ~s" 
                        prim-op)]))]))


(define (map-cps proc lst k)
    (if (null? lst)
        (apply-k k '())
        (proc (car lst)
            (map-head-k proc (cdr lst) k))))


(define (eval-def def k)
    (cases expression def
        [def-exp (var val) 
            (set! global-env 
                (extend-env (list var) (list (eval-exp val (empty-env) k)) global-env))]
        [else (eopl:error 'eval-def "bad definition: ~s" def)]))

(define rep      ; "read-eval-print" loop.
    (lambda ()
        (display "--> ")
        ;; notice that we don't save changes to the environment...
        (let ([answer (top-level-eval (syntax-expand (parse-exp (read))) (init-k))])
            ;; TODO: are there answers that should display differently?
            (cond
                [(eq? (void) answer) (void)]
                [(proc-val? answer) (begin (display "#<procedure>") (newline))]
                [else (eopl:pretty-print answer) (newline)])
            (rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
    (lambda (x) (top-level-eval (syntax-expand (parse-exp x)) (init-k))))