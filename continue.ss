(define-datatype cont cont?
    (test-k 
        (then-exp expression?)
        (else-exp expression?)
        (env environment?)
        (k cont?))
    (rator-k
        (rands (list-of expression?))
        (env environment?)
        (k cont?))
    (rands-k
        (proc proc-val?)
        (k cont?))
    (let-k
        (vars (list-of symbol?))
        (bodies (list-of expression?))
        (env environment?)
        (k cont?))
    (map-head-k
        (proc procedure?)
        (lst-cdr list?)
        (k cont?))
    (map-tail-k
        (head scheme-value?)
        (k cont?))
    (bodies-k
        (cdr-bodies (list-of expression?))
        (env environment?)
        (k cont?))
    (while-bodies-k
        (bodies (list-of expression?))
        (expr expression?)
        (env environment?)
        (k cont?))
    (while-loop-k
        (expr expression?)
        (env environment?)
        (k cont?))
    (case-k
        (vals list?)
        (bodies (list-of expression?))
        (test expression?)
        (env environment?)
        (k cont?))
    (set!-k
        (var symbol?)
        (env environment?)
        (k cont?))
    (init-k))

(define (apply-k k val)
    (cases cont k
        [init-k ()
            val]
        [test-k (then-exp else-exp env k)
            (if val
                (eval-exp then-exp env k)
                (eval-exp else-exp env k))]
        [rator-k (rands env k)
            (eval-rands rands env
                (rands-k val k))]
        [rands-k (proc k)
            (apply-proc proc val k)]
        [let-k (vars bodies env k)
            (eval-bodies bodies (extend-env vars val env) k)]
        [map-head-k (proc lst-cdr k)
            (map-cps proc lst-cdr
                (map-tail-k val k))]
        [map-tail-k (head k)
            (apply-k k (cons head val))]
        [bodies-k (cdr-bodies env k)
            (eval-bodies cdr-bodies env k)]
        [while-bodies-k (bodies expr env k)
            (if val
                (eval-bodies
                    bodies
                    env
                    (while-loop-k expr env k))
                (apply-k k (void)))]
        [while-loop-k (expr env k)
            (eval-exp expr env k)]
        [case-k (vals bodies test env k)
            (cond
                [(null? vals) (apply-k k (void))]
                [(eq? (car vals) 'else) (eval-exp (car bodies) env k)]
                [(member val (car vals))
                    (eval-exp (car bodies) env k)]
                [else 
                    (eval-exp (case-exp test (cdr vals) (cdr bodies)) env k)])]
        [set!-k (var env k)
            (apply-k
                k
                (set-box! 
                    (apply-env-ref env var
                        identity-proc
                        (lambda () (apply-env-ref global-env var
                            identity-proc
                            (lambda () (eopl:error 'apply-env-ref "variable not found in environment: ~s" var)))))
                    val))]))