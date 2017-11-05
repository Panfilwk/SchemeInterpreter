(define-datatype cont cont?
    (test-k 
        (then-exp expression?)
        (else-exp expression?)
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
        (k cont?)))

(define (apply-k k val)
    (cases cont k
        [test-k (then-exp else-exp env k)
            (if val
                (eval-exp then-exp env k)
                (eval-exp else-exp env k))]
        [rands-k (rands env k)
            (eval-rands rands env
                (rands-k val k))]
        [rator-k (proc k)
            (apply-proc proc val k)]
        [let-k (vars bodies env k)
            (eval-bodies bodies (extend-env vars val env) k)])