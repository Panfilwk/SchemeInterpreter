; Environment definitions for CSSE 304 Scheme interpreter.  
; Based on EoPL sections 2.2 and  2.3

(define empty-env
    (lambda ()
        (empty-env-record)))

(define extend-env
    (lambda (syms vals env)
        (extended-env-record syms (map box vals) env)))

(define list-find-position
    (lambda (sym los)
        (list-index (lambda (xsym) (eqv? sym xsym)) los)))

(define list-index
    (lambda (pred ls)
        (cond
            ((null? ls) #f)
            ((pred (car ls)) 0)
            (else (let ((list-index-r (list-index pred (cdr ls))))
                (if (number? list-index-r)
                    (+ 1 list-index-r)
                    #f))))))

(define apply-env-ref
    (lambda (env sym succeed fail) ; succeed and fail are "callback procedures, 
          (cases environment env       ;  succeed is appluied if sym is found, otherwise 
              [empty-env-record ()       ;  fail is applied.
                  (fail)]
              [extended-env-record (syms vals env)
                    (let ([pos (list-find-position sym syms)])
                            (if (number? pos)
                                (succeed (list-ref vals pos))
                                (apply-env-ref env sym succeed fail)))])))

(define (apply-env env sym succeed fail)
    (let ([ref (apply-env-ref env sym succeed fail)])
        (if (box? ref)
            (unbox ref)
            ref)))

(define (addr-env-ref env addr)
    (cases address addr
        [free-addr (id)
            (apply-env-ref global-env id
                identity-proc
                (lambda () (eopl:error 'addr-env-ref "Could not find free variable in global env: ~s" id)))]
        [bound-addr (depth pos)
            (addr-lookup env depth pos)]))

(define (addr-lookup env depth pos)
    (if (= depth 0)
        (cases environment env
            [empty-env-record ()
                (eopl:error 'addr-lookup "Addr out of env depth")]
            [extended-env-record (syms vals env)
                (list-ref vals pos)])
        (cases environment env
            [empty-env-record ()
                (eopl:error 'addr-lookup "Addr out of env depth")]
            [extended-env-record (syms vals env)
                (addr-lookup env (- depth 1) pos)])))

(define (addr-env env addr)
    (let ([ref (addr-env-ref env addr)])
        (if (box? ref)
            (unbox ref)
            ref)))