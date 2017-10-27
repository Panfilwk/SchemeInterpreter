(define (syntax-expand expr)
    (cases expression expr
        [lit-exp (datum) expr]
        [var-exp (id) expr]
        [app-exp (rator rands)
            (cases expression rator
                [var-exp (id) 
                    (case id
                        [(begin) (app-exp (lambda-exp '() '() (map syntax-expand rands)) '())]
                        [(and)
                         (if (null? (cdr rands))
                            (syntax-expand (car rands))
                            (if-exp (syntax-expand (car rands)) (syntax-expand (app-exp (var-exp 'and) (cdr rands))) (lit-exp #f)))]
                        [(or)
                         (cond
                            [(null? rands) (lit-exp #f)]
                            [(null? (cdr rands))
                                (syntax-expand (car rands))]
                             [else (let-exp
                                        '(eval)
                                        (list (syntax-expand (car rands)))
                                        (list (if-exp (var-exp 'eval) (var-exp 'eval) (syntax-expand (app-exp (var-exp 'or) (cdr rands))))))])]
                        [else (app-exp rator (map syntax-expand rands))])]
                [else (app-exp rator (map syntax-expand rands))])]
        [if-exp (test then other) (if-exp 
            (syntax-expand test) 
            (syntax-expand then) 
            (syntax-expand other))]
        [cond-exp (tests thens other)
            (cond-helper tests thens other)]
        [let-exp (vars vals bodies) (let-exp
            vars (map syntax-expand vals)
            (map syntax-expand bodies))]
        [lambda-exp (args vargs bodies) (lambda-exp
            args vargs (map syntax-expand bodies))]
        [while-exp (test bodies)
            (while-exp (syntax-expand test) (map syntax-expand bodies))]
        [case-exp (test vals bodies)
            (case-exp (syntax-expand test) vals (map syntax-expand bodies))]
        [let*-exp (vars vals bodies)
            (if (null? (cdr vars))
                (let-exp vars (map syntax-expand vals) (map syntax-expand bodies))
                (let-exp
                    (list (car vars))
                    (list (syntax-expand (car vals)))
                    (list (syntax-expand (let*-exp (cdr vars) (cdr vals) bodies)))))]
        [set!-exp (var val)
            (set!-exp var (syntax-expand val))]
        [letrec-exp (vars vals bodies)
            (let-exp
                vars
                (make-list (length vals) (lit-exp #f))
                (append (map set!-exp vars (map syntax-expand vals)) (map syntax-expand bodies)))]
        [named-let-exp (name vars vals func)
            (syntax-expand
                (letrec-exp
                    (list name)
                    (list (lambda-exp vars '() func))
                    (list (app-exp (var-exp name) vals))))]
        [def-exp (var val)
            (def-exp var (syntax-expand val))]
        [else (eopl:error 'syntax-expand "Bad abstract syntax: ~s" exp)]))

(define (cond-helper tests thens other)
    (cond
        [(null? tests) other]
        [else
            (if-exp
                (syntax-expand (car tests))
                (syntax-expand (car thens))
                (cond-helper (cdr tests) (cdr thens) other))]))