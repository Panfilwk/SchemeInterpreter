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
                         (if (null? (cdr rands))
                             (syntax-expand (car rands))
                             (let-exp '(eval) (list (syntax-expand (car rands)))
                                (list (if-exp (var-exp 'eval) (var-exp 'eval) (syntax-expand (app-exp (var-exp 'or) (cdr rands)))))))]
                        [else (app-exp rator (map syntax-expand rands))])]
                [else (app-exp rator (map syntax-expand rands))])]
        [if-exp (test then other) (if-exp 
            (syntax-expand test) 
            (syntax-expand then) 
            (syntax-expand other))]
        [let-exp (vars vals bodies) (let-exp
            vars (map syntax-expand vals)
            (map syntax-expand bodies))]
        [lambda-exp (args vargs bodies) (lambda-exp
            args vargs (map syntax-expand bodies))]
        [else (eopl:error 'syntax-expand "Bad abstract syntax: ~a" exp)]))