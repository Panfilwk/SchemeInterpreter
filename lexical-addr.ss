(define (lexical-addr exp scope-list)
    (cases expression exp
        [var-exp (id)
            (generate-address (var-exp id) scope-list)]
        [lit-exp (id)
            (lit-exp id)]
        [if-exp (test then other)
            (if-exp
                (lexical-addr test scope-list)
                (lexical-addr then scope-list)
                (lexical-addr other scope-list))]
        [set!-exp (var val)
            (set!-exp (lexical-addr var scope-list) (lexical-addr val scope-list))]
        [lambda-exp (args vargs bodies)
            (lambda-exp
                args
                vars
                (if (null? vargs)
                    (map (lambda (body) (lexical-addr body (cons args scope-list))) bodies)
                    (map (lambda (body) (lexical-addr body (cons (cons vargs args) scope-list))) bodies)))]
        [let-exp (vars vals bodies)
            (let-exp
                vars
                (map (lambda (body) (lexical-addr body scope-list)) vals)
                (map (lambda (body) (lexical-addr body (cons vars scope-list))) bodies))]
        [app-exp (rator rands)
            (app-exp
                (lexical-addr rator scope-list)
                (map (lambda (body) (lexical-addr body scope-list)) rands))]
        [while-exp (test bodies)
            (while-exp
                (lexical-addr test scope-list)
                (map (lambda (body) (lexical-addr body scope-list)) bodies))]
        [case-exp (test vals bodies)
            (case-exp
                (lexical-addr test scope-list)
                vals
                (map (lambda (body) (lexical-addr body scope-list)) bodies))]
        [def-exp (var val)
            (def-exp
                var
                (lexical-addr val scope-list))]
        [begin-exp (bodies)
            (begin-exp
                (map (lambda (body) lexical-addr body scope-list) bodies))]
        [else (eopl:error 'lexical-addr "Unable to lexically address expression ~s" exp)]))