; This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

; Procedures to make the parser a little bit saner.

(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
(define 4th cadddr)
(define rest cdr)
(define bods cddr)

(define parse-exp         
  (lambda (datum)
    (cond
        [(symbol? datum)(var-exp datum)]
        [(lit-format? datum) (lit-exp datum)]
        [(pair? datum)
            (cond
                [(eqv? (1st datum) 'if) (parse-if datum)]
                [(eqv? (1st datum) 'cond) (parse-cond datum)]
                [(eqv? (1st datum) 'and)
                    (and-exp (map parse-exp (rest datum)))]
                [(eqv? (1st datum) 'or)
                    (or-exp (map parse-exp (rest datum)))]
                [(eqv? (1st datum) 'set!)
                    (if (set!-format? datum)
                        (set!-exp (2nd datum) (parse-exp (3rd datum)))
                        (eopl:error 'parse-exp "malformed set!: ~s" datum))]
                [(eqv? (1st datum) 'lambda)
                    (if (lambda-format? datum)
                        (let ([args (parse-args (2nd datum))])
                                (lambda-exp (car args) (cdr args) (map parse-exp (bods datum))))
                        (eopl:error 'parse-exp "malformed lambda: ~s" datum))]
                [(eqv? (1st datum) 'let)
                    (cond
                        [(let-format? datum)
                            (let-exp (map 1st (2nd datum)) (map parse-exp (map 2nd (2nd datum))) (map parse-exp (bods datum)))]
                        [(named-let-format? datum)
                            (named-let-exp (2nd datum) (map 1st (3rd datum)) (map parse-exp (map 2nd (3rd datum))) (map parse-exp (cdddr datum)))]
                        (eopl:error 'parse-exp "malformed let: ~s" datum))]
                [(eqv? (1st datum) 'let*)
                    (if (let-format? datum)
                        (let*-exp (map 1st (2nd datum)) (map parse-exp (map 2nd (2nd datum))) (map parse-exp (bods datum)))
                        (eopl:error 'parse-exp "malformed let*: ~s" datum))]
                [(eqv? (1st datum) 'letrec)
                    (if (let-format? datum)
                        (letrec-exp (map 1st (2nd datum)) (map parse-exp (map 2nd (2nd datum))) (map parse-exp (bods datum)))
                        (eopl:error 'parse-exp "malformed letrec: ~s" datum))]
                [(eqv? (1st datum) 'while)
                    (if (>= (length datum) 3)
                        (while-exp (parse-exp (2nd datum)) (map parse-exp (bods datum)))
                        (eopl:error 'while-exp "bad while format"))]
                [(eqv? (1st datum) 'case)
                    (if (>= (length datum) 3)
                        (case-exp (parse-exp (2nd datum)) (map 1st (bods datum)) (map parse-exp (map 2nd (bods datum))))
                        (eopl:error 'case-exp "bad case format"))]
                [(eqv? (1st datum) 'begin)
                    (begin-exp (map parse-exp (rest datum)))]
                [(eqv? (1st datum) 'define)
                    (if (>= (length datum) 3)
                        (def-exp (2nd datum) (parse-exp (3rd datum)))
                        (eopl:error 'define-exp "bad define format"))]
                [else
                    (if (app-format? datum)
                        (app-exp (parse-exp (1st datum)) (map parse-exp (rest datum)))
                        (eopl:error 'parse-exp "malformed application: ~s" datum))])]
        [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))

(define (lit-format? datum)
    (ormap 
        (lambda (pred) (pred datum))
        (list number? vector? boolean? symbol? string? (lambda (x) (and (pair? x) (eq? (car x) 'quote))) null?)))

(define (lambda-format? datum)
    (and
        (>= (length datum) 3)))

(define (parse-args arg-lst)
    (if (pair? arg-lst)
        (let ([parsed (parse-args (cdr arg-lst))])
            (if (or (symbol? (car arg-lst)) (null? (car arg-lst)))
                (cons (cons (car arg-lst) (car parsed)) (cdr parsed))
                (eopl:error 'parse-exp "all lambda arguments must be symbols")))
        (if (or (symbol? arg-lst) (null? arg-lst))
            `(() . ,arg-lst)
            (eopl:error 'parse-exp "all lambda arguments must be symbols"))))

(define (unparse-args expr varg)
    (if (null? (cdr expr))
        (cons (car expr) varg)
        (cons (car expr) (unparse-args (cdr expr) varg))))

(define (let-format? datum)
    (and
        (>= (length datum) 3)
        (list? (2nd datum))
        (andmap (lambda (a) 
            (and 
                (list? a)
                (= (length a) 2)
                (symbol? (1st a))))
            (2nd datum))))

(define (named-let-format? datum)
    (and
        (>= (length datum) 4)
        (symbol? (2nd datum))
        (list? (3rd datum))
        (andmap (lambda (a)
            (and 
                (list? a)
                (= (length a) 2)
                (symbol? (1st a))))
            (3rd datum))))

(define (set!-format? datum)
    (and
        (= (length datum) 3)))

(define (parse-if datum)
    (let ([length (length datum)])
        (cond
            [(= length 3) (if-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)) (parse-exp '(void)))]
            [(= length 4) (if-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)) (parse-exp (4th datum)))]
            [else (eopl:error 'parse-exp "malformed if: ~s" datum)])))

(define (parse-cond datum)
    (let ([tests (parse-tests-cond (cdr datum))]
          [thens (parse-thens-cond (cdr datum))]
          [other (extract-else-cond (cdr datum))])
            (cond-exp tests thens other)))

(define (parse-tests-cond arms)
    (if (null? arms)
        '()
        (let ([arm (car arms)])
            (cond
                [(eq? (car arm) 'else)
                    (parse-tests-cond (cdr arms))]
                [else
                    (cons (parse-exp (car arm)) (parse-tests-cond (cdr arms)))]))))

(define (parse-thens-cond arms)
    (if (null? arms)
        '()
        (let ([arm (car arms)])
            (cond
                [(eqv? (car arm) 'else)
                    (parse-thens-cond (cdr arms))]
                [(null? (cdr arm))
                    (cons (parse-exp (car arm)) (parse-thens-cond (cdr arms)))]
                [else
                    (cons (parse-exp (cadr arm)) (parse-thens-cond (cdr arms)))]))))

(define (extract-else-cond arms)
    (cond
        [(null? arms) (parse-exp '(void))]
        [(and (eqv? (caar arms) 'else) (null? (cdr arms))) (apply parse-exp (cdar arms))]
        [(eqv? (caar arms) 'else) (eopl:error 'parse-exp "misplaced else in clause ~s" arms)]
        [else (extract-else-cond (cdr arms))]))

(define (app-format? datum)
    (and
        (list? datum)))