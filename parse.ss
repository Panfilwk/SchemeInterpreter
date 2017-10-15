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
           (if (let-format? datum)
               (let-exp (map 1st (2nd datum)) (map parse-exp (map 2nd (2nd datum))) (map parse-exp (bods datum)))
               (eopl:error 'parse-exp "malformed let: ~s" datum))]
          [(eqv? (1st datum) 'let*)
           (if (let-format? datum)
               (let*-exp (map 1st (2nd datum)) (map parse-exp (map 2nd (2nd datum))) (map parse-exp (bods datum)))
               (eopl:error 'parse-exp "malformed let*: ~s" datum))]
          [(eqv? (1st datum) 'letrec)
           (if (let-format? datum)
               (letrec-exp (map 1st (2nd datum)) (map parse-exp (map 2nd (2nd datum))) (map parse-exp (bods datum)))
               (eopl:error 'parse-exp "malformed letrec: ~s" datum))]
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

(define (set!-format? datum)
    (and
        (= (length datum) 3)))

(define (parse-if datum)
    (let ([length (length datum)])
        (cond
            [(= length 3) (if-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)) (parse-exp '(void)))]
            [(= length 4) (if-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)) (parse-exp (4th datum)))]
            [else (eopl:error 'parse-exp "malformed if: ~s" datum)])))

(define (app-format? datum)
    (and
        (list? datum)))









