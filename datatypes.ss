
;; Parsed expression datatypes

(define-datatype expression expression?
  [var-exp
   (id symbol?)]
  [lit-exp
   (id lit-format?)]
  [if-exp
   (test expression?)
   (then expression?)
   (other expression?)]
  [set!-exp
   (var symbol?)
   (val expression?)]
  [lambda-exp
   (args (list-of symbol?))
   (vargs (lambda (x) (or (symbol? x) (null? x))))
   (bodies (list-of expression?))]
  [let-exp
   (vars (list-of symbol?))
   (vals (list-of expression?))
   (bodies (list-of expression?))]
  [let*-exp
   (vars (list-of symbol?))
   (vals (list-of expression?))
   (bodies (list-of expression?))]
  [letrec-exp
   (vars (list-of symbol?))
   (vals (list-of expression?))
   (bodies (list-of expression?))]
  [app-exp
   (rator expression?)
   (rands (list-of expression?))]
  [while-exp
   (test expression?)
   (bodies (list-of expression?))]
  [case-exp
   (test expression?)
   (vals list?)
   (bodies (list-of expression?))])

;; environment type definitions

(define scheme-value?
  (lambda (x) #t))

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (env environment?)))

; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)]
  [lambda-proc
   (args (list-of symbol?))
   (bodies (list-of expression?))
   (env environment?)]
  [var-lambda-proc
   (args (list-of symbol?))
   (bodies (list-of expression?))
   (env environment?)])