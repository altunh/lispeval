;;;; Scheme Interpreter in Scheme

;;; Quotation
(define (quoted? exp) (if (pair? exp) (eq? (car exp) (quote quote)) #f))
(define (text-of-quotation exp) (cadr exp))

;;; Lambdas
(define (lambda? exp) (if (pair? exp) (eq? (car exp) (quote lambda)) #f))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;;; Conditional
(define (cond? exp) (if (pair? exp) (eq? (car exp) (quote cond)) #f))
(define (if? exp) (if (pair? exp) (eq? (car exp) (quote if)) #f))
(define (true? exp) (eq? #t exp))
(define (false? exp) (eq? #f exp))

(define (eval-if exp env)
  (let ((predicate (cadr exp))
        (true-branch (caddr exp))
        (false-branch (cadddr exp)))
        (if (true? (eval predicate env))
            (eval true-branch env)
            (eval false-branch env))))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses) 'false
    (let ((first (car clauses))
          (rest (cdr clauses)))
      (if (cond-else-clause? first)
        (if (null? rest)
          (sequence->exp (cond-actions first))
          (error "Else clause isn't last: COND->IF" clauses))
        (make-if (cond-predicate first)
                  (sequence->exp (cond-actions first))
                  (expand-clauses rest))))))

;;; Begin
(define (begin? exp) (if (pair? exp) (eq? (car exp) (quote begin)) #f))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

;;; More on sequences
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

;;; Apply & Operands
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (make-procedure parameters body env) (list 'procedure parameters body env))
(define (compound-procedure? exp) (if (pair? exp) (eq? (car exp) (quote procedure)) #f))
(define (procedure-parameters p) (cadr p)) (define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

;;; Assignment
(define (assignment? exp) (if (pair? exp) (eq? (car exp) (quote set!)) #f))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
    (eval (assignment-value exp) env) env) 'ok)

;;; Definition
(define (definition? exp) (if (pair? exp) (eq? (car exp) (quote define)) #f))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) (cddr exp))))

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env) env) 'ok)

;;; Procedure Arguments
(define (list-of-values exps env)
  (if (no-operands? exps) '()
    (cons (eval (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))

;;; Self-evaluating?
(define (self-evaluating? exp)
(or (number? exp)
    (string? exp)
    (null? exp)
    (boolean? exp)
    (list? exp)))

(define (variable? exp) (symbol? exp))

;;; Environments
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())
(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars))))

;;; Eval Sequence
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

;;; Eval
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp) (lambda-body exp) env))
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp) (apply (eval (operator exp) env) (list-of-values (operands exp) env)))
        (else (error "Eval Error: Unknown expression type"))))

;;; Apply
(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
          (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
          (eval-sequence (procedure-body procedure)
            (extend-environment
              (procedure-parameters procedure)
             arguments
              (procedure-environment procedure))))
        (else (error "Apply Error: Unknown procedure type"))))
