#lang racket

(define (empty-environment)
  '())
(define empty-env? null?)

(define (extend-environment vars vals env)
  (if (= (length vars) (length vals))
       (let ((frame (make-hash (map cons vars vals))))
             (cons frame env))
      (error "no of variables and values must match" vars vals)))

(define/match (apply-env env var)
  [((? empty-env?) _) (error "identifier not defined: " var)]
  [((cons frame rest-env) var)
   (hash-ref frame var (lambda () (apply-env rest-env var)))])


(define/match (change-env env var val)
  [((? empty-env?) _ _) (error "variable is not defined: " var)]
  [((cons frame rest-env) var val)
   (let ((binding-exists? (hash-ref frame var #f)))
     (if binding-exists?
         (hash-set! frame var val)
         (change-env rest-env var val)))])

(define/match (define-variable env var val)
  [((? empty-env?) _ _) (list (make-hash (list (cons var val))))]
  [((cons frame rest-env) var val)
         (hash-set! frame var val)])



(define-struct thunk (exp env evalled) #:mutable)

(define (delay-it exp env)
  (thunk exp env #f))

(define (force-it obj)
  (cond [(and (thunk? obj) (thunk-evalled obj))
         (thunk-exp obj)]
        [(thunk? obj)
         (let ((result (actual-value (thunk-exp obj) (thunk-env obj))))
           (set-thunk-exp! obj result)
           (set-thunk-evalled! obj #t)
           result)]
        [else obj]))

(define (actual-value exp env)
  (if  (thunk? exp)
       (force-it exp)
       (force-it (exp env))))
      

(define (scm-eval exp env)
  ((analyze exp) env))


(define (analyze exp)
  (cond [(not (pair? exp))
         (cond [((disjoin number? string?) exp) (lambda (env) exp)]
               [(symbol? exp) (lambda (env) (apply-env env exp))])]
        [else
         (match exp
           [(list 'quote a)
            (lambda (env) a)]
           [(list 'if pred then-clause else-clause)
            (let [(a-pred (analyze pred))
                  (a-then (analyze then-clause))
                  (a-else (analyze else-clause))]
              (lambda (env)
                (if (true? (actual-value a-pred env))
                    (a-then env)
                    (a-else env))))]
           [(list 'define (list proc-name params ...) body ...)
            (let ((abody (analyze-sequence body)))
              (lambda (env)
                (define-variable env proc-name (make-procedure params abody env))))]
           [(list 'define var value)
            (let [(avalue (analyze value))]
              (lambda (env)
                (define-variable env var (avalue env))))]

           [(list 'and exprs ...)
            (analyze (and->if exprs))]
           [(list 'or exprs ...)
            (analyze (or->if exprs))]

           [(list 'let (list (list var val) ...) body ...)
            (analyze (cons (cons 'lambda (cons var body)) val))]  ;convert into procedure application
           
           [(list 'set! var val)
             (let [(avalue (analyze val))]
              (lambda (env)
                (change-env env var (avalue env))))]
           [(list 'begin exps ...)
            (analyze-sequence exps)]
           [(list 'cond (list test expr) ...)
            (analyze (nested-ifs test expr))]
           [(list 'lambda (list parameters ...) body ...)
            (let [(analyzed-body (analyze-sequence body))]
              (lambda (env)
                (make-procedure parameters analyzed-body env)))]
           [(list rator rands ...)
            (let [(arator (analyze rator))
                  (arands  (map analyze rands))]
              (lambda (env)
                (scm-apply (actual-value arator env) arands env)))])]))


(define/match (and->if exprs)
  [((cons expr '())) expr]
  [((cons expr exprs))
   (make-if expr (and->if exprs) false)])

(define/match (or->if exprs)
  [((cons expr '())) expr]
  [((cons expr exprs))
   (make-let 'or-it expr
             (make-if 'or-it 'or-it
                      (or->if exprs)))])

(define (make-let var val body)
  (list 'let (list (list var val)) body))
  

(define (analyze-sequence exps)
  (define (sequentially a b) (lambda (env) (a env) (b env)))
  (let loop [(aexps (map analyze exps))]
    (if (null? (cdr aexps))
        (lambda (env) ((car aexps) env))
        (sequentially (car aexps)
                      (loop (cdr aexps))))))

(define/match (nested-ifs tests exprs)
  [((cons pred '()) (cons result '()))
   (make-if pred (make-begin result) false)]
  [((cons pred preds) (cons result results))
   (make-if pred (make-begin result) (nested-ifs preds results))])


(define (make-begin body)
  (if (pair? body)
      (cons 'begin body)
      (list 'begin body)))
(define (make-if pred then-clause else-clause)
  (list 'if pred then-clause else-clause))
(define (make-procedure parameters analyzed-body env)
  (list 'closure parameters analyzed-body env))

(define (scm-apply rator rands env)
  (match rator
    [(list 'closure params body proc-env)
     (body (extend-environment params (delay-args rands env) proc-env))]
    [_
     (apply-primitive-procedure rator (force-args rands env))]))


(define (delay-args rands env)
  (map (lambda (x) (delay-it x env)) rands))

(define (force-args rands env)
  (map (lambda (x) (actual-value x env)) rands))

(define (true? x) (not (false? x)))
(define (false? x) (or (eq? x false) (eq? x 'false)))

(define (apply-primitive-procedure rator rands)
  (apply rator rands))

(define global-env
  (extend-environment '(+ - * / car cdr null? = true false)
                      (list + - * / car cdr null? = true false)
                      (empty-environment)))


(define closure? (conjoin pair? (lambda (x) (eq? (car x) 'closure))))

(define (driver-loop)
  (display "--> ")
  (init-program global-env)
  (let loop [(env global-env)
        (input (read))]
    (let ((result (actual-value (analyze input) env)))
      (cond [(void? result) 'do-nothing]
            [(closure? result)
             (match result
               [(list 'closure params body env)
                (display (list 'procedure:  params))])]
            [else
             (display result)]))
    (newline)
    (display "--> ")
    (loop env (read))))

(define (init-program env)
  (map
   (lambda (x)
     (scm-eval
      x env))
   '((define cons (lambda (a b) (lambda (p) (p a b))))
     (define (car a) (a (lambda (a b) a)))
     (define (cdr a) (a (lambda (a b) b))))))
   
    
    
        
             
