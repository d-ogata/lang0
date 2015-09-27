(use srfi-1)

(define (eval form env)
  ((analyze form) env))

(define (analyze form)
  (cond
   [(self-evaluating?  form) (analyze-self-evaluating form)]
   [(variable?         form) (analyze-variable form)]
   [(special-form?     form) (analyze-special-form form)]
   [(proc-application? form) (analyze-proc-application form)]
   [else
    (error 'unknown-form)]))

;;
;; self evaluating
;;
(define (self-evaluating? form)
  (or (string? form)
      (number? form)
      (null? form)
      (boolean? form)))

(define (analyze-self-evaluating form)
  (lambda (env) form))

;;
;; variable
;;
(define variable? symbol?)

(define (search-var var env)
  (if (null? env)
      (error 'unknown-variable: var)
      (cond [(assoc var (car env)) => (^(x) x)]
            [else
             (search-var var (cdr env))])))

(define (lookup-var var env)
  (cdr (search-var var env)))

(define (dump-env env)
  (define (val->printable val)
    (if (compound-proc? val) (car val) val))
  ;
  (let loop ([env (reverse env)] [lv 0])
    (when (not (null? env))
      (dolist (binding (car env))
        (let ([indent (make-string (* 2 lv) #\space)]
              [var (car binding)]
              [val (cdr binding)])
          (format #t "~a~a: ~a\n" indent var (val->printable val))))
      (loop (cdr env) (+ lv 1)))))

(define (analyze-variable form)
  (lambda (env) (lookup-var form env)))

;;
;; special form
;;
(define (analyze-special-form form)
  (let ((type (car form)))
    (cond
     [(eq? type 'lambda) (analyze-lambda form)]
     [(eq? type 'quote) (analyze-quote form)]
     [(eq? type 'let) (analyze (let->lambda form))]
     [(eq? type 'if) (analyze-if form)]
     [(eq? type 'define) (analyze-define form)]
     [(eq? type 'set!) (analyze-set! form)]
     [else
      (error 'unkonwn-special-form)])))

(define (special-form? form)
  (and (pair? form)
       (or (memq (car form)
                 '(lambda
                   if
                   quote
                   let
                   define
                   set!
                   )))))

;
; lambda
;
(define proc-type first)
(define proc-env  second)
(define proc-args third)
(define proc-body fourth)

(define (make-proc env args body)
  (list 'compound-proc env args body))

(define lambda-args cadr)
(define lambda-body cddr)

(define (analyze-lambda form)
  (let ([args (lambda-args form)]
        [body-proc (analyze-sequence (map analyze (lambda-body form)))])
    (lambda (env)
      (make-proc env args body-proc))))

(define (analyze-sequence seq)
  (define (sequentially p1 p2)
    (lambda (env) (p1 env) (p2 env)))
  ;
  (if (null? (cdr seq))
      (lambda (env) ((car seq) env))
      (fold sequentially (car seq) (cdr seq))))

;
; quote
;
(define quote-text cadr)
(define (analyze-quote form)
  (lambda (env) (quote-text form)))

;
; if
;
(define if-condition cadr)
(define if-then-clause caddr)
(define if-else-clause cadddr)

(define (analyze-if form)
  (let ([condition-proc (analyze (if-condition form))]
        [then-clause-proc (analyze (if-then-clause form))]
        [else-clause-proc (analyze (if-else-clause form))])
    (lambda (env)
      (if (condition-proc env)
          (then-clause-proc env)
          (else-clause-proc env)))))

;
; let
;
(define let-binds cadr)
(define let-body cddr)

(define (let->lambda form)
  (define (make-lambda args body)
    (cons 'lambda (cons args body)))
  (define make-proc-application cons)
  (define (transpose binds)
    (fold (lambda (bind res)
            (let ([f (car bind)]
                  [a (cadr bind)])
              (cons (cons f (car res))
                    (cons a (cdr res)))))
          '(() . ()) binds))
  ;
  (let* ([binds (let-binds form)]
         [body (let-body form)]
         [transposed (transpose binds)])
    (let ([formal-args (car transposed)]
          [actual-args (cdr transposed)])
      (make-proc-application (make-lambda formal-args body) actual-args))))

;
; define
;
(define definition-var cadr)
(define definition-def caddr)

(define (analyze-define form)
  (let ([var (definition-var form)]
        [def-proc (analyze (definition-def form))])
    (when (not (symbol? var))
      (error 'malformed-define: var))
    (lambda (env)
      (let ([binding (cons var (def-proc env))]
            [current-frame (car env)])
        (set-car! env (cons binding current-frame))))))

;
; set!
;
(define assignment-var cadr)
(define assignment-val caddr)

(define (analyze-set! form)
  (let ([var (assignment-var form)]
        [val-proc (analyze (assignment-val form))])
    (when (not (symbol? var))
      (error 'malformed-set!: var))
    (lambda (env)
      (let ([binding (search-var var env)])
        (set-cdr! binding (val-proc env))))))

(define (make-prim-proc native-proc)
  (list 'primitive native-proc))

(define *env* #f)

(define (init)
  (define (prim sym proc)
    (cons sym (make-prim-proc proc)))
  ;;
  (set! *env* 
        (list
         (list
          (prim '+ +)
          (prim '- -)
          (prim '* *)
          (prim '/ /)
          (prim '= =)
          (prim '< <)
          (prim '> >)
          (prim '>= >=)
          (prim '<= <=)
          (prim 'car car)
          (prim 'cdr cdr)
          ))))

;;
;; procedure application
;;
(define proc-application? pair?)

(define (primitive-proc? val)
  (and (pair? val)
       (eq? 'primitive (car val))))

(define (compound-proc? val)
  (and (pair? val)
       (eq? 'compound-proc (car val))))

(define (analyze-proc-application form)
  (let ([proc-proc (analyze (car form))]
        [arg-procs (map analyze (cdr form))])
    (lambda (env)
      (proc-application-main (proc-proc env)
                             (map (lambda (ap) (ap env)) arg-procs)))))

(define (proc-application-main proc args)
  (cond
   [(primitive-proc? proc) (apply-primitive-proc proc args)]
   [(compound-proc? proc) (apply-compound-proc proc args)]
   [else
    (error 'malformed-proc: proc)]))

(define (apply-primitive-proc proc args)
  (apply (cadr proc) args))

(define (apply-compound-proc proc args)
  (let* ([frame (map cons (proc-args proc) args)]
         [new-env (cons frame (proc-env proc))])
    ((proc-body proc) new-env)))

;;
;; TEST
;;
(define (test)
  (init)
  (for-each
   (lambda (f) (format #t "~a:\n  ~a\n" f (eval f *env*)))
   '(;;
     0
     #f
     #t
     "a"
     (define a 1)
     a
     (set! a 2)
     a
     (+ 1 2)
     (+ (+ 1 2) (+ 3 4))
     (lambda (x) (+ 1 x))
     ((lambda (x) (+ 1 x)) 0)
     (((lambda (x) (lambda (y) (+ x y))) 1) 1)
     (define fact (lambda (n) (if (= n 1) 1 (* n (fact (- n 1))))))
     (fact 5)
     (set! fact (lambda (x) (+ x 1)))
     (fact 5)
     (let ((a 1) (b 2)) (+ 1 2))
     ((let ((a 1) (b 2)) (lambda (c) (+ a b c))) 3)
     (let ((a 4) (b 3))
       ((let ((a 1) (b 2))
          (lambda (c) (+ a b c)))
        (+ a b)))
     ;;
     ((let ((f (lambda (a b)
                (lambda (x) (+ x a b)))))
        (f 1 2))
      3)
     ;;
     (if #t 1 2)
     (if #f 1 2)
     'a
     '(0 1)
     )))

(test)

;; EOF
