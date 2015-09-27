(use srfi-1)

(define (self-evaluating? form)
  (or (string? form)
      (number? form)
      (null? form)
      (boolean? form)))

(define variable? symbol?)

(define (search-var var env)
  (if (null? env)
      (error 'unknown-variable: var)
      (cond [(assoc var (car env)) => (^(x) x)]
            [else
             (search-var var (cdr env))])))

(define (var-value var env)
  (cdr (search-var var env)))

(define (dump-env env)
  (define (val->printable val)
    (if (compound-proc? val) (car val) val))
  ;;
  (let loop ([env (reverse env)] [lv 0])
    (when (not (null? env))
      (dolist (binding (car env))
        (let ([indent (make-string (* 2 lv) #\space)]
              [var (car binding)]
              [val (cdr binding)])
          (format #t "~a~a: ~a\n" indent var (val->printable val))))
      (loop (cdr env) (+ lv 1)))))

(define (make-compound-proc env args body)
  (list 'compound-proc env args body))

(define proc-type first)
(define proc-env  second)
(define proc-args third)
(define proc-body fourth)

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

(define (do-let-form form env)
  (define (tr binds)
    (fold (lambda (bind res)
            (let ([f (car bind)]
                  [a (cadr bind)])
              (cons (cons f (car res))
                    (cons a (cdr res)))))
          '(() . ()) binds))
  (let* ([binds (cadr form)]
         [body (cddr form)]
         [transposed (tr binds)])
    (let ([formal-args (car transposed)]
          [actual-args (cdr transposed)])
      (apply-proc-main
       (make-compound-proc env formal-args body)
       (map (lambda (a) (eval a env)) actual-args)))))

(define (do-if-form form env)
  (let ([condition (cadr form)]
        [then-clause (caddr form)]
        [else-clause (cadddr form)])
    (if (eval condition env)
        (eval then-clause env)
        (eval else-clause env))))

(define (do-define-form form env)
  (let ([var (cadr form)]
        [def (caddr form)])
    (when (not (symbol? var))
      (error 'malformed-define: var))
    (let ([binding (cons var (eval def env))]
          [current-frame (car env)])
      (set-car! env (cons binding current-frame)))))

(define (do-set!-form form env)
  (let ([var (cadr form)]
        [def (caddr form)])
    (when (not (symbol? var))
      (error 'malformed-set!: var))
    (let ([binding (search-var var env)])
      (set-cdr! binding (eval def env)))))

(define (do-special-form form env)
  (let ((type (car form)))
    (cond
     [(eq? type 'lambda) (make-compound-proc env (cadr form) (cddr form))]
     [(eq? type 'quote) (cadr form)]
     [(eq? type 'let) (do-let-form form env)]
     [(eq? type 'if) (do-if-form form env)]
     [(eq? type 'define) (do-define-form form env)]
     [(eq? type 'set!) (do-set!-form form env)]
     [else
      (error 'unkonwn-special-form)])))

(define proc-application? pair?)

(define (primitive-proc? val)
  (and (pair? val)
       (eq? 'primitive (car val))))

(define (compound-proc? val)
  (and (pair? val)
       (eq? 'compound-proc (car val))))

(define (apply-primitive-proc proc args)
  (apply (cadr proc) args))

(define (apply-compound-proc proc args)
  (let* ([frame (map cons (proc-args proc) args)]
         [new-env (cons frame (proc-env proc))])
    (let loop ([res #f]
               [body (proc-body proc)])
      (if (null? body)
          res
          (loop (eval (car body) new-env)
                (cdr body))))))

(define (apply-proc-main proc args)
  (cond
   [(primitive-proc? proc) (apply-primitive-proc proc args)]
   [(compound-proc? proc) (apply-compound-proc proc args)]
   [else
    (error 'malformed-proc: proc)]))

(define (apply-proc form env)
  (let ([proc (eval (car form) env)]
        [args (map (lambda (f) (eval f env)) (cdr form))])
    (apply-proc-main proc args)))

(define (eval form env)
  (format #t "<--~a\n" form)
  (dump-env env)
  (format #t "--->\n")
  (cond
   [(self-evaluating?  form) form]
   [(variable?         form) (var-value form env)]
   [(special-form?     form) (do-special-form form env)]
   [(proc-application? form) (apply-proc form env)]
   [else
    (error 'unknown-form)]))

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
          (prim '<= <=))
         )))

(define (test)
  (init)
  (for-each
   (lambda (f) (format #t "~a:\n  ~a\n" f (eval f *env*)))
   '(;;
     0
     ;;
     #f
     ;;
     #t
     ;;
     "a"
     ;;
     (lambda (x) (+ 1 x))
     ;;
     ((lambda (x) (+ 1 x)) 0)
     ;;
     (((lambda (x) (lambda (y) (+ x y))) 1) 1)
     ;;
     (let ((a 1) (b 2)) (+ 1 2))
     ;;
     ((let ((a 1) (b 2)) (lambda (c) (+ a b c))) 3)
     ;;
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
     ;;
     (if #f 1 2)
     ;;
     (define fact (lambda (n) (if (= n 1) 1 (* n (fact (- n 1))))))
     ;;
     (fact 5)
     ;;
     (set! fact (lambda (x) (+ x 1)))
     ;;
     (fact 5)
     )))

(test)

;; EOF
