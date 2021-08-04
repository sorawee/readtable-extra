#lang racket/base

(provide make-readtable+ make-readtable++ default-action peek/read? peek-not?)

(require racket/match
         racket/string
         racket/port)

(struct read-info (rt ch mode port src line col pos))

(define current-read-info (make-parameter #f))

(define (make-readtable+ rt . args)
  (let loop ([args args] [processed-args '()])
    (match args
      [(list) (apply make-readtable rt (reverse processed-args))]
      [(list key) (make-readtable rt key)]
      [(list key mode) (make-readtable rt key mode)]
      [(list key mode action rst ...)
       (loop
        rst
        (list*
         (cond
           [(or (not (procedure? action))
                (not (procedure-arity-includes? action 6)))
            action]
           [(procedure-arity-includes? action 2)
            (case-lambda
              [(ch port)
               (define the-info (read-info rt ch mode port #f #f #f #f))
               (parameterize ([current-read-info the-info])
                 (action ch port))]
              [(ch port src line col pos)
               (define the-info (read-info rt ch mode port src line col pos))
               (parameterize ([current-read-info the-info])
                 (action ch port src line col pos))])]
           [else
            (λ (ch port src line col pos)
              (define the-info (read-info rt ch mode port src line col pos))
              (parameterize ([current-read-info the-info])
                (action ch port src line col pos)))])
         mode
         key
         processed-args))])))

(define (make-readtable++ rt . args)
  (let loop ([args args] [rt rt])
    (match args
      [(list) rt]
      [(list key) (make-readtable rt key)]
      [(list key mode) (make-readtable rt key mode)]
      [(list key mode action rst ...)

       ;; precondition: can only be used when key is a string
       (define (run-action port dispatch-thunk)
         (cond
           [(peek/read? (substring key 1 (string-length key)) port)
            (dispatch-thunk)]
           [else (default-action)]))

       (cond
         [(string? key)
          (unless (positive? (string-length key))
            (raise-argument-error
             'make-readtable++
             "string of positive length"
             key))
          (unless (memq mode '(terminating-macro non-terminating-macro dispatch-macro))
            (raise-argument-error
             'make-readtable++
             "(or/c 'terminating-macro 'non-terminating-macro 'dispatch-macro)"
              mode))
          (unless (and (procedure? action) (procedure-arity-includes? action 6))
            (raise-argument-error
             'make-readtable++
             "(procedure-arity-includes/c 6)"
             action))
          (loop rst
                (make-readtable+
                 rt
                 (string-ref key 0)
                 mode
                 (cond
                   [(procedure-arity-includes? action 2)
                    (case-lambda
                      [(ch port)
                       (run-action
                        port
                        (λ () (action ch port)))]
                      [(ch port src line col pos)
                       (run-action
                        port
                        (λ () (action ch port src line col pos)))])]
                   [else
                    (λ (ch port src line col pos)
                      (run-action
                       port
                       (λ () (action ch port src line col pos))))])))]
         [else (loop rst (make-readtable+ rt key mode action))])])))

(define (default-action [rt-config 'base])
  (match-define (read-info rt-previous ch mode port src line col pos)
    (current-read-info))

  (define rt
    (make-readtable
     (cond
       [(eq? rt-config 'base) rt-previous]
       [else rt-config])))

  (define key (and ch (format "~a" ch)))

  (define (unget-port port . prefixes)
    (input-port-append
     #f
     (open-input-string (string-append* prefixes))
     port))

  (define-values (kind proc-regular proc-dispatch)
    (readtable-mapping rt ch))

  (define (do-read port)
    (cond
      [src (read/recursive port #f #f)]
      [else (read-syntax/recursive src port #f #f)]))

  (define (make-do-read proc)
    (and
     (procedure? proc)
     (λ (port)
       (cond
         [(and (procedure-arity-includes? proc 2) (not src))
          (proc ch port)]
         [else (proc ch port src line col pos)]))))

  (define do-dispatch (make-do-read proc-dispatch))
  (define do-regular (make-do-read proc-regular))

  (cond
    [(eq? mode 'dispatch-macro)
     (cond
       [do-dispatch (do-dispatch port)]
       [else (do-read (unget-port port "#" key))])]
    [(eq? mode kind) (do-regular port)]
    [else (do-read (unget-port port key))]))

(define (peek/read? str in)
  (and (equal? str (peek-string (string-length str) 0 in))
       (read-string (string-length str) in)))

(define (peek-not? xs in)
  (for/and ([x (in-list xs)])
    (not (equal? x (peek-string (string-length x) 0 in)))))

(module+ test
  (require rackunit)

  ;; Test interaction with existing dispatch macro

  (define (read-string-for-syntax amt port src line col pos)
    (define s (read-string amt port))
    (datum->syntax #f
                   s
                   (let-values ([(l c p) (port-next-location port)])
                     (list src line col pos (and pos (- p pos))))))

  (let ()
    (define s "(#paq #px\"abc\" #pxqe #p@ #pu)")

    (define (read-one ch port src line col pos)
      (read-string-for-syntax 1 port src line col pos))

    (define (read-yield ch port src line col pos)
      (cond
        [(peek-not? '("x" "a") port) (read-string-for-syntax 1 port src line col pos)]
        [else (default-action)]))

    (parameterize ([current-readtable (make-readtable++
                                       #f
                                       "pa" 'dispatch-macro read-one
                                       "pxq" 'dispatch-macro read-one
                                       "p" 'dispatch-macro read-yield)])
      (check-equal? (read (open-input-string s)) '("q" #px"abc" "e" "@" "u")))))

(module+ test
  (let ()

    ;; Test ordering

    (define s "(#qabc@$ #qab)")

    (define (read-two ch port src line col pos)
      (read-string-for-syntax 2 port src line col pos))

    (define (read-yield ch port src line col pos)
      (cond
        [(peek-not? '("abc") port) (read-string-for-syntax 2 port src line col pos)]
        [else (default-action)]))

    (parameterize ([current-readtable (make-readtable++
                                       #f
                                       "q" 'dispatch-macro read-two
                                       "qabc" 'dispatch-macro read-two)])
      (check-equal? (read (open-input-string s)) '("@$" "ab")))

    (parameterize ([current-readtable (make-readtable++
                                       #f
                                       "qabc" 'dispatch-macro read-two
                                       "q" 'dispatch-macro read-two)])
      (check-equal? (read (open-input-string s)) '("ab" c@$ "ab")))

    (parameterize ([current-readtable (make-readtable++
                                       #f
                                       "qabc" 'dispatch-macro read-two
                                       "q" 'dispatch-macro read-yield)])
      (check-equal? (read (open-input-string s)) '("@$" "ab")))))
