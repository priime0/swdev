#lang racket

(require json)
(require threading)
(require racket/sandbox)

(define dirs '("3"  "4" "5" "6" "7"))

(define root-directory (build-path (current-directory) 'up 'up))

(define (string-numeric? s)
  (andmap char-numeric? (string->list s)))

(define (test-input-file? f)
  (path-has-extension? f "-in.json"))

(define (test-output-file? f)
  (path-has-extension? f "-out.json"))

(define (filter-input-files dir)
  (define raw-paths (filter test-input-file? (directory-list dir)))
  (map (lambda (f) (build-path dir f)) raw-paths))

(define (filter-output-files dir)
  (define raw-paths (filter test-output-file? (directory-list dir)))
  (map (lambda (f) (build-path dir f)) raw-paths))

(define test-directories
  (for/list ([dir dirs])
    (build-path root-directory dir)))

(define (run-script-for-tests script input-file output-file)
  (define in-port (open-input-file input-file))
  (define out-port (open-output-string))
  (define expected-port (open-input-file output-file))
  (define expected (read-json expected-port))
  (close-input-port expected-port)
  

  (parameterize ([current-input-port in-port]
                 [current-output-port out-port])
    (with-handlers ([exn:fail? (lambda (e) (raise e))])
      ((dynamic-require (make-resolved-module-path script) 'main))))

  (close-input-port in-port)
  (define output-string (get-output-string out-port))
  (define actual-output (read-json (open-input-string output-string)))
  
  (cond
    [(equal? expected actual-output)
     #t]
    [else
     (displayln "DIFFERENT OUTPUT")
     (display "test: ")
     (println input-file)
     (displayln "expected:")
     (println expected)
     (displayln "received:")
     (println actual-output)
     #f]))

(define (run-test-dir script inputs outputs)
  (for/fold ([passed 0]
             [failed 0]
             [total 0])
    ([input-file inputs]
     [output-file outputs]
     [i (in-naturals)])
    (define r (run-script-for-tests script input-file output-file))
    (values (if r (add1 passed) passed)
            (if r failed (add1 failed))
            (add1 total))))


(module+ main
  (define passed* 0)
  (define failed* 0)
  (define total* 0)
  (define run-all #t)

  (for ([dir dirs]
        [test-dir test-directories])
    (displayln "=================================")
    (printf "MILESTONE ~a\n" dir)
    (displayln "=================================")

    (define script
      (for/first ([file-path (directory-list test-dir)]
                  #:when (string-suffix? (path->string file-path) ".rkt"))
        (simplify-path (build-path test-dir file-path))))

    (define group-test-dir (simplify-path (build-path test-dir "Tests")))

    (define inputs1  (filter-input-files group-test-dir))
    (define outputs1 (filter-output-files group-test-dir))

    (define-values (passed+ failed+ total+)
      (run-test-dir script inputs1 outputs1))


    (set! passed* (+ passed* passed+))
    (set! failed* (+ failed* failed+))
    (set! total*  (+ total* total+))

    (let/ec return
      (when run-all
        (define grade-dir (build-path test-dir "grade"))
        (unless (directory-exists? grade-dir)
          (return))

        (define other-test-dirs
          (cons "staff-tests"
           (~>> (build-path test-dir "grade")
                directory-list
                (filter (compose string-numeric? path->string)))))

        (for ([other-dir other-test-dirs])
          (define other-test-dir (simplify-path (build-path test-dir "grade" other-dir)))
          (define inputs  (filter-input-files other-test-dir))
          (define outputs (filter-output-files other-test-dir))

          (define-values (passed^ failed^ total^)
            (run-test-dir script inputs outputs))

          (set! passed+ (+ passed+ passed^))
          (set! failed+ (+ failed+ failed^))
          (set! total+  (+ total+ total^))
          (printf "~a/~a passed in ~a\n" passed^ total^ other-dir))))

    (printf "~a/~a passed\n" passed+ total+)

    (set! passed* (+ passed* passed+))
    (set! failed* (+ failed* failed+))
    (set! total*  (+ total*  total+)))

  (displayln "=================================")
  (displayln "TOTAL")
  (displayln "=================================")
  (printf "~a/~a passed\n" passed* total*)

  (unless (= passed* total*)
    (exit 1)))
