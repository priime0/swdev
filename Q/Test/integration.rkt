#lang racket

(require json)
(require threading)
(require racket/place/distributed)

(define dirs '("7"))

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
  (define expected-port (open-input-file output-file))
  (define expected (read-json expected-port))
  (close-input-port expected-port)

  (define command (racket-path))
  (define script-file (path->string script))
  
  (define-values (s result-port op err-port)
    (subprocess #f in-port #f command script-file))
  (subprocess-wait s)
  (close-input-port in-port)

  (define result
    (let/ec return
      (unless (zero? (subprocess-status s))
        (displayln "ERRORED")
        (display "test: ")
        (println input-file)
        (displayln "error:")
        (displayln (port->string err-port))
        (return #f))

      (define result (read-json result-port))
      (unless (equal? expected result)
        (displayln "DIFFERENT OUTPUT")
        (display "test: ")
        (println input-file)
        (displayln "expected:")
        (println expected)
        (displayln "received:")
        (println result)
        (return #f))

      #t))

  (close-input-port result-port)
  (when op (close-output-port op))
  (close-input-port err-port)

  result)

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
                  #:when (path-has-extension? file-path ".rkt"))
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
          (~>> (build-path test-dir "grade")
               directory-list
               (filter (compose string-numeric? path->string))))

        (for ([other-dir other-test-dirs])
          (define other-test-dir (simplify-path (build-path test-dir "grade" other-dir)))
          (define inputs  (filter-input-files other-test-dir))
          (define outputs (filter-output-files other-test-dir))

          (define-values (passed^ failed^ total^)
            (run-test-dir script inputs outputs))

          (set! passed+ (+ passed+ passed^))
          (set! failed+ (+ failed+ failed^))
          (set! total+  (+ total+ total^)))))

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
