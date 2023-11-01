#lang racket

(require json)


(define dirs '("3" "4" "5" "6"))

(define root-directory (build-path (current-directory) 'up 'up))

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
  (define expected (read-json (open-input-file output-file)))

  (define command "/usr/bin/racket")
  (define script-file (path->string script))

  (define-values (s result-port _ err-port)
    (subprocess #f in-port #f command script-file))
  (subprocess-wait s)

  (let/ec return
    (unless (zero? (subprocess-status s))
      (displayln "ERRORED")
      (display "location: ")
      (println input-file)
      (displayln "error:")
      (displayln (port->string err-port))
      (return #f))

    (define result (read-json result-port))
    (unless (equal? expected result)
      (displayln "DIFFERENT OUTPUT")
      (display "location: ")
      (println input-file)
      (displayln "expected:")
      (println expected)
      (displayln "received:")
      (println result)
      (return #f))

    #t))

(module+ main
  (define passed* 0)
  (define failed* 0)
  (define total* 0)


  (for ([dir dirs]
        [test-dir test-directories])
    (define passed 0)
    (define failed 0)
    (define total 0)

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

    (for ([input-file inputs1]
          [output-file outputs1]
          [i (in-naturals)])
      (define r (run-script-for-tests script input-file output-file))
      (set! total (add1 total))
      (if r
          (set! passed (add1 passed))
          (set! failed (add1 failed))))

    (set! passed* (+ passed* passed))
    (set! failed* (+ failed* failed))
    (set! total*  (+ total* total))

    (printf "~a/~a passed\n" passed total))

  (displayln "=================================")
  (displayln "TOTAL")
  (displayln "=================================")
  (printf "~a/~a passed\n" passed* total*)

  (unless (= passed* total*)
    (exit 1)))
