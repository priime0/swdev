#lang racket

(require Q/Common/interfaces/playable)
(require Q/Common/config)
(require Q/Lib/result)

(require racket/class)
(require racket/sandbox)

(provide send/checked)

(define-syntax send/checked
  (syntax-rules ()
    [(_ obj-expr method-id payload arg ...)
     (with-handlers
         ([exn:fail?
           (λ (e) (failure payload))])
       (success
        (call-with-limits (*timeout*) #f
                          (thunk
                           (send obj-expr method-id arg ...)))))]))
