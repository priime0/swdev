#lang racket

(require racket/class)

(require Q/Common/turn-action)
(require Q/Common/interfaces/playable)
(require Q/Common/interfaces/serializable)
(require Q/Lib/connection)

(require (for-syntax syntax/parse syntax/stx racket/syntax))


#; {class PlayerProxy}
;; A PlayerProxy acts as a proxy for the referee to interact with remote players via JSON. If the
;; PlayerProxy receives malformed data, then it will error.
(define player-proxy%
  (class* object% (playable<%>)
    (super-new)
    (init-field conn)
    (init-field id)

    (define/public (name)
      id)

    (define/public (setup pub-state hand)
      (define jpub (serialize pub-state))
      (define hand (serialize hand))

      (define message `("setup" (,jpub ,hand)))
      (conn-write conn message)

      (define result (conn-read conn))

      (unless (equal? result "void")
        (error 'setup "expected void, got ~a" result)))

    (define/public (take-turn pub-state)
      (define jpub (serialize pub-state))
      
      (define message `("take-turn" (,jpub)))
      (conn-write conn message)

      (define result (conn-read conn))
      
      (hash->turn-action result))

    (define/public (new-tiles tiles)
      (define jtile* (serialize tiles))

      (define message `("new-tiles" (,jtile*)))
      (conn-write conn message)

      (define result (conn-read conn))

      (unless (equal? result "void")
        (error 'new-tiles "expected void, got ~a" result)))

    (define/thing (win won?)
      (unless (equal? result "void")
        (error 'win "expected void, got ~a" result)))

#;
    (define/public (win won?)
      (define jwon (serialize won?))

      (define message `("new-tiles" (,jwon)))
      (conn-write conn message)

      (define result (conn-read conn))

      (unless (equal? result "void")
        (error 'win "expected void, got ~a" result)))))

#;
(define-syntax (define/thing stx)
  (syntax-parse stx
    [(_ (method-name args ...) exprs ...)
     (with-syntax
         ([new-ids (stx-map (lambda (id) (format-id stx "~a+" id)) #'(args ...))]
          [method-name-str (syntax->datum #'method-name)]
          [(id+-defs ...)
           (stx-map
            (lambda (id id+)
              #`(define #,id+
                  (if (list? #,id)
                      (map serialize #,id)
                      (serialize #,id))))
            #'(args ...)
            new-ids)])
       #`(define/public (method-name args ...)
           id+-defs ...
           (define message (list method-name-str (list @,new-ids)))
           (conn-write conn message)
           (define result (conn-read conn))

           exprs ...))
     
     ]))
