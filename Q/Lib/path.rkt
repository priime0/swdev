#lang racket

(provide
 make-directory-not-exists
 delete-directory-exists)


#; {Path -> Void}
;; Make the directory if it does not exist.
(define (make-directory-not-exists dir)
  (when (not (directory-exists? dir))
    (make-directory dir)))

#; {Path -> Void}
;; Delete the directory if it exists.
(define (delete-directory-exists dir)
  (when (directory-exists? dir)
    (delete-directory/files dir)))
