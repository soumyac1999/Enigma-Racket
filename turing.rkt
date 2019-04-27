#lang racket

(require "matrix-mult.rkt")
(require "list-comprehension.rkt")
(provide the-truth)

(define (decrypt-text e-m l m r)
  (begin
    ((set-enigma-mode! 'decrypt) l m r)
    (map convert-char! (string->list e-m))))

(define (prefix-matcher known-prefix decrypted-message) ;; passed as list of characters
  (match (cons known-prefix decrypted-message)
    [(cons '() _) #t]
    [(cons _ '()) #f]
    [(cons (cons a kn-p) (cons a d-m)) (prefix-matcher kn-p d-m)]
    [_ #f]))

(define (the-truth known-prefix encrypted-message)
  (let ([L (build-list 26 (lambda(x) x))]
        [M (build-list 26 (lambda(x) x))]
        [R (build-list 26 (lambda(x) x))])
     (filter (lambda (l) (car l)) (lc (cons (prefix-matcher (string->list known-prefix) (decrypt-text encrypted-message l m r)) (list l m r)) : l <- L m <- M r <- R))))
