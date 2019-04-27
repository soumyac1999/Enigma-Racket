#lang racket

(require "matrix-mult.rkt")

(provide prefix-matcher)
(provide decrypt-text)

(define NUMCHAR 26)
(define CHAR-OFFSET 65)

(define (decrypt-text e-m l m r)
  (begin 
    ((set-enigma-mode! 'decrypt) l m r)
    (map (lambda(c) (cond [(and (> (char->integer c) 64) (< (char->integer c) 91)) (convert-char! c)]
                          [else c])) (string->list e-m))))

(define (prefix-matcher known-prefix decrypted-message) ;; passed as list of characters
  (match (cons known-prefix decrypted-message)
    [(cons '() _) #t]
    [(cons _ '()) #f]
    [(cons (cons a kn-p) (cons a d-m)) (prefix-matcher kn-p d-m)]
    [_ #f]))


