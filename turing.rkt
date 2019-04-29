#lang racket

(require "matrix-mult.rkt")

(provide prefix-matcher)
(provide convert-any-char)

(define convert-any-char
   (lambda(c) (cond [(and (> (char->integer c) 64) (< (char->integer c) 91)) (car (convert-char! c))]
                    [else c])))

(define NUMCHAR 26)
(define CHAR-OFFSET 65)


(define (split-to-words message)
  (define (split-helper msg-lst unfinished acc)
    (match msg-lst
      ['() (match unfinished
             ['() (reverse acc)]
             [word (reverse (cons (list->string word) acc))])]
      [(cons #\space rest)
       (match unfinished
         ['() (split-helper rest '() acc)]
         [word (split-helper rest '() (cons (list->string word) acc))])]
      [(cons a rest)
       (split-helper rest (append unfinished (list a)) acc)]))
  (split-helper (string->list message) '() '()))

(define (possible-words encrypted-message n)
  (filter (lambda(w)
            (= (string-length w) n))
          (split-to-words encrypted-message)))


;(define (decrypt-text e-m l m r)
;  (begin 
;    ((set-enigma-mode! 'decrypt) l m r)
;    (map convert-any-char (string->list e-m))))

(define (prefix-matcher known-prefix encrypted-message) ;; passed as list of characters
  (match (cons known-prefix encrypted-message)
    [(cons '() _) #t]
    [(cons _ '()) #f]
    [(cons (cons a kn-p) (cons b e-m))
     #:when (equal? a (convert-any-char b))
     (prefix-matcher kn-p e-m)]
    [_ #f]))

;(define (decryption-check word possible-ciphers) ;; this assume initial 