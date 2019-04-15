#lang racket/gui
(provide (all-defined-out))
(define (key-handle key-event) 
     (define key-value (send key-event get-key-code))
     (define listed (string->list (~a key-value)))
     (cond
       [(equal? key-value 'release)
         'done]
        [(and (= 1 (length listed))
         (char-alphabetic? (car listed)))
         (define character (char-upcase (car listed)))
         'send-char]
        ))
