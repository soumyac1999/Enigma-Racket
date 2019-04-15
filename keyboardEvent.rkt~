#lang racket/gui
(define frame (new frame% [label "Example"]))
 
; Make a static text message in the frame
(define msg (new message% [parent frame]
                          [label "No events so far..."]))
 
; Make a button in the frame
(new button% [parent frame]
             [label "Click Me"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (send msg set-label "Button click"))])
 
; Show the frame by calling its show method
(send frame show #t)

; Derive a new canvas (a drawing window) class to handle events
(define my-canvas%
  (class canvas% ; The base class is canvas%
    ; Define overriding method to handle mouse events
    (define/override (on-event event)
      (send msg set-label "Canvas mouse"))
    ; Define overriding method to handle keyboard events
    (define/override (on-char key-event)
     (define key-value (send key-event get-key-code))
     (define listed (string->list (~a key-value)))
     (cond
       [(equal? key-value 'release)
         'done]
        [(and (= 1 (length listed))
         (char-alphabetic? (car listed)))
         (define character (char-upcase (car listed)))
         (send msg set-label (~a character))]
        ))
    ; Call the superclass init, passing on all init args
    (super-new)))
 
; Make a canvas that handles events in the frame
(new my-canvas% [parent frame])