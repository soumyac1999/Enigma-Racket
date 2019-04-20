#lang racket/gui
(require 2htdp/image)
(define i 10)
(define j 12)
(define k 1)

(define (get-value num)
  (cond [(= num 0) i]
        [(= num 1) j]
        [(= num 2) k]))
(define knob%
  (class canvas%
    (init-field i)
    (super-new)
    (inherit get-dc get-width get-height)
    (define current 0)
    (define/override (on-paint)
      (define dc (get-dc))
      (define knob (make-object bitmap% (string-append
                                       "circle-"
                                       (~a (get-value i)) ".png")))
      (define knob-w (image-width knob))
      (define knob-h (image-height knob))
      (define w (get-width))
      (display w)
      (newline)
      (define h (get-height))
      (display h)
      (newline)
        (send dc draw-bitmap
               knob (/ (- w knob-w) 2) (/ (- h knob-h) 2))
      )
      ))
(define frame (new frame%
                   [label "Example"]
                   [width 750]
                   [height 400]))
(define panel (new horizontal-panel%
                   [parent frame]))
(new knob%
     [parent panel]
     [i 0]
     [style (list 'border)]
     )
(new knob%
     [parent panel]
     [i 1]
     [style (list 'border)]
     )
(new knob%
     [parent panel]
     [i 2]
     [style (list 'border)]
     )

(send frame show #t)
