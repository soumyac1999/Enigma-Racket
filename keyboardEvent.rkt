#lang racket/gui
(require 2htdp/image)
(provide (all-defined-out))
(define i 10)
(define j 12)
(define k 1)
(define (paint-proc-form i w h)
  (lambda (dc)
  (define knob (make-object bitmap% (string-append
                                       "circles/circle-"
                                       (~a (get-value i)) ".png")))
      (define knob-w (image-width knob))
      (define knob-h (image-height knob))
        (send dc draw-bitmap
               knob (/ (- w knob-w) 2) (/ (- h knob-h) 2))
      ))
(define (update-circles in jn kn knob1 knob2 knob3)
  (set! i (+ in 1))
  (set! j (+ jn 1))
  (set! k (+ kn 1))
  (send knob1 refresh)
  (send knob2 refresh)
  (send knob3 refresh))
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
      ((paint-proc-form i (get-width) (get-height)) dc)
      )
    (define/public (redraw)
      (define dc (get-dc))
      ((paint-proc-form i (get-width) (get-height)) dc))))