#lang racket/gui
(require 2htdp/image)

(define knob%
  (class canvas%
    (init-field i)
    (super-new)
    (inherit get-dc get-width get-height)
    (define current 0)

    (define knob (make-object bitmap% "26.png"))
    (define num 0)

    (define/override (on-paint)
      (define dc (get-dc))
      (define w (get-width))
      (define h (get-height))
        (send dc draw-bitmap
               knob 0 0)
      )
      ))


(define frame (new frame%
                   [label "Example"]
                   [width 750]
                   [height 225]))
(new knob%
     [parent frame]
     [i 0])
(new knob%
     [parent frame]
     [i 1])
(new knob%
     [parent frame]
     [i 2])

(send frame show #t)
