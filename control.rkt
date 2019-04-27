#lang racket

(require racket/gui)
(provide (all-defined-out))

(define button-canvas%
  (class canvas%
    (init-field encrypt)
    (init-field decrypt)
    (super-new)
    (inherit get-dc get-width get-height)

    (define en (make-object bitmap% "white.jpeg"))
    (define de (make-object bitmap% "white.jpeg"))

    (define/override (on-paint)
      (define dc (get-dc))
      (send dc draw-bitmap en 0 0)
      (send dc draw-bitmap de 0 75))

    (define/override (on-event mouse-event)
      (define dc (get-dc))
      (cond [(eq? (send mouse-event get-event-type) 'left-down)
             (let ([x (send mouse-event get-x)]
                   [y (send mouse-event get-y)])
               (if (<= y 75)
                   (encrypt)
                   (decrypt)))]))))