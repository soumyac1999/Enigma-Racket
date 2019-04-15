#lang racket

(require racket/gui)

(define enigma-canvas%
  (class canvas%
    (super-new)
    (inherit get-dc get-width get-height)
    (define current 0)

    (define r1 (build-vector
                10
                (λ (x) (make-object bitmap% "white.jpeg"))))
    (define r2 (build-vector
                9
                (λ (x) (make-object bitmap% "white.jpeg"))))
    (define r3 (build-vector
                8
                (λ (x) (make-object bitmap% "white.jpeg"))))

    (define/override (on-paint)
      (define dc (get-dc))
      (define w (get-width))
      (define h (get-width))

      (for ([i (in-range 10)])
        (send dc draw-bitmap
              (vector-ref r1 i)
              (* i (/ h 10)) 0))
      (for ([i (in-range 9)])
        (send dc draw-bitmap
              (vector-ref r2 i)
              (+ 30 (* i (/ h 10))) 75))
      (for ([i (in-range 8)])
        (send dc draw-bitmap
              (vector-ref r3 i)
              (+ 60 (* i (/ h 10))) 150)))



    ))


(define frame (new frame%
                   [label "Example"]
                   [width 750]
                   [height 225]))
(new enigma-canvas%
     [parent frame])

(send frame show #t)
