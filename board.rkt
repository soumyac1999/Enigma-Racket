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
                7
                (λ (x) (make-object bitmap% "white.jpeg"))))

    (define imgs (vector r1 r2 r3))
    (define (2d-vec-ref vec i j) (vector-ref (vector-ref vec i) j))

    (define my-map (make-hash (list (cons #\Q (cons 0 0))
                                    (cons #\W (cons 0 1))
                                    (cons #\E (cons 0 2))
                                    (cons #\R (cons 0 3))
                                    (cons #\T (cons 0 4))
                                    (cons #\Y (cons 0 5))
                                    (cons #\U (cons 0 6))
                                    (cons #\I (cons 0 7))
                                    (cons #\O (cons 0 8))
                                    (cons #\P (cons 0 9))
                                    (cons #\A (cons 1 0))
                                    (cons #\S (cons 1 1))
                                    (cons #\D (cons 1 2))
                                    (cons #\F (cons 1 3))
                                    (cons #\G (cons 1 4))
                                    (cons #\H (cons 1 5))
                                    (cons #\J (cons 1 6))
                                    (cons #\K (cons 1 7))
                                    (cons #\L (cons 1 8))
                                    (cons #\Z (cons 2 0))
                                    (cons #\X (cons 2 1))
                                    (cons #\C (cons 2 2))
                                    (cons #\V (cons 2 3))
                                    (cons #\B (cons 2 4))
                                    (cons #\N (cons 2 5))
                                    (cons #\M (cons 2 6)))))                   

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
      (for ([i (in-range 7)])
        (send dc draw-bitmap
              (vector-ref r3 i)
              (+ 75 (* i (/ h 10))) 150)))

    (define/override (on-char key-event)
      (define dc (get-dc))
      (define w (get-width))
      (define h (get-width))
      (define key-value (send key-event get-key-code))
      (define listed (string->list (~a key-value)))
      (cond
        [(equal? key-value 'release)
         'done]
        [(and (= 1 (length listed))
              (char-alphabetic? (car listed)))
         
         (let* ([character (char-upcase (car listed))]
                [pos (hash-ref my-map character)])
           (send (2d-vec-ref imgs (car pos) (cdr pos))
                 load-file
                 "black.jpeg")
           (for ([i (in-range 10)])
             (send dc draw-bitmap
                   (vector-ref r1 i)
                   (* i (/ h 10)) 0))
           (for ([i (in-range 9)])
             (send dc draw-bitmap
                   (vector-ref r2 i)
                   (+ 30 (* i (/ h 10))) 75))
           (for ([i (in-range 7)])
             (send dc draw-bitmap
                   (vector-ref r3 i)
                   (+ 75 (* i (/ h 10))) 150))
           (send (2d-vec-ref imgs (car pos) (cdr pos))
                 load-file
                 "white.jpeg"))]))))

(define frame (new frame%
                   [label "Enigma"]
                   [width 750]
                   [height 225]))
(new enigma-canvas%
     [parent frame])

(send frame show #t)