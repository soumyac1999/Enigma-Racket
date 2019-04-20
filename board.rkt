#lang racket

(require racket/gui)
(require "matrix-mult.rkt")
(require "keyboardEvent.rkt")

(define rotors (list 0 0 0))

(define enigma-canvas%
  (class canvas%
    (super-new)
    (inherit get-dc get-width get-height)
    (define current 0)
    (define mesg "")
    (define max-len 35)

    (define msg (new message% [parent frame]
                     [label ""]
                     [auto-resize #t]
                     [font (make-object font% 20 'script)]))
    (define letters (list #\Q #\W #\E #\R #\T #\Y #\U #\I #\O #\P #\A #\S #\D #\F #\G #\H #\J #\K #\L #\Z #\X #\C #\V #\B #\N #\M))
    (define img-map (make-hash (list (cons #\Q "letters/Q.jpg")
                                     (cons #\W "letters/W.jpg")
                                     (cons #\E "letters/E.jpg")
                                     (cons #\R "letters/R.jpg")
                                     (cons #\T "letters/T.jpg")
                                     (cons #\Y "letters/Y.jpg")
                                     (cons #\U "letters/U.jpg")
                                     (cons #\I "letters/I.jpg")
                                     (cons #\O "letters/O.jpg")
                                     (cons #\P "letters/P.jpg")
                                     (cons #\A "letters/A.jpg")
                                     (cons #\S "letters/S.jpg")
                                     (cons #\D "letters/D.jpg")
                                     (cons #\F "letters/F.jpg")
                                     (cons #\G "letters/G.jpg")
                                     (cons #\H "letters/H.jpg")
                                     (cons #\J "letters/J.jpg")
                                     (cons #\K "letters/K.jpg")
                                     (cons #\L "letters/L.jpg")
                                     (cons #\Z "letters/Z.jpg")
                                     (cons #\X "letters/X.jpg")
                                     (cons #\C "letters/C.jpg")
                                     (cons #\V "letters/V.jpg")
                                     (cons #\B "letters/B.jpg")
                                     (cons #\N "letters/N.jpg")
                                     (cons #\M "letters/M.jpg"))))
    (define r1 (build-vector
                10
                (λ (x) (make-object bitmap% (hash-ref img-map (list-ref letters x))))))
    (define r2 (build-vector
                9
                (λ (x) (make-object bitmap% (hash-ref img-map (list-ref letters (+ x 10)))))))
    (define r3 (build-vector
                7
                (λ (x) (make-object bitmap% (hash-ref img-map (list-ref letters (+ x 19)))))))

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

    (define/public (get-mesg)
      (displayln mesg))
    (define/public (clear-mesg)
      (set! mesg "")
      (on-paint)
      (send msg set-label ""))

    (define/override (on-paint)
      (define dc (get-dc))
      (define w (get-width))
      (define h (get-height))
      (match rotors
        [(list i j k) (update-circles i j k knob0 knob1 knob2)])
      (send dc set-brush (new brush% [color "black"]))
      (send dc draw-rectangle 0 0 w h)
      (for ([i (in-range 10)])
        (send dc draw-bitmap
              (vector-ref r1 i)
              (* i (/ w 10)) 0))
      (for ([i (in-range 9)])
        (send dc draw-bitmap
              (vector-ref r2 i)
              (+ 30 (* i (/ w 10))) 75))
      (for ([i (in-range 7)])
        (send dc draw-bitmap
              (vector-ref r3 i)
              (+ 75 (* i (/ w 10))) 150)))

    (define/override (on-char key-event)
      (define dc (get-dc))
      (define w (get-width))
      (define h (get-height))
      (define key-value (send key-event get-key-code))
      (define listed (string->list (~a key-value)))
      (cond
        [(equal? key-value 'release)
         'done]
        [(and (= 1 (length listed))
              (char-alphabetic? (car listed)))         
         (let* ([character (char-upcase (car listed))]
                [enc (convert-char! character)]
                [pos (hash-ref my-map (car enc))]
                [newimg (hash-ref img-map (car enc))])
           (set! rotors (cdr enc))
           (set! mesg (string-append mesg (~a (car enc))))
           (let* ([l (string-length mesg)]
                  [label (if (> l max-len)
                             (string-append "..." (substring mesg (- l max-len) l))
                             mesg)])
             (send msg set-label label))
           (match rotors
             [(list i j k) (update-circles i j k knob0 knob1 knob2)])
           (send (2d-vec-ref imgs (car pos) (cdr pos))
                 load-file
                 "letters/act.jpeg")
           (for ([i (in-range 10)])
             (send dc draw-bitmap
                   (vector-ref r1 i)
                   (* i (/ w 10)) 0))
           (for ([i (in-range 9)])
             (send dc draw-bitmap
                   (vector-ref r2 i)
                   (+ 30 (* i (/ w 10))) 75))
           (for ([i (in-range 7)])
             (send dc draw-bitmap
                   (vector-ref r3 i)
                   (+ 75 (* i (/ w 10))) 150))
           (send (2d-vec-ref imgs (car pos) (cdr pos))
                 load-file
                 newimg))]))))

(define frame (new frame%
                   [label "Enigma"]
                   [width 750]
                   [height 490]
                   [x 0]
                   [y 0]
                   ;[style (list 'no-caption 'no-resize-border)]
                   [border 1]))
(define panel (new horizontal-panel%
                   [parent frame]))
(define knob0 (new knob%
                   [parent panel]
                   [i 0]
                   [style (list 'border)]
                   ))
(define knob1 (new knob%
                   [parent panel]
                   [i 1]
                   [style (list 'border)]
                   ))
(define knob2 (new knob%
                   [parent panel]
                   [i 2]
                   [style (list 'border)]
                   ))

(define board (new enigma-canvas%
                   [parent frame]))

(send frame show #t)
(send frame enable #f)

(define (encrypt)
  (send frame enable #t)
  (set! rotors ((set-enigma-mode! 'encrypt)))
  (displayln rotors)
  ; Diable the rotor set inputs
  (match rotors
    [(list i j k) (update-circles i j k knob0 knob1 knob2)]))

(define (decrypt)
  ;Get values for rotors
  ;Disable inputs
  (define t1 (read))
  (define t2 (read))
  (define t3 (read))
  (send frame enable #t)
  (set! rotors ((set-enigma-mode! 'decrypt) t1 t2 t3))
  (match rotors
    [(list i j k) (update-circles i j k knob0 knob1 knob2)]))

(define (done)
  (send board get-mesg)
  (send board clear-mesg)
  (set! rotors '(0 0 0))
  (update-circles 0 0 0 knob0 knob1 knob2)
  (send frame enable #f))
  
;(define ls (new slider% [label "left"]
;                [min-value 0]
;                [max-value 25]
;                [parent panel]))
;(define cs (new slider% [label "center"]
;                [min-value 0]
;                [max-value 25]
;                [parent panel]))
;(define rs (new slider% [label "right"]
;                [min-value 0]
;                [max-value 25]
;                [parent panel]))
