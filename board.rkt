#lang racket

(require racket/gui)
(require "matrix-mult.rkt")
(require "knobs.rkt")
(require "control.rkt")
(require "turing.rkt")
(require "list-comprehension.rkt")


(define (update-circles-board t1 t2 t3)
  (update-circles t1 t2 t3 knob0 knob1 knob2))

(define state 'none)
(define rotors (list 0 0 0))
(define disallow
  (list #\page #\tab #\vtab #\backspace))
(define done-keys
  (list  #\linefeed #\newline #\return))
(define t1 0)
(define t2 0)
(define t3 0)
(define key-so-far 0)
(define msg #f)
(define (zip l1 l2)
  (map cons l1 l2))
(define (c->i char)
  (- (char->integer char) (char->integer #\A)))
(define (i->c int)
  (integer->char (+ int (char->integer #\A))))

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
    (define r1 (build-vector
                10
                (λ (x) (make-object bitmap% (~a "letters/" (list-ref letters x) ".jpg")))))
    (define r2 (build-vector
                9
                (λ (x) (make-object bitmap% (~a "letters/" (list-ref letters (+ x 10)) ".jpg")))))
    (define r3 (build-vector
                7
                (λ (x) (make-object bitmap% (~a "letters/" (list-ref letters (+ x 19)) ".jpg")))))

    (define imgs (vector r1 r2 r3))
    (define (2d-vec-ref vec i j) (vector-ref (vector-ref vec i) j))
    (define my-map (make-hash (zip letters (build-list 26 (lambda (i) (cond [(< i 19)
                                                                             (cons (quotient i 10) (modulo i 10))]
                                                                            [else (cons 2 (modulo (+ i 1) 10))]))))))

    (define/public (get-mesg)
      mesg)
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
      (cond [(equal? state 'waiting-for-key)
          (cond
            [(equal? key-value 'release)
             'done]
            [(and (= 1 (length listed))
                  (char-alphabetic? (car listed)))
             (let* ([character (char-upcase (car listed))]
                    [pos (hash-ref my-map character)]
                    [newimg (~a "letters/" character ".jpg")])
               (cond [(= key-so-far 0) (set! t1 (c->i character))]
                     [(= key-so-far 1) (set! t2 (c->i character))]
                     [(= key-so-far 2) (set! t3 (c->i character))])
               (set! key-so-far (+ key-so-far 1))
               (set! mesg (string-append mesg (~a character)))
               (let* ([l (string-length mesg)]
                      [label (if (> l max-len)
                                 (string-append "..." (substring mesg (- l max-len) l))
                                 mesg)])
                 (send msg set-label label))
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
                     newimg)
               (if (= key-so-far 3)
                   (decrypt)
                   'ignore))]
            )]
          [(equal? state 'msg-crack)
           (cond
            [(equal? key-value 'release)
             'done]
            [(and (= 1 (length listed))
                  (char-alphabetic? (car listed)))         
             (let* ([character (char-upcase (car listed))]
                    [enc (cons character rotors)]
                    [pos (hash-ref my-map (car enc))]
                    [newimg (~a "letters/" (car enc) ".jpg")])
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
                     newimg))]
            [(and (= 1 (length listed)) (member (car listed) done-keys))
             (crack-prefix)]
            [(and (= 1 (length listed)) (not (member (car listed) disallow)))
             (let* ([character (char-upcase (car listed))]
                    [enc (cons character rotors)])
               (set! rotors (cdr enc))
               (set! mesg (string-append mesg (~a (car enc))))
               (let* ([l (string-length mesg)]
                      [label (if (> l max-len)
                                 (string-append "..." (substring mesg (- l max-len) l))
                                 mesg)])
                 (send msg set-label label))
               (match rotors
                 [(list i j k) (update-circles i j k knob0 knob1 knob2)])
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
                       (+ 75 (* i (/ w 10))) 150)))])]
          [(equal? state 'crack-prefix)
            (cond
            [(equal? key-value 'release)
             'done]
            [(and (= 1 (length listed))
                  (char-alphabetic? (car listed)))         
             (let* ([character (char-upcase (car listed))]
                    [enc (cons character rotors)]
                    [pos (hash-ref my-map (car enc))]
                    [newimg (~a "letters/" (car enc) ".jpg")])
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
                     newimg))]
            [(and (= 1 (length listed)) (member (car listed) done-keys))
             (crack-execute)]
            [(and (= 1 (length listed)) (not (member (car listed) disallow)))
             (let* ([character (char-upcase (car listed))]
                    [enc (cons character rotors)])
               (set! rotors (cdr enc))
               (set! mesg (string-append mesg (~a (car enc))))
               (let* ([l (string-length mesg)]
                      [label (if (> l max-len)
                                 (string-append "..." (substring mesg (- l max-len) l))
                                 mesg)])
                 (send msg set-label label))
               (match rotors
                 [(list i j k) (update-circles i j k knob0 knob1 knob2)])
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
                       (+ 75 (* i (/ w 10))) 150)))])]
          [else (cond
            [(equal? key-value 'release)
             'done]
            [(and (= 1 (length listed))
                  (char-alphabetic? (car listed)))         
             (let* ([character (char-upcase (car listed))]
                    [enc (convert-char! character)]
                    [pos (hash-ref my-map (car enc))]
                    [newimg (~a "letters/" (car enc) ".jpg")])
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
                     newimg))]
            [(and (= 1 (length listed)) (member (car listed) done-keys))
             (done)]
            [(and (= 1 (length listed)) (not (member (car listed) disallow)))
             (let* ([character (char-upcase (car listed))]
                    [enc (convert-char! character)])
               (set! rotors (cdr enc))
               (set! mesg (string-append mesg (~a (car enc))))
               (let* ([l (string-length mesg)]
                      [label (if (> l max-len)
                                 (string-append "..." (substring mesg (- l max-len) l))
                                 mesg)])
                 (send msg set-label label))
               (match rotors
                 [(list i j k) (update-circles i j k knob0 knob1 knob2)])
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
                       (+ 75 (* i (/ w 10))) 150)))])]))))

(define (encrypt)
  (set! state 'encrypt)
  (send board enable #t)
  (send control enable #f)
  (set! seed (random 2147483647))
  (set-seed! seed)
  (set! rotors ((set-enigma-mode! 'encrypt)))
  (displayln seed)
  (displayln (map i->c rotors))
  (match rotors
    [(list i j k) (update-circles i j k knob0 knob1 knob2)]))

(define (decrypt-wait)
  (send control enable #f)
  (set! state 'waiting-for-key)
  (set! seed #f)
  (send popup show #t)
  (set! t1 0)
  (set! t2 0)
  (set! t3 0)
  (set! key-so-far 0)
  (send board enable #t))

(define (decrypt)
  (set! state 'decrypt)
  (send board clear-mesg)
  (set! key-so-far 0)
  (set-seed! seed)
  (set! rotors ((set-enigma-mode! 'decrypt) t1 t2 t3))
  (match rotors
    [(list i j k) (update-circles i j k knob0 knob1 knob2)]))

(define (done)
  (set! state 'none)
  (displayln (send board get-mesg))
  (send board clear-mesg)
  (set! rotors '(0 0 0))
  (set! t1 0)
  (set! t2 0)
  (set! t3 0)
  (set! key-so-far 0)
  (set! seed #f)
  (set! msg #f)
  (send control enable #t)
  (update-circles 0 0 0 knob0 knob1 knob2)
  (send board enable #f))

(define (crack)
  (set! state 'msg-crack)
  (send control enable #f)
  (set! seed #f)
  (set! t1 0)
  (set! t2 0)
  (set! t3 0)
  (send board enable #t)
  ;Get message
  ;Get prefix
  )
(define (crack-prefix)
  (set! msg (send board get-mesg))
  (send board clear-mesg)
  (set! state 'crack-prefix))

(define (crack-execute)
  (define prefix  (send board get-mesg))
  (send board clear-mesg)
  (send board enable #f)
  (decrypt-message msg prefix)
  (done))
  
(define (handle-seed-input t e)
  (cond [(equal? (send e get-event-type) 'text-field-enter)
         (define text (send (send t get-editor) get-text))
         (send (send t get-editor) erase)
         (define num (string->number text))
         (if num
             (begin 
               (set! seed num)
               (send err-msg set-label "")
               (send popup show #f))
             (begin
               (send err-msg set-label "Number!")
               ))]))
(define dialog-custom%
  (class dialog%
    (super-new)
    (define/augment (on-close)
      (send popup show #t))))
(define popup (new dialog-custom%
                   [parent #f]
                   [label "Seed"]
                   [width 200]
                   [height 200]
                   [style (list 'close-button)]))

(define tf (new text-field% [parent popup]
                [label "Enter seed"]
                [callback handle-seed-input]))
(define err-msg (new message% [parent popup]
                     [label (list->string (build-list 22 (lambda (x) #\space)))]))



(define frame (new frame%
                   [label "Enigma"]
                   [width 750]
                   [height 490]
                   [x 0]
                   [y 0]
                   ;[style (list 'no-caption 'no-resize-border)]
                   [border 1]))
(define panel1 (new horizontal-panel%
                    [parent frame]))
(define panel (new horizontal-panel%
                   [parent panel1]
                   [min-width 600]))

(define knob0 (new knob%
                   [parent panel]
                   [i 0]
                   [style (list 'border)]))
(define knob1 (new knob%
                   [parent panel]
                   [i 1]
                   [style (list 'border)]))
(define knob2 (new knob%
                   [parent panel]
                   [i 2]
                   [style (list 'border)]))

(define control (new button-canvas%
                     [parent panel1]
                     [encrypt encrypt]
                     [decrypt decrypt-wait]
                     [crack crack]))

(define board (new enigma-canvas%
                   [parent frame]))

(define (decrypt-attempt known-prefix e-m l m r)
  (let ([ans (prefix-matcher (string->list known-prefix) (string->list e-m))])
    (begin
      (cond [ans (display-message e-m l m r)])
      (list ans l m r))))

(define (decrypt-message encrypted-message known-prefix)
  (send popup show #t)
  (set-seed! seed)
  (let ([L (build-list 26 (lambda(x) x))]
        [M (build-list 26 (lambda(x) x))]
        [R (build-list 26 (lambda(x) x))])
    (filter (lambda (l) (car l))
            (lc (begin
                  ((set-enigma-mode! 'decrypt) l m r)
                  (decrypt-attempt known-prefix encrypted-message l m r)) : l <- L m <- M r <- R))))

(define (display-message e-m l m r)
  (begin
    ((set-enigma-mode! 'decrypt) l m r)
    (displayln (list->string (map convert-any-char (string->list e-m))))))

;(define (decrypt-message encrypted-message known-prefix)
;  (define rotors (map cdr (the-truth known-prefix encrypted-message)))
;  (cond [(null? rotors) (displayln "failed")]
;        [else (for-each (lambda(l) (apply (lambda(a b c) (let ([ans ]) l)) rotors)]))


(define seed #f)
(send frame show #t)
(send board enable #f)
