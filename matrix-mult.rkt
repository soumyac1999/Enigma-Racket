#lang racket
(require math/matrix)
(provide set-enigma-mode!)
(provide convert-char!)
(define NUMCHAR 26)
(define CHAR-OFFSET 65)
(define left-rotor 0)
(define middle-rotor 0)
(define right-rotor 0)
(define char-vectors (build-list NUMCHAR (lambda(x) (build-matrix NUMCHAR 1 (lambda(y _) (if (= x y) 1 0))))))
(define chars (build-list NUMCHAR (lambda(x) (integer->char (+ x CHAR-OFFSET)))))
(define key-to-vector (make-hash (map (lambda(x y) (cons x y)) chars char-vectors)))
(define vector-to-key (make-hash (map (lambda(x y) (cons x y)) char-vectors chars)))

(define IDENTITY (identity-matrix NUMCHAR))
(define RHO (build-matrix NUMCHAR NUMCHAR (lambda (x y)
                                            (if (= x 0)
                                                (if (= y 25) 1 0)
                                                (if (= y (- x 1)) 1 0)))))
(define RHO-INVERSE (matrix-inverse RHO))

;(define message chars)
(define (mult-matrix-to-char M ch) (hash-ref vector-to-key (matrix* M (hash-ref key-to-vector ch))))

(define U (build-matrix NUMCHAR NUMCHAR (lambda (x y)
                                          (if (= (modulo (+ y 13) NUMCHAR) x) 1 0))))
(define (genperm lst acc)
  (if (null? lst) acc
      (let ([x (list-ref lst (random (length lst)))])
        (genperm (remove x lst) (cons x acc)))))
(define (random-matrix)
  (let* ([rand-chars (genperm chars '())]
         [rand-lst (map (lambda (x) (hash-ref key-to-vector x)) rand-chars)])
    (displayln rand-chars)
    (build-matrix NUMCHAR NUMCHAR (lambda(i j) (matrix-ref (list-ref rand-lst j) i 0)))))

(random-seed 0)
(define default-L (random-matrix))
(define default-M (random-matrix))
(define default-R (random-matrix))

(define L default-L)
(define M default-M)
(define R default-R)
(define Li (matrix-inverse L))
(define Mi (matrix-inverse M))
(define Ri (matrix-inverse R))

(define (set-enigma-mode! mode)
  (if (equal? mode 'encrypt)
      (lambda()
        (let ([l (random NUMCHAR)]
              [m (random NUMCHAR)]
              [r (random NUMCHAR)])
          (begin
            (set! L (matrix* (matrix-expt RHO l) default-L (matrix-expt RHO-INVERSE l)))
            (set! M (matrix* (matrix-expt RHO m) default-M (matrix-expt RHO-INVERSE m)))
            (set! R (matrix* (matrix-expt RHO r) default-R (matrix-expt RHO-INVERSE r)))
            (set! Li (matrix-inverse L))
            (set! Mi (matrix-inverse M))
            (set! Ri (matrix-inverse R))
            (set! left-rotor l)
            (set! middle-rotor m)
            (set! right-rotor r)
            (list l m r))))
      (lambda(l m r)
        (begin
            (set! L (matrix* (matrix-expt RHO l) default-L (matrix-expt RHO-INVERSE l)))
            (set! M (matrix* (matrix-expt RHO m) default-M (matrix-expt RHO-INVERSE m)))
            (set! R (matrix* (matrix-expt RHO r) default-R (matrix-expt RHO-INVERSE r)))
            (set! Li (matrix-inverse L))
            (set! Mi (matrix-inverse M))
            (set! Ri (matrix-inverse R))
            (set! left-rotor l)
            (set! middle-rotor m)
            (set! right-rotor r)
            (list l m r)))))

(define (convert-char! ch) ; return list of new char and l,m,r
  (let* ([E (matrix* R M L U Li Mi Ri)]
         [new-char (mult-matrix-to-char E ch)])
    (begin
      (set! L (matrix* RHO L RHO-INVERSE))
      (set! M (matrix* RHO M RHO-INVERSE))
      (set! R (matrix* RHO R RHO-INVERSE))
      (set! Li (matrix* RHO Li RHO-INVERSE))
      (set! Mi (matrix* RHO Mi RHO-INVERSE))
      (set! Ri (matrix* RHO Ri RHO-INVERSE))
      (set! left-rotor (modulo (+ left-rotor 1) NUMCHAR))
      (cond [(zero? left-rotor) (begin
                                  (set! middle-rotor (modulo (+ 1 middle-rotor) NUMCHAR))
                                  (cond [(zero? middle-rotor) (set! right-rotor (modulo (+ 1 right-rotor) NUMCHAR))]))])
      (list new-char left-rotor middle-rotor right-rotor))))