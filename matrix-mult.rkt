#lang racket
(require math/matrix)
(provide all-defined-out)
(define NUMCHAR 26)
(define CHAR-OFFSET 65)
(define char-vectors (build-list NUMCHAR (lambda(x) (build-matrix NUMCHAR 1 (lambda(y _) (if (= x y) 1 0))))))
(define chars (build-list NUMCHAR (lambda(x) (string->symbol (list->string (list (integer->char (+ x CHAR-OFFSET))))))))
(define key-to-vector (make-hash (map (lambda(x y) (cons x y)) chars char-vectors)))
(define vector-to-key (make-hash (map (lambda(x y) (cons x y)) char-vectors chars)))

(define IDENTITY (identity-matrix NUMCHAR))
(define RHO (build-matrix NUMCHAR NUMCHAR (lambda (x y)
                                            (if (= x 0)
                                                (if (= y 25) 1 0)
                                                (if (= y (- x 1)) 1 0)))))

(define (mult-rho ch) (hash-ref vector-to-key (matrix* RHO (hash-ref key-to-vector ch))))
