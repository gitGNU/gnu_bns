(library (bns utils prelude)
  (export ++ -- ->string)
  (import (rnrs base) (rnrs io ports))

  (define (++ n) (+ n 1))

  (define (-- n) (- n 1))

  (define-syntax ->string
    (syntax-rules ()
      [(_ x) (call-with-string-output-port (lambda (p) (display x p)))]))

  )
