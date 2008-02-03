(library (bns utils random)
  (export random
          random-boolean-generator
          make-random-boolean-generator
          make-random-vector)
  (import (rnrs base)
          (rnrs arithmetic fixnums)
          (primitives random))
 
  ;;; Use max fixnum avaible to get better resolution.
  ;;; TODO: see in larceny 'random' library if there is a way to get negative
  ;;; random numbers, in order to use full avaible range.
  ;;; TODO: find/write a portable 'random' primitive
  (define *max* (greatest-fixnum))
  (define *max/2* (fxdiv *max* 2))

  ;;; Uniform distribution of #t and #f
  (define (random-boolean-generator)
    (cond
      [(fx<? (random *max*) *max/2*) #t]
      [else #f]))

  ;;; Biased distribution: bias is #t's probability [float between 0 and 1]
  (define (make-random-boolean-generator bias)
    (let [(treshold (exact (round (* bias *max*))))]
      (lambda ()
        (cond
          [(fx<? (random *max*) treshold) #t]
          [else #f]))))

  ;;; Return a vector of lenght n, filled by subsequent calls to rng.
  ;;; TODO: change name? (fng could be any thunk, so -random- is quite meanless)
  (define (make-random-vector n rng)
    (let ([v (make-vector n)])
      (let loop ([i n])
        (cond
          [(zero? i) v]
          [else (vector-set! v (- i 1) (rng)) (loop (- i 1))]))))

  )

