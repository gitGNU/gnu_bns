(library (bns nk-net rule)
  (export make-rule
          make-rule-random
          rule?
          rule-apply
          rule-write)
  (import (rnrs base)
          (err5rs records procedural)
          (rnrs arithmetic fixnums)
          (rnrs io simple)
          (bns utils random))

  (define rule-rtd (make-rtd 'rule-rtd '#((immutable n-inputs)
                                          (immutable fn))))

  (define new-rule (rtd-constructor rule-rtd))

  ;;; Make a rule from a vector, which is rule's truth-table
  (define (make-rule v)
    (new-rule (fxlength (fx- (vector-length v) 1)) v))

  ;;; Make a random rule that accepts n inputs, bias is #t's probability.
  (define (make-rule-random n . bias)
    (let ([bias (if (null? bias) 0.5 (car bias))])
      (new-rule n (make-random-vector
                    (fxarithmetic-shift-left 1 n)
                    (make-random-boolean-generator bias)))))

  (define rule? (rtd-predicate rule-rtd))

  (define rule-fn (rtd-accessor rule-rtd 'fn))

  ;;; Apply rule: binary representation of `i` is inputs values.
  (define (rule-apply r i) (vector-ref (rule-fn r) i))

  ;;; Write rule r on port p
  (define (rule-write r p) (write (rule-fn r) p))

  )

