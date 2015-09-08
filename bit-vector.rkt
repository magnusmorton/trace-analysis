#lang racket/base
;;
;; Simplified implementation of mutable bitvectors.

(provide
  make-bit-vector
  (rename-out [bit-vector* bit-vector])
  bit-vector?
  bit-vector/ok?
  bit-vector-ref
  bit-vector-set!
  bit-vector-length
  (rename-out [bit-vector-copy* bit-vector-copy])
  bit-vector->list
  list->bit-vector)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; representation of bitvectors

;; (struct bit-vector (words size))
;; COMMENTED out because of a bug in struct serialisation

(define bv-tag 'bv)
   
(define (bit-vector words size)
  (vector-immutable bv-tag words size))

(define (bit-vector? x)
  (and (vector? x) (> (vector-length x) 0) (eq? (vector-ref x 0) bv-tag)))

(define (bit-vector-words bv)
  (vector-ref bv 1))

(define (bit-vector-size bv)
  (vector-ref bv 2))

;; Validity predicate.
(define (bit-vector/ok? x)
  (and
    (bit-vector? x)
    (= (vector-length x) 3)
    (let ([words (bit-vector-words x)] [size (bit-vector-size x)])
      (and
        (bytes? words)
        (exact-nonnegative-integer? size)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; operations on bitvectors

(define bits-in-a-word 8)

(define largest-word
  (- (expt 2 bits-in-a-word) 1))

(define (make-bit-vector size [fill #f])
  (define-values (q r) (quotient/remainder size bits-in-a-word))
  (define word-size (+ q (if (zero? r) 0 1)))
  (define words (make-bytes word-size (if fill largest-word 0)))  
  (when (and fill (not (zero? r))) 
    (bytes-set! words q (- (expt 2 r) 1)))
  (bit-vector words size))

(define bit-vector*
  (let ([bit-vector
         (lambda init-bits
           (list->bit-vector init-bits))])
    bit-vector))

(define not-given (gensym))

(define (bit-vector-ref bv n [default not-given])
  (unless (exact-nonnegative-integer? n)
    (raise-argument-error 'bit-vector-ref "exact-nonnegative-integer?" n))
  (cond [(< n (bit-vector-size bv))
         (unsafe-bit-vector-ref bv n)]
        [else
         (cond [(eq? default not-given)
                (raise-range-error 'bit-vector-ref
                                   "bit-vector"
                                   "" n bv 0 (sub1 (bit-vector-size bv)))]
               [(procedure? default)
                (default)]
               [else default])]))

(define (unsafe-bit-vector-ref bv n)
  (define-values (wi bi) (quotient/remainder n bits-in-a-word))
  ;(unless (bit-vector? bv) (error "unsafe-bit-vector-ref"))
  (define words (bit-vector-words bv))
  (define word  (bytes-ref words wi))
  (define bit   (bitwise-bit-set? word bi))
  bit)

(define (bit-vector-iterate-first bv)
  (if (zero? (bit-vector-size bv)) #f 0))

(define (bit-vector-iterate-next bv pos)
  (if (>= (+ pos 1) (bit-vector-size bv))
      #f
      (+ pos 1)))

(define (bit-vector-iterate-key bv key)
  key)

(define (bit-vector-iterate-value bv key)
  (bit-vector-ref bv key))

(define (bit-vector-set! bv n b)
  (define-values (wi bi) (quotient/remainder n bits-in-a-word))
  ;(unless (bit-vector? bv) (error "bit-vector-set!"))
  (define words (bit-vector-words bv))
  (define word  (bytes-ref words wi))
  (define bit   (bitwise-bit-set? word bi))
  (unless (eq? bit b)
    (bytes-set! words wi (bitwise-xor word (expt 2 bi)))))

(define (bit-vector-length bv)
  (bit-vector-size bv))

(define bit-vector-copy*
  (let ([bit-vector-copy
         (case-lambda
          [(bv)
           (bit-vector (subbytes (bit-vector-words bv) 0)
                       (bit-vector-size bv))]
          [(bv start)
           (bit-vector-copy-iter* bv start (bit-vector-size bv))]
          [(bv start end)
           (bit-vector-copy-iter* bv start end)])])
    bit-vector-copy))

(define (bit-vector-copy-iter* bv start end)
  (define the-start (max start 0))
  (define the-end   (min end (bit-vector-size bv)))
  (define copy-size (max (- the-end the-start) 0))
  (define copy (make-bit-vector copy-size))
  (for ([n (in-range copy-size)])
    (bit-vector-set! copy n (unsafe-bit-vector-ref bv (+ n the-start))))
  copy)

(define (bit-vector->list bv)
  (define len (bit-vector-size bv))
  (let loop ([i 0])
    (cond [(< i len)
           (cons (unsafe-bit-vector-ref bv i)
                 (loop (add1 i)))]
          [else null])))

(define (list->bit-vector init-bits)
  (define len (length init-bits))
  (define bv (make-bit-vector len))
  (for ([i (in-range len)]
        [b (in-list init-bits)])
    (bit-vector-set! bv i b))
  bv)
