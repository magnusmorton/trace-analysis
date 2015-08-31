#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sum of Euler's totient function

(define (totient n)
  (for/sum ([k (in-range n)]) (if (= (gcd n k) 1) 1 0)))

;; Sum of `totient` over an interval integers from `lower` (inclusive)
;; to  `upper` (exclusive), optionally in strides of width `stride`.
(define (sum-totient lower upper [stride 1])
  (for/sum ([k (in-range lower upper stride)]) (totient k)))


(define (chunk-sum-totient lower len chunksize)
   (let* ([upper (+ lower len)]
          [intervals (for/list ([lb (in-range lower upper chunksize)])
                      (cons lb (min chunksize (- upper lb))))]
          [partial-sums (for/list ([in intervals])
                          (sum-totient (car in) (+ (car in) (cdr in))))])
    (for/sum ([s partial-sums]) s)))

(define (stride-sum-totient lower len stride)
  (define upper (+ lower len))
  (define lower-bounds (build-list stride (lambda (i) (+ lower i))))
  (define partial-sums (for/list ([lower lower-bounds])
                         (sum-totient lower upper stride)))
  (for/sum ([s partial-sums]) s))
