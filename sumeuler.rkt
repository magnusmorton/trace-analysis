#lang racket/base


(require racket/cmdline)

(define chunk-size (make-parameter 0))
(define task? (make-parameter #f))
(define stride? (make-parameter #f))
(define len (make-parameter 4000))

(command-line
 #:program "sumeuler"
 #:once-each
 [("-t" "--task") "task?"
  (task? #t)]
 [("-c" "--chunk-size") cs "chunk size"
  (chunk-size (string->number cs))]
 [("-s" "--stride") "stride?"
  (stride? #t)]
 [("-l" "--length") ls "length"
  (len (string->number ls))]
 [("-s" "--start-point") sp "start point"
  (start-point (string->number sp))])
 
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


(define start (+ (quotient (* (start-point)) 10) lower))

;; warmup
(define lower 1)
(define max-chunk 8000)
;; (if  (= (chunk-size) 0)
;;      (sum-totient lower (+ lower (len)))
;;      (if (stride?)
;;          (stride-sum-totient lower (len) (chunk-size))
;;          (chunk-sum-totient lower (len) (chunk-size))))

(sum-totient lower (+ lower (len)))

;; task
(when (task?)
  (if (= (chunk-size) 0)
      (time (sum-totient lower (+ lower (len))))
      (if (stride?)
          (time (sum-totient start (+ start (len)) (chunk-size)))
          (time (sum-totient start (+ start (chunk-size)))))))
     
