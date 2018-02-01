#lang racket/base

(require racket/list)
(require racket/cmdline)
(require "cost.rkt")

(define fib-num (make-parameter 0))
(define task? (make-parameter #f))
(define threshold (make-parameter 0))

(command-line
 #:program "matmul"
 #:once-each
 [("-t" "--task") "task?"
  (task? #t)]
 [("-o" "--threshold") th "threshold"
  (threshold (string->number th))]
 [("-n" "--number") n "number"
  (fib-num (string->number n))])
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sequential Fibonacci (naively recursive)

(define (fib n)
  (if (<= n 1) 1 (+ (fib (- n 1)) (fib (- n 2)))))

(define (fib/abs n)
  (fib n))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parallel Fibonacci (divide-and-conquer with sequential threshold)

(define (dnc-fib thresh n)
  (if (<= n thresh)
    ;; below threshold: compute sequentially
    (fib n)
    ;; else gen parallelism: spawn task for 1st rec call, then comp 2nd call
    (+
      (dnc-fib thresh (- n 1))
      (dnc-fib thresh (- n 2)))))



;;Warmup
(if (= (threshold) 0)
    (fib (fib-num))
    (dnc-fib (threshold) (fib-num)))

(snapshot-counts)

;;Task
(when (task?)
  (if (= (threshold) 0)
      (time (fib (fib-num)))
      (time (fib (threshold) ))))


(printf "COST: ~a~n" (task-cost))
