#lang racket

;;(define WORKERS (string->number (vector-ref (current-command-line-arguments) 0 )))
(define (random-vector size)
  (for/vector ([i size])
    (random 10)))

(define (make-mat rows cols)
  (for/vector ([i cols])
    (random-vector rows)))

(define (matmul m1 m2)
  (let ([transpose (apply vector-map vector (vector->list m2))]) 
    (for/vector ([r m1])
      (for/vector ([c transpose])
        (for/fold ([sum 0])
                  ([i (vector-map * r c)])
          (+ sum i))))))

(define (matmul-chunked m1 m2 n)
  (let ([chunk (/ (vector-length m1) n)])
    (apply vector-append
           (for/list ([worker n])
             (matmul (vector-copy m1 (* worker chunk) (+ (* worker chunk) chunk)) m2)))))


(define mat1 (make-mat 1000 1000))
(define mat2 (make-mat 1000 1000))

;;(time (matmul-chunked (make-mat 1000 100) (make-mat 100 1000) WORKERS))

;; warmup
;;(matmul mat1 mat2)


;;actual
(time (matmul mat1 mat2))

;; actual chunked
;;(matmul-chunked mat1 mat2 10)
