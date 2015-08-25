#lang racket/base


(require racket/flonum)
(require racket/vector)
(require racket/cmdline)
(define mod-id #"matmult")

(define chunk-size (make-parameter 0))
(define task? (make-parameter #f))


(command-line
 #:program "matmul"
 #:once-each
 [("-t" "--task") "task?"
  (task? #t)]
 [("-c" "--chunk-size") cs "chunk size"
  (chunk-size (string->number cs))])
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vector operations;
;; row and column vectors are flvectors of positive dimension.

(define (flvec-dim v)
  (flvector-length v))

;; Retrieve element j of vector v, 0 <= i < n, where n is the dimension of v
(define (flvec-elt v j)
  (flvector-ref v j))

;; Generate random vector of dim n
(define (flvec-random n)
  (define rg (current-pseudo-random-generator))
  (for/flvector #:length n ([j (in-range n)]) (flrandom rg)))

;; Sum of vector v
(define (flvec-sum v)
  (for/fold ([sum 0.0]) ([y v]) (fl+ sum y)))

;; Inner product of vectors u and v, both of dim n
(define (flvec-inner-* u v)
  (for/fold ([sum 0.0]) ([x u] [y v]) (fl+ sum (fl* x y))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Matrix operations;
;; matrices are stored in row-major order, i.e. a matrix of dimension m x n
;; consists of m row vectors of dimension n; m and n positive.

(define (flmat-rows A)
  (vector-length A))

;; Retrieve row i of m x n matrix A, 0 <= i < m; row i is a vector of dim n
(define (flmat-row A i)
  (vector-ref A i))

(define (flmat-cols A)
  (flvec-dim (flmat-row A 0)))

;; Retrieve col j of m x n matrix A, 0 <= j < n; col j is a vector of dim m
(define (flmat-col A j)
  (for/flvector #:length (flmat-rows A) ([row A]) (flvec-elt row j)))

;; Generate random matrix of dim m x n
(define (flmat-random m n)
  (for/vector #:length m ([i (in-range m)]) (flvec-random n)))

;; Sum of m x n matrix A
(define (flmat-sum A)
  (for/fold ([sum 0.0]) ([row A]) (fl+ sum (flvec-sum row))))

;; Transpose m x n matrix A
(define (flmat-transpose A)
  (define n (flmat-cols A))
  (for/vector #:length n ([j (in-range n)]) (flmat-col A j)))

;; Multiply m x k matrix A with transposed n x k matrix Bt
(define (flmat-*t A Bt)
  (define m (flmat-rows A))  ;; rows of result
  (define n (flmat-rows Bt)) ;; cols of result
  (for/vector #:length m ([row A])
    (for/flvector #:length n ([col Bt])
      (flvec-inner-* row col))))

;; Multiply m x k matrix A with k x n matrix B
(define (flmat-* A B)
  (define Bt (flmat-transpose B))
  (flmat-*t A Bt))

;; Row-cluster m x n matrix A into list of m x c matrices, 0 < c <= m
(define (flmat-cluster A c)
  (define m (flmat-rows A))
  (for/list ([i (in-range 0 m c)])
    (vector-copy A i (min (+ i c) m))))

;; Inverse of `flmat-cluster`
(define (flmat-uncluster As)
  (apply vector-append As))


(define (flmat-*cluster A B c)
  (define Bt (flmat-transpose B))
  (flmat-uncluster
   (for/list ([cluster (flmat-cluster A c)])
     (flmat-*t cluster Bt))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mat1 (flmat-random 1000 1000))
(define mat2 (flmat-random 1000 1000))

;; Warmup
;;
(define mat2t (flmat-transpose mat2))
(define cluster #())
;;(define cluster (vector-copy mat1 0 (chunk-size)))
(if (= (chunk-size) 0)
    (flmat-* mat1 mat2)
    (begin
      (set! cluster  (vector-copy mat1 0 (chunk-size)))
      (flmat-*cluster mat1 mat2 (chunk-size))))
    

;; task
;;(time 
(when (task?)
  (if (= (chunk-size) 0)
      (time (flmat-* mat1 mat2)) 
      (time (flmat-*t cluster mat2t))))

   
