#lang racket/base

(require (only-in racket/list argmin drop first shuffle take))
(require (only-in racket/sequence sequence->list))
(require (only-in racket/flonum
           fl= fl< flmax flmin fl+ fl- fl* fl/ flsqrt ->fl
           flvector make-flvector flvector-length flvector-ref flvector-set!
           in-flvector for/flvector))
(require racket/cmdline)
(require "cost.rkt")
(define chunk-size (make-parameter 0))
(define task? (make-parameter #f))
(define filename (make-parameter ""))
(define k (make-parameter 0))

(command-line
 #:program "k-means"
 #:once-each
 [("-t" "--task") "task?"
  (task? #t)]
 [("-c" "--chunk-size") cs "chunk size"
  (chunk-size (string->number cs))]
 [("-f" "--file") fn "filename"
  (filename fn)]
 [("-k" "--kay") ks "k"
  (k (string->number ks ))])

(define my-chunk #f)


;; `sums` is a mutable vector of points, `x` is a point and
;; `i` an index into `sums`; returns nothing.
(define (update-sums! sums x i)
  (define sum_i (vector-ref sums i))
  (define d (flvector-length sum_i))
  ;; More efficient version: in-place update of sum point.
  (for ([j (in-range d)] [s_j (in-flvector sum_i)] [x_j (in-flvector x)])
    (flvector-set! sum_i j (fl+ s_j x_j))))
  ;; Less efficient version: construction of new sum point and vector update.
  ;;(vector-set! sums i
  ;;  (for/flvector #:length d ([s_j (in-flvector sum_i)] [x_j (in-flvector x)])
  ;;    (fl+ s_j x_j))))

;; `hist` is a mutable vector of integers and `i` index into `hist`;
;; returns nothing.
(define (update-hist! hist i)
  (define count_i (vector-ref hist i))
  (vector-set! hist i (add1 count_i)))

;; `sqdists` is a vector of mutable flvectors of length 3,
;; `i` an index into `sqdists` and `sqd_i_x` the squared
;; distance of some point `x` from the i-th center point; returns nothing;
;; flvectors in `sqdists` must be initialised to `#(-1.0 -1.0 -1.0)`.
(define (update-sqdists! sqdists sqd_i_x i)
  (define sqdists_i (vector-ref sqdists i))
  (let ([sum (flvector-ref sqdists_i 0)])
    (flvector-set! sqdists_i 0
      (if (fl< sum 0.0) sqd_i_x (fl+ sum sqd_i_x))))
  (let ([min (flvector-ref sqdists_i 1)])
    (flvector-set! sqdists_i 1
      (if (fl< min 0.0) sqd_i_x (flmin min sqd_i_x))))
  (let ([max (flvector-ref sqdists_i 2)])
    (flvector-set! sqdists_i 2
      (if (fl< max 0.0) sqd_i_x (flmax max sqd_i_x)))))


;; `a` and `b` are points (flvectors); returns the squared Euclidean distance
(define (squared-dist a b)
  (for/fold ([sum 0.0]) ([x (in-flvector a)] [y (in-flvector b)])
    (let ([z (fl- x y)]) (fl+ sum (fl* z z)))))

;; Extract relavant chunk from list `data` based and set `my-chunk`;
;; returns nothing.
(define (set-my-chunk! data workers)
  (if (zero? workers)
    ;; we are master and have no workers; all the data is ours
    (set! my-chunk data)
    ;; else: there are workers
    (if #t
      ;; we are a worker; extract chunk based on worker ID
      (let* ([N     (length data)]
             [n     workers]
             [pos   (lambda (i) (floor (* i (/ N n))))]
             [i     0]
             [start (pos i)]
             [end   (pos (add1 i))]
             [len   (- end start)]
             [chunk (take (drop data start) len)])
        (set! my-chunk chunk))
      ;; else: we are master and have workers; this path should not be called.
      (error "set-my-chunk!: PANIC!\n"))))

(define (nearest-centroid centroids x)
  (define ctr0 (first centroids))
  (define sqd0 (squared-dist ctr0 x))
  (define-values (i min-i min-ctr min-sqd)
    (for/fold ([i 0] [min-i 0] [min-ctr ctr0] [min-sqd sqd0])
              ([ctr_i (in-list centroids)])
      (define sqd_i_x (squared-dist ctr_i x))
      (if (fl< sqd_i_x min-sqd)
         (values (add1 i) i     ctr_i   sqd_i_x)
         (values (add1 i) min-i min-ctr min-sqd))))
  (values min-i min-ctr min-sqd))

(define (parse-data file)
  (or
    my-chunk
    (let* ([file-in (open-input-file file)]
           [data (for/list ([line (in-lines file-in)])
                   (define line-in (open-input-string line))
                   (define point (for/flvector ([x (in-port read line-in)])
                                   (if (exact-integer? x) (->fl x) x)))
                   (close-input-port line-in)
                   point)])
      (close-input-port file-in)
      data)))



(define (k-means-step/abs d k centroids)
  (define hist (for/vector #:length k ([i (in-range k)]) 0))
  (define sums (for/vector #:length k ([i (in-range k)])
                 (for/flvector #:length d ([j (in-range d)]) 0.0)))
  (define sqdists (for/vector #:length k ([i (in-range k)])
                    (for/flvector #:length 3 ([j (in-range 3)]) -1.0)))
  (for ([x (in-list my-chunk)])
    (define-values (i ctr_i sqd_i_x) (nearest-centroid centroids x))
    (update-hist! hist i)
    (update-sums! sums x i)
    (update-sqdists! sqdists sqd_i_x i))
  (vector hist sums sqdists))

(define (random-choice data k)
  (take (shuffle data) k))




;;warmup

(define data (parse-data (filename)))
(set-my-chunk! data  (chunk-size))

(define centroids0 (random-choice data (k)))
(k-means-step/abs (flvector-length (first data)) (k) centroids0 )
(k-means-step/abs (flvector-length (first data)) (k) centroids0 )
(snapshot-counts)
(when (task?)
  (time (k-means-step/abs (flvector-length (first data)) (k) centroids0)))


(printf "COST: ~a~n" (task-cost))
