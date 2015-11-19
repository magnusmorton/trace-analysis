#lang racket/base

(require racket/flonum)
(require (only-in racket/list argmin first shuffle take))
(require (only-in racket/sequence sequence->list))


(define (list-chunk lst n)
  (if (< (length lst) n)
      lst
      (let-values ([(first-chunk remaining) (split-at lst n) ])
        (cons first-chunk (list-chunk remaining n)))))

;; Equality of two flvectors
(define (flvector= u v)
  (and
    (= (flvector-length u) (flvector-length v))
    (for/and ([u_i (in-flvector u)] [v_i (in-flvector v)]) (fl= u_i v_i))))

;; Picks `k` distinct elements from list `data` at random, and pairs them
;; with an index; `k` must be less than `n`.
(define (random-choice data k)
  (define indices (take (shuffle (sequence->list (in-range (length data)))) k))
  (for/list ([i (in-range (length indices))] [idx (in-list indices)])
    (cons i (list-ref data idx))))

;; `a` and `b` are points (flvectors); returns the squared Euclidean distance
(define (squared-dist a b)
  (for/fold ([sum 0.0]) ([x (in-flvector a)] [y (in-flvector b)])
    (let ([z (fl- x y)]) (fl+ sum (fl* z z)))))

;; `centroids` is a list of pairs consisting of list index and center point,
;; `x` is a point; returns an index into `centroids`.
(define (nearest-centroid centroids x)
  (define ctr (argmin (lambda (ctr) (squared-dist (cdr ctr) x)) centroids))
  (values (car ctr) (cdr ctr)))

;; equality of centroids
(define (centroid= ctrs1 ctrs2)
  (and
    (= (length ctrs1) (length ctrs2))
    (for/and ([ctr1_i (in-list ctrs1)] [ctr2_i (in-list ctrs2)])
      (flvector= (cdr ctr1_i) (cdr ctr2_i)))))

(define (merge-hists . vs)
  (for/vector ([xs (in-parallel vs)])
    (apply + xs)))

;; `hist` is a mutable vector of integers and `i` index into `hist`;
;; returns nothing.
(define (update-hist! hist i)
  (define count_i (vector-ref hist i))
  (vector-set! hist i (add1 count_i)))

;; `maxsqdist` is a mutable flvector of the maximum of squared distances,
;; `x` is a point, `i` an index into `maxsqdist` and
;; `ctr_i` the i-th center point; returns nothing.
(define (update-maxsqdist! maxsqdist x i ctr_i)
  (define max_squared_dist_i (flvector-ref maxsqdist i))
  (flvector-set! maxsqdist i (flmax max_squared_dist_i (squared-dist ctr_i x))))

;; `minsqdist` is a mutable flvector of the minimum of squared distances,
;; `x` is a point, `i` an index into `minsqdist` and
;; `ctr_i` the i-th center point; returns nothing.
(define (update-minsqdist! minsqdist x i ctr_i)
  (define sqdist_x (squared-dist ctr_i x))
  (define min_squared_dist (flvector-ref minsqdist i))
  (flvector-set! minsqdist i
    (if (fl< min_squared_dist 0.0) sqdist_x (flmin min_squared_dist sqdist_x))))

;; `sqdists` is a mutable flvector of the sum of squared distances,
;; `x` is a point, `i` an index into `sqdists` and
;; `ctr_i` the i-th center point; returns nothing.
(define (update-sqdists! sqdists x i ctr_i)
  (define sum_of_squared_dists_i (flvector-ref sqdists i))
  (flvector-set! sqdists i (fl+ sum_of_squared_dists_i (squared-dist ctr_i x))))

(define (merge-sums . sums)
  (for/vector ([])))

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


(define (task-helper data k centroids)
  (define hist (for/vector #:length k ([i (in-range k)]) 0))
  (define sums (for/vector #:length k ([i (in-range k)])
                 (for/flvector #:length d ([j (in-range d)]) 0.0)))
  (for ([x (in-list data)])
    (define-values (i ctr_i) (nearest-centroid centroids x))
    (update-hist! hist i)
    (update-sums! sums x i))
  (values hist sums))

;; `data` is a list of points, `k` is a positive integer and `centroids` is
;; a list of length `k` of pairs consisting of list index and center point.
;; The function returns:
;; * a new list of centroids,
;; * a vector of histograms,
;; * a vector of standard deviations from the center,
;; * a vector of maximum distances from the center,
;; * a vector of minimum distances from the center.
(define (k-means-step data k centroids chunk-size)
  (define d (flvector-length (cdr (first centroids))))
  ;; (define hist (for/vector #:length k ([i (in-range k)]) 0))
  ;; (define sums (for/vector #:length k ([i (in-range k)])
  ;;                (for/flvector #:length d ([j (in-range d)]) 0.0)))
  (define sqdists (for/flvector #:length k ([i (in-range k)]) 0.0))
  (define maxsqdist (for/flvector #:length k ([i (in-range k)]) -1.0))
  (define minsqdist (for/flvector #:length k ([i (in-range k)]) -1.0))
  ;; (for ([x (in-list data)])
  ;;   (define-values (i ctr_i) (nearest-centroid centroids x))
  ;;   (update-hist! hist i)
  ;;   (update-sums! sums x i)
  ;;   (update-sqdists! sqdists x i ctr_i)
  ;;   (update-maxsqdist! maxsqdist x i ctr_i)
  ;;   (update-minsqdist! minsqdist x i ctr_i))
  (define-values (hist sums) (task-helper data k centroids))
  (define new_centroids
    (for/list ([i (in-range k)] [c_i (in-vector hist)] [s_i (in-vector sums)])
      (if (zero? c_i)
        ;; no points near i-th centroid; keep it stable
        (list-ref centroids i)
        ;; else: compute new centroid
        (let ([ctr_i (for/flvector #:length d ([s_i_j (in-flvector s_i)])
                       (fl/ s_i_j (->fl c_i)))])
          (cons i ctr_i)))))
  (define stdev
    (for/flvector #:length k
      ([c_i (in-vector hist)] [sqd_i (in-flvector sqdists)])
      (if (zero? c_i)
        -1.0  ;; no points near i-th centroid; ret stdev -1.0 as error value
        (flsqrt (fl/ sqd_i (->fl c_i))))))
  (define maxdist
    (for/flvector #:length k ([maxsqdist_i (in-flvector maxsqdist)])
      (flsqrt maxsqdist_i)))
  (define mindist
    (for/flvector #:length k ([minsqdist_i (in-flvector minsqdist)])
      (flsqrt minsqdist_i)))
  (values new_centroids hist stdev maxdist mindist))

;; `data` is a list of points, `n_clusters` a pos integer and `term` an integer.
;; The function returns the centroids, histogram, standard deviations, maximum
;; and minimum distances of a k-means clustering of `data` with `n_clusters`,
;; starting from random centroids. It also returns the number of steps taken.
;; `term` determines the termination criterion:
;; * if `term` is nonnegative, clustering terminates after `term` steps exactly;
;; * if `term` is -2, clustering terminates when the histogram is stable;
;; * if `term` is -1 (default), clustering terminates when both histogram
;;   and centroids are stable.
(define (k-means data n_clusters [term -1])
  (define centroids0 (random-choice data n_clusters))
  (define k (length centroids0))
  (define hist0 (for/vector #:length k ([i (in-range k)]) 0))
  (define (loop centroids old_hist n)
    (define-values (new_centroids hist stdev maxdist mindist)
      (k-means-step data k centroids))
    (cond
      [(eq? term n)
         (values centroids hist stdev maxdist mindist n)]
      [(and (eq? term -2) (equal? hist old_hist))
         (values centroids hist stdev maxdist mindist n)]
      [(and (eq? term -1)
            (equal? hist old_hist)
            (centroid= new_centroids centroids))
         (values centroids hist stdev maxdist mindist n)]
      [else
         (loop new_centroids hist (add1 n))]))
  (loop centroids0 hist0 0))

;; Parse input `file`; returns list of points.
(define (parse-data file)
  (define file-in (open-input-file file))
  (define data
    (for/list ([line (in-lines file-in)])
      (define line-in (open-input-string line))
      (define point (for/flvector ([x (in-port read line-in)])
                      (if (exact-integer? x) (->fl x) x)))
      (close-input-port line-in)
      point))
  (close-input-port file-in)
  data)

;; main script
(define args (vector->list (current-command-line-arguments)))
(if (< (length args) 2)
  (printf "Usage: racket kmeans.rkt FILE K [TERM] [SEED]\n")
  (let ([file (list-ref args 0)]
        [k    (string->number (list-ref args 1))]
        [term (if (> (length args) 2) (string->number (list-ref args 2)) 0)]
        [seed (if (> (length args) 3) (string->number (list-ref args 3)) 0)])
    (when (> seed 0) (random-seed seed))
    (printf "Parsing ...\n")
    (define data (time (parse-data file)))
    (define n (length data))
    (define d (if (zero? n) 0 (flvector-length (first data))))
    (printf "N   = ~a\n" n)
    (printf "dim = ~a\n" d)
    (printf "Clustering ...\n")
    (define-values (centroids hist stdevs maxdist mindist steps)
      (time (k-means data k term)))
    (when (<= (* k d) 20) (printf "centroid = ~a\n" centroids))
    (printf "k        = ~a\n" (length centroids))
    (printf "count    = ~a\n" hist)
    (printf "std dev  = ~a\n" stdevs)
    (printf "max dist = ~a\n" maxdist)
    (printf "min dist = ~a\n" mindist)
    (printf "steps    = ~a\n" steps)))
