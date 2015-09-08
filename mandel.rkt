#lang racket/base

(require racket/flonum)
(require racket/stream)
(require "bit-vector.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; complex numbers

;; (struct complex (real img) #:prefab)
;; COMMENTED out because of a bug in struct serialisation

(define complex-tag 'complex)
   
(define (complex real img)
  (vector-immutable complex-tag real img))

(define (complex? x)
  (and (vector? x) (> (vector-length x) 0) (eq? (vector-ref x 0) complex-tag)))

(define (complex-real z)
  (vector-ref z 1))

(define (complex-img z)
  (vector-ref z 2))

;; Validity predicate.
(define (complex/ok? x)
  (and
    (complex? x)
    (= (vector-length x) 3)
    (let ([real (complex-real x)] [img (complex-img x)])
      (and
        (flonum? real)
        (flonum? img)))))

(define-syntax-rule (complex+ x ...)
  (complex (fl+ (complex-real x) ...) (fl+ (complex-img x) ...)))

(define (complex- x)
  (complex (fl- 0.0 (complex-real x)) (fl- 0.0 (complex-img x))))

(define (complex* x y)
  (define rx (complex-real x))
  (define ix (complex-img  x))
  (define ry (complex-real y))
  (define iy (complex-img  y))
  (complex (fl- (fl* rx ry) (fl* ix iy)) (fl+ (fl* ix ry) (fl* rx iy))))

(define (complexmagnitude x)
  (define rx (complex-real x))
  (define ix (complex-img  x))
  (flsqrt (fl+ (fl* rx rx) (fl* ix ix))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mandelbrot computation (escape time algorithm)
;;
;; Computes the Mandelbrot set on the complex plane between -2 and 0.5
;; on the real axis and 1 and -1 on the imaginary axis.

;; Type: exact-nonnegative-integer?
;;    -> complex?
;;   [-> boolean?]
;;    -> exact-nonnegative-integer?
;; Returns 0 if `a` is in the Mandelbrot set, or a positive number < `max_time`.
;; Computes with complex numbers by default, or with real number instead.
(define (escape-time max_time a [use-C #t])
  (define ra (complex-real a))
  (define ia (complex-img  a))
  (define (go/complex t z)
    (cond [(fl>= (complexmagnitude z) 2.0) t]
          [(>= t max_time)                 0]
          [else (go/complex (add1 t) (complex+ (complex* z z) a))]))
  (define (go/real t rz iz)
    (cond [(fl>= (fl+ (fl* rz rz) (fl* iz iz)) 4.0) t]
          [(>= t max_time)                          0]
          [else (go/real (add1 t) (fl+ (fl- (fl* rz rz) (fl* iz iz)) ra)
                                  (fl+ (fl* 2.0 (fl* rz iz)) ia))]))
  (if use-C
      (go/complex 0 (complex 0.0 0.0))
      (go/real    0 0.0 0.0)))


;; Type: exact-nonnegative-integer?
;;    -> exact-positive-integer?
;;    -> exact-positive-integer?
;;    -> exact-nonnegative-integer?
;;   [-> boolean?]
;;    -> bit-vector?
;; Returns the bitvector indicating which points on the `y`-th row
;; (0 <= y < y_res) belong to the Mandelbrot set; `x_res` is the resultion
;; used to compute the row.
;; Computes with complex numbers by default, or with real number instead.
(define (mandelbrot-row max_time x_res y_res y [use-C #t])
  (define (real x) (fl+ -2.0 (fl* (->fl x) (fl/  2.5 (->fl (sub1 x_res))))))
  (define (img  y) (fl+  1.0 (fl* (->fl y) (fl/ -2.0 (->fl (sub1 y_res))))))
  (define img_y (img y))
  (define bv (make-bit-vector x_res #f))
  (for ([x (in-range x_res)])
    (define a (complex (real x) img_y))
    (define t (escape-time max_time a use-C))
    (when (= t 0) (bit-vector-set! bv x #t)))
  bv)


;; Type: exact-nonnegative-integer?
;;    -> exact-positive-integer?
;;    -> exact-positive-integer?
;;    -> exact-nonnegative-integer?
;;    -> exact-nonnegative-integer?
;;   [-> boolean?]
;;    -> vectorof bit-vector?
;; Returns a vector of bitvectors, each corresponding to one row of the
;; Mandelbrot set, from row `y_start` (inclusive) to row `y_end` (exclusive);
;; `x_res` is the number of points per row, `y_res` the total number of rows;
;; the inequality `0 <= y_start <= y_end <= y_res` must hold.
;; Computes with complex numbers by default, or with real number instead.
(define (mandelbrot-rows max_time x_res y_res y_start y_end [use-C #t])
  (define v (make-vector (- y_end y_start) #f))
  (for ([y (in-range y_start y_end)])
    (define bv (mandelbrot-row max_time x_res y_res y use-C))
    (vector-set! v (- y y_start) bv))
  v)


;; Type: exact-nonnegative-integer?
;;    -> exact-positive-integer?
;;    -> exact-positive-integer?
;;   [-> boolean?]
;;    -> vectorof bit-vector?
;; Returns a vector of bitvectors, one per row of Mandelbrot set;
;; `x_res` is the number of points per row, `y_res` the number of rows.
;; Computes with complex numbers by default, or with real number instead.
(define (mandelbrot max_time x_res y_res [use-C #t])
  (mandelbrot-rows max_time x_res y_res 0 y_res use-C))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; chunked parallel Mandelbrot computation (chunks of rows in parallel)

;; Like `mandelbrot` but takes extra `chunksize` parameter and computes
;; chunks of `chunksize` rows of the image in parallel.
;; Computes with complex numbers by default, or with real number instead.
;; (define (chunk-mandelbrot max_time x_res y_res chunksize [use-C #t])
;;   (define v (make-vector y_res #f))
;;   (define clo-f
;;     (closure chunk-mandelbrot/abs/stat use-C max_time x_res y_res chunksize))
;;   (for ([chunk (par-map clo-f (stream->list (in-range 0 y_res chunksize)))])
;;     (define y_chunk (car chunk))
;;     (define v_chunk (cdr chunk))
;;     (for ([i (in-range (vector-length v_chunk))])
;;       (vector-set! v (+ i y_chunk) (vector-ref v_chunk i))))
;;   v)

(define (chunk-mandelbrot/abs use-C max_time x_res y_res chunksize y_start)
  (define y_end (min (+ y_start chunksize) y_res))
  (define v (mandelbrot-rows max_time x_res y_res y_start y_end use-C))
  (cons y_start v))

(define (chunk_clo use-C max_time x_res y_res chunksize)
  (lambda (x)
    (chunk-mandelbrot/abs use-C max_time x_res y_res chunksize x)))

(define (mandelbrot-chunked max_time x_res y_res chunksize [use-C #t])
  (define v (make-vector y_res #f))
  (define clo-f (chunk_clo use-C max_time x_res y_res chunksize))
  (for ([chunk (map clo-f (stream->list (in-range 0 y_res chunksize)))])
    (define y_chunk (car chunk))
    (define v_chunk (cdr chunk))
    (for ([i (in-range (vector-length v_chunk))])
      (vector-set! v (+ i y_chunk) (vector-ref v_chunk i))))
  v)
