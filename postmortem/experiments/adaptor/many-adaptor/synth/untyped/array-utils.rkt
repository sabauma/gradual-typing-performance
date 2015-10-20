#lang racket/base

(require 
         (only-in racket/performance-hint begin-encourage-inline)
         (for-syntax racket/base)
         (only-in racket/fixnum fx* fx+)
         )

(provide
array-shape-size
check-array-shape
check-array-shape-size
make-thread-local-indexes
next-indexes!
unsafe-array-index->value-index
unsafe-vector-insert
unsafe-vector-remove
vector-copy-all
)

(define index? exact-nonnegative-integer?)

(begin-encourage-inline
  
  ;(: vector->supertype-vector (All (A B) ((Vectorof A) -> (Vectorof (U A B)))))
  (define (vector->supertype-vector js)
    (define dims (vector-length js))
    (cond [(= dims 0)  (vector)]
          [else  (define new-js  (make-vector dims (vector-ref js 0)))
                 (let loop ([i 1])
                   (cond [(i . < . dims)  (vector-set! new-js i (vector-ref js i))
                                          (loop (+ i 1))]
                         [else  new-js]))]))
  
  ;(: vector-copy-all (All (A) ((Vectorof A) -> (Vectorof A))))
  (define (vector-copy-all js) (vector->supertype-vector js))
  
  ;(: array-shape-size (Indexes -> Integer))
  (define (array-shape-size ds)
    (define dims (vector-length ds))
    (let loop ([i 0] [n  1])
      (cond [(i . < . dims)  (define d (vector-ref ds i))
                             (loop (+ i 1) (* n d))]
            [else  n])))
  
  ;(: check-array-shape-size (Symbol Indexes -> Integer))
  (define (check-array-shape-size name ds)
    (define size (array-shape-size ds))
    (cond [(index? size)  size]
          [else  (error name "array size ~e (for shape ~e) is too large (is not an Index)" size ds)]))
  
  ;(: check-array-shape ((Vectorof Integer) (-> Nothing) -> Indexes))
  (define (check-array-shape ds fail)
    (define dims (vector-length ds))
    (define new-ds  (make-vector dims 0))
    (let loop ([i  0])
      (cond [(i . < . dims)
             (define di (vector-ref ds i))
             (cond [(index? di)  (vector-set! new-ds i di)
                                 (loop (+ i 1))]
                   [else  (fail)])]
            [else  new-ds])))
  
  ;(: unsafe-array-index->value-index (Indexes Indexes -> Integer))
  (define (unsafe-array-index->value-index ds js)
    (define dims (vector-length ds))
    (let loop ([i  0] [j  0])
      (cond [(i . < . dims)
             (define di (vector-ref ds i))
             (define ji (vector-ref js i))
             (loop (+ i 1) (fx+ ji (fx* di j)))]
            [else  j])))
  
  )  ; begin-encourage-inline

;(: raise-array-index-error (Symbol Indexes In-Indexes -> Nothing))
(define (raise-array-index-error name ds js)
  (error name "expected indexes for shape ~e; given ~e"
         (vector->list ds) js))

;(: array-index->value-index (Symbol Indexes In-Indexes -> Integer))
(define (array-index->value-index name ds js)
  (define (raise-index-error) (raise-array-index-error name ds js))
  (define dims (vector-length ds))
  (unless (= dims (vector-length js)) (raise-index-error))
  (let loop ([i 0] [j   0])
    (cond [(i . < . dims)
           (define di (vector-ref ds i))
           (define ji (vector-ref js i))
           (cond [(and (exact-integer? ji) (0 . <= . ji) (ji . < . di))
                  (loop (+ i 1) (fx+ ji (fx* di j)))]
                 [else  (raise-index-error)])]
          [else  j])))

;(: check-array-indexes (Symbol Indexes In-Indexes -> Indexes))
(define (check-array-indexes name ds js)
  (define (raise-index-error) (raise-array-index-error name ds js))
  (define dims (vector-length ds))
  (unless (= dims (vector-length js)) (raise-index-error))
  (define new-js  (make-vector dims 0))
  (let loop ([i 0])
    (cond [(i . < . dims)
           (define di (vector-ref ds i))
           (define ji (vector-ref js i))
           (cond [(and (exact-integer? ji) (0 . <= . ji) (ji . < . di))
                  (vector-set! new-js i ji)
                  (loop (+ i 1))]
                 [else  (raise-index-error)])]
          [else  new-js])))

;(: unsafe-vector-remove (All (I) ((Vectorof I) Integer -> (Vectorof I))))
(define (unsafe-vector-remove vec k)
  (define n (vector-length vec))
  (define n-1 (sub1 n))
  (cond
    [(not (index? n-1)) (error 'unsafe-vector-remove "internal error")]
    [else
     (define new-vec  (make-vector n-1 (vector-ref vec 0)))
     (let loop ([i  0])
       (when (i . < . k)
         (vector-set! new-vec i (vector-ref vec i))
         (loop (+ i 1))))
     (let loop ([i  k])
       (cond [(i . < . n-1)
              (vector-set! new-vec i (vector-ref vec (+ i 1)))
              (loop (+ i 1))]
             [else  new-vec]))]))

;(: unsafe-vector-insert (All (I) ((Vectorof I) Integer I -> (Vectorof I))))
(define (unsafe-vector-insert vec k v)
  (define n (vector-length vec))
  (define dst-vec  (make-vector (+ n 1) v))
  (let loop ([i  0])
    (when (i . < . k)
      (vector-set! dst-vec i (vector-ref vec i))
      (loop (+ i 1))))
  (let loop ([i  k])
    (when (i . < . n)
      (let ([i+1  (+ i 1)])
        (vector-set! dst-vec i+1 (vector-ref vec i))
        (loop i+1))))
  dst-vec)

;(: make-thread-local-indexes (Integer -> (-> Indexes)))
(define (make-thread-local-indexes dims)
  (let ([val  (make-thread-cell #f)])
    (λ () (or (thread-cell-ref val)
              (let ([v   (make-vector dims 0)])
                (thread-cell-set! val v)
                v)))))

;(: next-indexes! (Indexes Integer Indexes -> Void))
;; Sets js to the next vector of indexes, in row-major order
(define (next-indexes! ds dims js)
  (let loop ([k  dims])
    (unless (zero? k)
      (let ([k  (- k 1)])
        (define jk (vector-ref js k))
        (define dk (vector-ref ds k))
        (let ([jk  (+ jk 1)])
          (cond [(jk . >= . dk)
                 (vector-set! js k 0)
                 (loop k)]
                [else
                 (vector-set! js k jk)]))))))