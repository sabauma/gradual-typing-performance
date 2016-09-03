#lang racket

(require "aux.rkt" "world.rkt" "bset.rkt" "data.rkt")

(define N
  (let ([args (current-command-line-arguments)])
    (if (< (vector-length args) 1) 1
      (let ([n (string->number (vector-ref args 0))])
        (if (fixnum? n) n
          (error 'main "must have a fixnum argument"))))))

(define (world0)
  (world (list-pick-random tetras) empty))

(define (replay w0 hist)
  (for/fold ([w w0]) ([e hist])
    (match e
      [`(on-key ,ke) (world-key-move w ke)]
      [`(on-tick) (next-world w)]
      [`(stop-when)
       (λ (w) (blocks-overflow? (world-blocks w))) ;; Unused in original code https://github.com/philnguyen/soft-contract/blob/master/benchmark-contract-overhead/tetris.rkt#L959
       w]))
  (void))

(define SMALL_TEST "../base/tetris-hist-small.rktd")
(define LARGE_TEST "../base/tetris-hist-large.rktd")

(define (main filename)
  (define w0 (world0))
  (define raw (with-input-from-file filename read))
  (unless (list? raw) (error "bad input"))
  (replay w0 (reverse raw)))

;(time (main SMALL_TEST)) ; 0ms
(for ([i (in-range N)]) (time (main LARGE_TEST)))
