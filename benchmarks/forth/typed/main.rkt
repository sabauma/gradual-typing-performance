#lang typed/racket/base

(require
  benchmark-util
  "../base/command-types.rkt")
(require/typed/check "eval.rkt"
  (forth-eval* (-> Input-Port (Values Any Any)))
)

(define N
 (let ([args (current-command-line-arguments)])
   (if (< (vector-length args) 1) 1
     (let ([n (string->number (vector-ref args 0))])
       (if (fixnum? n) n
         (error 'main "must have a fixnum argument"))))))

;; =============================================================================

(define (main)
  (call-with-input-file* (ann "../base/history.txt" Path-String)
    (lambda ([p : Input-Port])
      (let-values ([(_e _s) (forth-eval* p)]) (void))))
  (void))

(for ([i (in-range (sub1 N))]) (main))
(time (main))
