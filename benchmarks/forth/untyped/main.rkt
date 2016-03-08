#lang racket/base

(require (only-in "eval.rkt"
  forth-eval*
))

(define N
 (let ([args (current-command-line-arguments)])
   (if (< (vector-length args) 1) 1
     (let ([n (string->number (vector-ref args 0))])
       (if (fixnum? n) n
         (error 'main "must have a fixnum argument"))))))

;; =============================================================================

(define (main)
  (call-with-input-file* "../base/history.txt"
    (lambda (p)
      (let-values ([(_e _s) (forth-eval* p)]) (void))))
  (void))

(for ([i (in-range (sub1 N))]) (main))
(time (main))
