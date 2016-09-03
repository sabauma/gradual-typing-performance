#lang typed/racket/base

(require benchmark-util)

(require/typed/check "zo-shell.rkt"
  [init (-> (Vectorof String) Void)])

(define N
 (let ([args (current-command-line-arguments)])
   (if (< (vector-length args) 1) 1
     (let ([n (string->number (vector-ref args 0))])
       (if (fixnum? n) n
         (error 'main "must have a fixnum argument"))))))

;; Stress tests: search entire bytecode for the fairly-common branch struct
(define SELF-TEST '("../base/zo-shell.zo" "../base/zo-find.zo" "../base/zo-string.zo" "../base/zo-transition.zo"))
(define (self-test)
  (for ([b SELF-TEST]) (init (vector b "branch"))))

(define SMALL-TEST "../base/hello-world.zo")
(define (small-test)
  (init (vector SMALL-TEST "branch")))

(define LARGE-TEST "../base/streams.zo")
(define (large-test)
  (init (vector LARGE-TEST "branch")))

;; -----------------------------------------------------------------------------

(define-syntax-rule (main test)
  (with-output-to-file "/dev/null" test #:exists 'append))

(for ([i (in-range N)]) (time (main self-test)))
;(time (main small-test)) ;
;(time (main large-test)) ;
