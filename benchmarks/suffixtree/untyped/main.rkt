#lang racket/base

(define N
 (let ([args (current-command-line-arguments)])
   (if (< (vector-length args) 1) 1
     (let ([n (string->number (vector-ref args 0))])
       (if (fixnum? n) n
         (error 'main "must have a fixnum argument"))))))

(require benchmark-util
         (only-in racket/file file->lines file->string))

(require "lcs.rkt")

(define LARGE_TEST "../base/prufock.txt")
(define SMALL_TEST "../base/hunt.txt")
(define KCFA_TYPED "../base/kcfa-typed.rkt")

;; LCS on all pairs of lines in a file
(define (main testfile)
  (define lines (file->lines testfile))
  (for* ([a lines] [b lines])
    (longest-common-substring a b))
  (void))

;(time (main SMALL_TEST)) ;150ms
(for ([i (in-range N)]) (time (main LARGE_TEST)))
;(time (main KCFA_TYPED)) ; 22567ms
