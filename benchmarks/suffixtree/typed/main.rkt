#lang typed/racket/base

(define N
 (let ([args (current-command-line-arguments)])
   (if (< (vector-length args) 1) 1
     (let ([n (string->number (vector-ref args 0))])
       (if (fixnum? n) n
         (error 'main "must have a fixnum argument"))))))

(require benchmark-util
 (only-in racket/file file->lines file->string))

(require/typed/check "lcs.rkt"
  [longest-common-substring (-> String String String)])

(define LARGE_TEST "../base/prufock.txt")
(define SMALL_TEST "../base/hunt.txt")
(define KCFA_TYPED "../base/kcfa-typed.rkt")

;; LCS on all pairs of lines in a file
(: main (-> String Void))
(define (main testfile)
  (define lines (file->lines testfile))
  (for* ([a lines] [b lines])
    (longest-common-substring a b))
  (void))

;(time (main SMALL_TEST)) ; 110ms
(for ([i (in-range (sub1 N))]) (main LARGE_TEST))
(time (main LARGE_TEST))
;(time (main KCFA_TYPED)) ; 16235ms
