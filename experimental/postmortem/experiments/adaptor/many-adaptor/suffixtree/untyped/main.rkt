#lang racket/base

(require
 (only-in racket/file file->lines file->string))

(require "lcs.rkt")

(define LARGE_TEST "../base/prufock.txt")
(define SMALL_TEST "../base/hunt.txt")
(define KCFA_TYPED "../base/kcfa-typed.rkt")

;; LCS on all pairs of lines in a file
;(: main (-> String Void))
(define (main testfile)
  (define lines (file->lines testfile))
  (for* ([a lines] [b lines])
    (longest-common-substring a b))
  (void))

(time (main LARGE_TEST)) ; 110ms
;(require contract-profile)
;(contract-profile-thunk (lambda () (main LARGE_TEST))) ; 1900ms
;(time (main KCFA_TYPED)) ; 16235ms
