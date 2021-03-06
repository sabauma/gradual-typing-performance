#lang typed/racket/base

(provide block-move)

(require "data-block-adapted.rkt")

;; =============================================================================

(: block-move (-> Real Real Block Block))
(define (block-move dx dy b)
  (block (+ dx (block-x b))
         (+ dy (block-y b))
         (block-color b)))
