#lang typed/racket

(require typed/racket/class
         "require-typed-check.rkt" ; benchmark infrastructure
         "typed-base.rkt"
         (prefix-in mred: typed/racket/gui))

(require/typed/check (prefix-in card-class: "card-class.rkt")
                     [(card% card-class:card%) Card%])

(provide back deck-of-cards make-card) ; contract & contract helper for make-card removed

;; partial inline of list-hash.rkt here

(: make-hash (All (a b) (-> (Hash a b))))
(define (make-hash)
  (box empty))

;; -----------------------------------
  
(: get-bitmap ((U Path-String Input-Port) -> (Instance mred:Bitmap%)))
(define (get-bitmap file)
  (mred:read-bitmap file
                    #:try-@2x? ((assert (mred:get-display-backing-scale)) . > . 1)))

(: make-dim ((Instance mred:Bitmap%) -> (Instance mred:Bitmap%)))
(define (make-dim bm-in)
  (let ([w (send bm-in get-width)]
        [h (send bm-in get-height)]
        [s (exact-round (send bm-in get-backing-scale))])
    (let* ([bm (mred:make-bitmap w h #:backing-scale s)]
           [mdc (make-object mred:bitmap-dc% bm)])
      (send mdc draw-bitmap bm-in 0 0)
      (let* ([len (* w h 4 s s)]
             [b (make-bytes len)])
        (send bm get-argb-pixels 0 0 (* w s) (* h s) b #:unscaled? #t)
        (let loop ([i 0])
          (unless (= i len)
            (when (positive? (modulo i 4))
              (bytes-set! b i (quotient (* 3 (bytes-ref b i)) 4)))
            (loop (add1 i))))
        (send bm set-argb-pixels 0 0 (* w s) (* h s) b #:unscaled? #t))
      (send mdc set-bitmap #f)
      bm)))

(: here (Path-String -> Path))
(define here
  (let ([cp (collection-path "games" "cards")])
    (lambda (file)
      (build-path cp
                  (if ((mred:get-display-depth)  . <= . 8)
                      "locolor"
                      "hicolor")
                  file))))

(define: back : (Instance mred:Bitmap%) (get-bitmap (here "card-back.png")))

(define dim-back
  (make-dim back))

(define deck-of-cards
  (let* ([w (send back get-width)]
         [h (send back get-height)])
    (let: sloop : (Listof (Instance Card%)) ([suit : Integer 4])
      (if (zero? suit)
          null
          (let: vloop : (Listof (Instance Card%)) ([value : Integer 13])
            (sleep)
            (if (zero? value)
      	  (sloop (sub1 suit))
      	  (let ([front (get-bitmap
      			(here
      			 (format "card-~a-~a.png"
      				 (sub1 value)
      				 (sub1 suit))))])
      	    (cons (make-object card-class:card%
      		    suit
      		    value
      		    w h
      		    front back
      		    (lambda () (make-dim front))
      		    (lambda () dim-back)
                    (make-hash))
      		  (vloop (sub1 value))))))))))

(: make-card ((Instance mred:Bitmap%) (Instance mred:Bitmap%) Any Any -> (Instance Card%)))
(define (make-card front-bm back-bm suit-id value)
  (let ([w (send back get-width)]
        [h (send back get-height)])
    (make-object card-class:card%
      	         suit-id
      	         value
                 w h
      	         front-bm (or back-bm back)
      	         (lambda () (make-dim front-bm))
      	         (lambda ()
      	           (if back-bm
      	               (make-dim back)
      	               dim-back))
                 (make-hash))))