#lang racket/base

(require
  racket/cmdline
  racket/list
  racket/match
  racket/os
  racket/path
  racket/system
  glob)

(struct config (iterations jobid progress systems pattern) #:transparent)

(define (get-arguments argv)
  ;; set! or parameters. Are you testing me Satan?
  (define benchmark-iterations 1)
  (define benchmark-systems '())
  (define benchmark-jobid #f)
  (define benchmark-progress #f)
  (command-line
    #:program "runner"
    #:argv    argv
    #:once-each
    ;; Iteration count
    [("-i" "--iterations") i
     "Number of benchmark iterations"
     (set! benchmark-iterations (string->number i))]
    ;; Job identifier
    [("-j" "--jobid") j
     "Job identifier: ideally the contents of $PBS_JOB_ID"
     (set! benchmark-jobid j)]
    ;; Print progress information
    [("-p" "--progress")
     "Show job progress"
     (set! benchmark-progress #t)]
    ;; Take each system
    #:multi
    [("-s" "--system") s
     "Benchmark system"
     (set! benchmark-systems (append benchmark-systems (list s)))]
    ;; Extra args are also systems
    #:args (pattern)
    (config benchmark-iterations
            benchmark-jobid
            benchmark-progress
            benchmark-systems
            pattern)))

;; Parses the result of the time macro
(define parse-time #rx"cpu time: ([0-9]*) real time: ([0-9]*) gc time: ([0-9]*)")

;; Call the given proc with a sequence of file handles based generated from the
;; provided list of file names. The handles are passed to the proc in the order
;; specified by the original list.
(define (call-with-output-files* fnames
                                 proc
                                 #:mode   (mode 'binary)
                                 #:exists (exists 'error))
  ;; The fold function accumulates a closure expecting the rest of the file handles
  (define (cons-case file acc)
    (λ (rest)
      (call-with-output-file* file
        (λ (handle) (acc (cons handle rest)))
        #:mode mode
        #:exists exists)))

  ;; Apply the procedure to the list of file handles
  (define (nil-case files)
    (apply proc files))

  ;; Tricksy bit: the left fold ensures the proc is call in the dynamic context of
  ;; the iterated sequence of call-with-output-file* by accumulating a closure which
  ;; iteratively opens each file and passes it to the next closure in the chain.
  ;; Invoking the result on the empty list ensures we invoke the final procedure
  ;; in the dynamic context of each call-with-output-file* call.
  ((foldl cons-case nil-case fnames) '()))

(define (extract-runtimes in-port)
  (regexp-match* parse-time in-port #:match-select cadr))

(define (path->configuration-name job)
  (define path (path-only (string->path job)))
  (path->string (last (explode-path path))))

(define (run-benchmarks c output-files)
  (match-define (config iters jid show-progress systems pattern) c)

  (unless (= (length output-files) iters)
    (error "number of output files does not match iteration count"))

  ;; Iterate over each configuration
  (for ([configuration (in-glob pattern)])

    (when show-progress
      (printf "running configuration: ~s~n" configuration))

    ;; Write the current configuration on each line
    (for ([file output-files])
      (write-string (path->configuration-name configuration) file)
      (write-string " " file))

    ;; Iterate over each system
    (for ([sys systems])
      ;; Run the process
      ;; Command is of the form
      ;; $ <system-with-args> <configuration/main.rkt> <iters>
      (define cmd (format "~a ~a ~a" sys configuration iters))
      (when show-progress
        (printf "$ ~a~n" cmd))
      (match-define (list stdout stdin pid stderr monitor-proc) (process cmd))

      (define (cleanup)
        (close-output-port stdin)
        (close-input-port stdout)
        (close-input-port stderr))

      (define (handle-process)
        ;; Wait for the process to finish
        (monitor-proc 'wait)
        ;; Check termination conditions
        ;(unless (zero? (monitor-proc 'exit-code))
          ;(error "Process terminated with non-zero exit code"))

        (define runtimes
          (if (zero? (monitor-proc 'exit-code))
            ;; Success: extract the runtimes
            (extract-runtimes stdout)
            ;; Failure: subprocess crashed
            (for/list ([_ (in-range iters)]) #"???")))
        (unless (= (length runtimes) iters)
          (error "Got fewer results than iteration count"))

        ;; Write out the results for each iteration of the system to the
        ;; corresponding file
        (for ([time runtimes] [file output-files])
          (write-bytes time file)
          (write-string " "  file)
          (flush-output file)))

      ;; Ensure that process handles are closed regardless
      (dynamic-wind void handle-process cleanup))

    ;; Write a newline for the next configuration
    (for ([file output-files])
      (newline file)
      (flush-output file))
    )
  )

(module+ main
  (define configuration (get-arguments (current-command-line-arguments)))

  (define output-fnames
    (for/list ([i (config-iterations configuration)])
      (format "results.~a.~a" (add1 i) (or (config-jobid configuration) (getpid)))))

  (call-with-output-files* output-fnames
    (λ ports (run-benchmarks configuration ports))
    #:exists 'truncate))

(module+ test
  (require rackunit)

  (define (choice chars n)
    (define (char) (string-ref chars (random (string-length chars))))
    (list->string
      (for/list ([i (in-range n)])
        (char))))

  (define (random-file)
    (define tmp   (find-system-path 'temp-dir))
    (define fname (choice "abcdefghijklmnopqrstuvwxyz" 15))
    (build-path tmp fname))

  (define file-names (for/list ([_ (in-range 15)]) (random-file)))

  (call-with-output-files* file-names
    (λ handles
       (for ([i (in-range (length handles))]
             [h handles])
         (write i h)))
    #:exists 'truncate)

  (for ([i (in-range (length file-names))]
        [fname file-names])
    (check-equal? (file-exists? fname) #t)
    (call-with-input-file* fname
      (λ (file) (check-equal? (read file) i))))
  )

