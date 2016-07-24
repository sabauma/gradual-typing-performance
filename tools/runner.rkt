#lang racket/base

(require
  racket/cmdline
  racket/list
  racket/match
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
    [("-j" "--jobid") j
     "Job identifier: probably should be a number but I don't really care"
     (set! benchmark-jobid j)]
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
    (config benchmark-iterations benchmark-jobid benchmark-progress benchmark-systems pattern)))

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
    (apply proc (reverse files)))

  ;; Tricksy bit: the right fold ensures the proc is call in the dynamic context of
  ;; the iterated sequence of call-with-output-file*, ensuring that all files are
  ;; cleaned up afterwards.
  ((foldr cons-case nil-case fnames) '()))

(define (extract-runtimes in-port)
  (regexp-match* parse-time in-port #:match-select cadr))

(define (path->configuration-name job)
  (define path (path-only (string->path job)))
  (path->string (last (explode-path path))))

(define (run-benchmarks c output-files)
  (define iters         (config-iterations c))
  (define jid           (config-jobid c))
  (define systems       (config-systems c))
  (define pattern       (config-pattern c))
  (define show-progress (config-progress c))

  (unless (= (length output-files) iters)
    (error "numer of output files does not match iteration count"))

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
        (unless (= (monitor-proc 'exit-code) 0)
          (error "Process terminated with non-zero exit code"))

        ;; Success: extract the runtimes
        (define runtimes (extract-runtimes stdout))
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
  (printf "~s~n" configuration)

  (define output-fnames
    (for/list ([i (config-iterations configuration)])
      (format "results.~a.~a" (add1 i) (or (config-jobid configuration) "testing"))))

  (call-with-output-files* output-fnames
    (λ ports (run-benchmarks configuration ports))
    #:exists 'truncate)

  ;(call-with-output-files* '("/tmp/1" "/tmp/2" "/tmp/3")
    ;(λ (a b c) (write "1" a) (write "2" b) (write "3" c))
    ;#:exists 'truncate)

  ;; Open files to be used to store output
  ;(define output-files
    ;(for/list ([i (in-range (config-iterations configuration))])
      ;(define fname (format "results.~a.~a"
                            ;(add1 i)
                            ;(or (config-jobid configuration) "testing")))
      ;(open-output-file fname #:mode 'text #:exists 'truncate)))
  )
