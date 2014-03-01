(use-modules (srfi srfi-1)
	     (ice-9 popen)
	     (ice-9 rdelim)
	     (ice-9 regex)
	     (ice-9 ftw)
	     (ice-9 match))

(set! *random-state* (random-state-from-platform))

(define (system/wrapped cmd)
  (check-exit-code (system cmd) cmd))

; cmd only used for information
(define (check-exit-code result cmd)
  (let ((exit-code (status:exit-val result)))
    (when (not (= exit-code 0))
      (error "command returned unsuccessful status code" cmd exit-code))))

(define find-unsafe
  (make-regexp "[^[:alnum:]_@%+=:,./-]"))

(define (shell-quote string)
  (cond
   ((string=? string "")
    "''")
   ((not (regexp-exec find-unsafe string))
    string)
   (else
    (string-append "'"
		   (regexp-substitute/global #f "'" string
					     'pre "'\"'\"'" 'post)
		   "'"))))
(define (slurp port)
  (read-delimited "" port))

(define (command->string command)
  (let* ((pipe (open-input-pipe command))
	 (result (slurp pipe)))
    (check-exit-code (close-pipe pipe) command)
    result))

(define (chomp string)
  (if (string=? (string-take-right string 1) "\n")
      (string-drop-right string 1)
      string))
          
(define (milliseconds->minutes val)
  (/ (/ val 1000) 60))

(define (get-duration path)
  (let ((full-command
	 (string-append "mediainfo --Inform='Audio;%Duration%' "
			(shell-quote path))))
;    (display full-command)
    (string->number
     (chomp (command->string full-command)))))

(define (generate-slice-list total-length)
  (define offset 60)
  (define minimum (* 8 60 1000))
  
  (let loop ((n 1) (remaining-time total-length))
    (cond
     ((< remaining-time minimum)  (list remaining-time))
     (else
      (let ((length (+ (* 8 60 1000)
		       (- (* (random offset) 1000) (/ (* offset 1000) 2)))))
	(cons length (loop (+ n 1) (- remaining-time length))))))))

(define (milliseconds->samples ms)
  (* ms (/ 44100 1000)))

(define (main . args)
  (let loop ((n 1) (offset 0) (slices (generate-slice-list
				 (get-duration (first args)))))
    (when (not (null? slices))
      (let ((length
	     (ceiling (milliseconds->samples (car slices))))
	    (offset-samples
	     (ceiling (milliseconds->samples offset))))

	(display length)
	(newline)

	(display offset-samples)
	(newline)

	(system/wrapped (string-append "sox " (shell-quote (first args))
				       " output-" (number->string n)
				       ".wav trim "
				       (number->string offset-samples)
				       "s " (number->string length) "s"))
	
	(loop (+ n 1)
	      (+ offset (car slices))
	      (cdr slices))))))

(apply main (cdr (program-arguments)))

