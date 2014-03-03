(define-module (os-interface)
  #:export (system/wrapped
	    shell-quote
	    command->string
	    chomp))

(use-modules (ice-9 regex)
	     (ice-9 popen)
	     (ice-9 rdelim))

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

  
