(use-modules (srfi srfi-1)
	     (ice-9 popen)
	     (ice-9 rdelim)
	     (ice-9 regex)
	     (ice-9 ftw)
	     (ice-9 match))

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

(define (get-duration path)
  (let ((full-command
	 (string-append "mediainfo --Inform='Audio;%Duration%' "
			(shell-quote path))))
;    (display full-command)
    (string->number
     (chomp (command->string full-command)))))

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

(define (is-audio-file? filename)
  (let ((suffices '(".flac" ".ogg" ".mp3")))
    (any
     (lambda (suffix)
       (string-suffix-ci? suffix filename))
     suffices)))

(define (count-files files)
  (apply + (map get-duration files)))

(define (unzip path)
  (let ((target-dir (basename path ".zip")))
    (system/wrapped (string-append "unzip -o -d " (shell-quote target-dir) " "
			   (shell-quote path)))
    target-dir))

(define (is-zipbomb? path)
  (any
   (lambda (path) (not (string-contains path "/")))
   (filter (lambda (path) (not (string=? path "")))
	  (string-split
	   (command->string (string-append "zipinfo -1 " (shell-quote path)))
	   #\newline))))

(define (maybe-unzip path)
  (if (string-suffix-ci? ".zip" path)
	(unzip path)
	path))


(define (scan-tree root)
  (let loop ((path-so-far root)
	     (tree (file-system-tree root))
	     (in-root? #t))
    (let ((full-path (if in-root?
			 path-so-far
			 (string-append path-so-far "/" (first tree)))))
      (cond
       ((= (length tree) 2)
	(if (is-audio-file? full-path)
	    (list full-path)
	    '()))
       (else
	(apply append (map (lambda (x)
			     (loop full-path x #f))
			   (cdr (cdr tree)))))))))

(define (decode file)
  (let ((output-file (string-append file ".wav")))
    (system/wrapped (string-append "ffmpeg -y -i " (shell-quote file) " "
			   (shell-quote output-file)))
    output-file))

(define (fix-format file)
  (define sample-rate 44100)
  (define channels 2)
  (define encoding "signed-integer")
  (define bits 16)
  
  (let ((output-file (string-append file ".wav-fixed.wav")))
    (system/wrapped (string-append "sox -G " (shell-quote file)
			   " -r " (number->string sample-rate)
			   " -c " (number->string channels)
			   " -e " encoding
			   " -b " (number->string bits)
			   " " (shell-quote output-file)))
			   
    output-file))

(define (burn files)
  (let ((all-files (string-join (map shell-quote files))))
    (system/wrapped (string-append "wodim -v -sao speed=1 -audio -pad -copy "
			    all-files))))
(define (disk-usage name)
;  (display name)
  (let ((command (string-append "du -sb " (shell-quote name) " | cut -f 1")))
;    (display name)
;    (newline)
;    (display command)
;    (newline)
    (string->number (chomp (command->string command)))))
  
(define (main . args)
  (let ((burn-list (apply append (map (lambda (name)
  
  (let ((item (maybe-unzip name)))
    (let ((files (scan-tree item)))
      (let ((decoded (map decode files)))
	  (let ((fixed (map fix-format decoded)))
	    (write fixed)
	    (sort fixed string<))))))
       args))))
    (let ((count (exact->inexact (milliseconds->minutes (count-files burn-list)))))
      (when (> count 80)
        (error "too large for disc" count))
      (when (< count 40)
        (error "too small for disc" count))

      (burn burn-list))))

(define (main2)
  (let ((usages (map (lambda (dir)
		       (cons dir (disk-usage dir)))
		     (scandir "."  (lambda (x)
				     (not (or (string=? x ".")
				       (string=? x ".."))))))))
    (write (map car (take (sort usages (lambda (x y) (> (cdr x) (cdr y))))
			  5)))))



(apply main (cdr (program-arguments)))

;; (write (exact->inexact (milliseconds->minutes
;; 	(get-duration (second (program-arguments))))))
