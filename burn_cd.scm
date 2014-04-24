(use-modules (srfi srfi-1)
	     (srfi srfi-9)
	     (srfi srfi-11)
	     (ice-9 popen)
	     (ice-9 rdelim)
	     (ice-9 regex)
	     (ice-9 ftw)
	     (ice-9 match)
	     (md5)
	     (os-interface))

(define-record-type <options>
  (make-options temporary-output-dir)
  options?
  (temporary-output-dir options:temporary-output-dir options:set-temporary-output-dir!))

(define (setup-environment)
  (make-options "/tmp/burn_cd"))

(define *the-environment* (setup-environment))

(define (original-path->output-path original-path phase extension)
  ; create the output dir if it doesn't exist
  (catch #t
	 (lambda () (mkdir (options:temporary-output-dir *the-environment*)))
	 (lambda (key . parameters) 'ignored))
  
  (string-append (options:temporary-output-dir *the-environment*)
		 "/"
		 (md5 (open-input-string original-path))
		 "-"
		 phase
		 extension))

(define (get-duration path)
  (let ((full-command
	 (string-append "mediainfo --Inform='Audio;%Duration%' "
			(shell-quote path))))
;    (display full-command)
    (string->number
     (chomp (command->string full-command)))))

          
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
  (let ((target-dir (original-path->output-path path "unzipped" "d")))
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
  (let ((output-file (original-path->output-path file "decode" ".wav")))
    (system/wrapped (string-append "ffmpeg -y -i " (shell-quote file) " "
			   (shell-quote output-file)))
    output-file))

(define (fix-format file)
  (define sample-rate 44100)
  (define channels 2)
  (define encoding "signed-integer")
  (define bits 16)

  (let ((input-file (original-path->output-path file "decode" ".wav"))
	(output-file (original-path->output-path file "resample" ".wav")))
    (system/wrapped (string-append "sox -G " (shell-quote input-file)
			   " -r " (number->string sample-rate)
			   " -c " (number->string channels)
			   " -e " encoding
			   " -b " (number->string bits)
			   " " (shell-quote output-file)))
			   
    output-file))

(define (burn files)
  (let ((all-files (string-join (map shell-quote files))))
    (system/wrapped (string-append "sudo wodim -v -sao speed=1 -audio -pad -copy "
			    all-files))))
(define (disk-usage name)
;  (display name)
  (let ((command (string-append "du -sb " (shell-quote name) " | cut -f 1")))
;    (display name)
;    (newline)
;    (display command)
;    (newline)
    (string->number (chomp (command->string command)))))

(define (decode-and-merge items)
  (let loop ((items items) (burn-list '()) (remove-list '()))
    (cond
     ((null? items)  (values burn-list remove-list))
     (else
      (let-values (((burn remove) (decode-and-fix-item (car items))))
	(loop (cdr items) (append burn-list burn) (append remove-list remove)))))))

(define (decode-and-fix-item name)
  (let ((item (maybe-unzip name)))
    (let ((files (scan-tree item)))
      (let ((decoded (map decode files)))
	  (let ((fixed (map fix-format files)))
	    (write fixed)
	    (values (map (lambda (original-path)
			   (original-path->output-path original-path "resample" ".wav"))
			 (sort files string<))
		    (if (not (string=? item name))   ; if we unzipped...
			(list name item)             
			(list name))))))))
			     

(define (recursive-delete path)
  (let ((command (string-append "rm -rf " (shell-quote path))))
    (system/wrapped command)))
  
(define (main . args)
  (let-values (((burn-list remove-list) (decode-and-merge args)))
    (let ((count (exact->inexact (milliseconds->minutes (count-files burn-list)))))
      (when (> count 80)
        (error "too large for disc" count))
      (when (< count 40)
        (error "too small for disc" count))

      (burn burn-list)
      (for-each recursive-delete remove-list))))

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
