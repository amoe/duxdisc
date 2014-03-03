(use-modules (srfi srfi-64)
	     (srfi srfi-13)
	     (os-interface))

(test-begin "main")

(define tmp-path "/tmp/guile-test.tmp")
(system/wrapped (string-append "touch " tmp-path))
(test-assert (file-exists? tmp-path))
(delete-file tmp-path)

(define safe-unquoted
  (string-append (char-set->string (char-set-intersection
				    char-set:letter char-set:ascii))
		 (char-set->string (char-set-intersection
				    char-set:digit char-set:ascii))
		 "@%_-+=:,./"))


(define unicode-sample "\u00e9\u00e0\u00df")
(define unsafe (string-append "\"`$\\!" unicode-sample))

(test-equal "''" (shell-quote ""))
(test-equal "'test file name'" (shell-quote "test file name"))
(test-equal safe-unquoted (shell-quote safe-unquoted))

(define (assert-interpolates-correctly u)
  (test-equal (string-append "'test" u "name'")
	      (shell-quote (string-append "test" u "name")))
  (test-equal (string-append "'test" u "'\"'\"'name'\"'\"''")
	      (shell-quote (string-append "test" u "'name'"))))

(for-each assert-interpolates-correctly (map string (string->list unsafe)))

(test-equal "Hello, world!"
	    (command->string "printf '%s' 'Hello, world!'"))

(test-equal "chomped" (chomp "chomped\n"))

(test-end "main")

