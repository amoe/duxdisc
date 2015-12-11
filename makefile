# makefile for duxdisk

guile_libdir = /usr/local/share/guile/site
guile_path = /usr/bin/guile
duxdisc_lib = $(guile_libdir)/duxdisc

install:
	(printf "%s %s \\ \n-L %s -s\n%s" "#!" $(guile_path) $(duxdisc_lib) "!#"; cat burn_cd.scm) > /tmp/main.scm
	mkdir -p $(guile_libdir)/duxdisc
	cp os-interface.scm $(guile_libdir)/duxdisc
	cp /tmp/main.scm /usr/local/bin/duxdisc
	chmod 755 /usr/local/bin/duxdisc
