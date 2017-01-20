stumpwm: *lisp
	@echo "You can build StumpWM with built-in modules,"
	@echo "just specify them in builtin-modules.lisp-expr file as a list of strings"
	env STUMPWM_TARGET="${.TARGET}" sbcl --load build.lisp

clean:
	rm -f stumpwm

.PHONY: clean
