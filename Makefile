stumpwm: *lisp
	env STUMPWM_TARGET=${.TARGET} sbcl --load build.lisp

clean:
	rm -f stumpwm

.PHONY: clean
