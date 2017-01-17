#-sbcl (error "Currently, only SBCL builts are supported")

(require 'sb-posix)

(defun get-target-name ()
  (sb-posix:getenv "STUMPWM_TARGET"))

(ql:quickload :stumpwm)

(sb-ext:save-lisp-and-die (get-target-name)
                          :executable t
                          :toplevel #'stumpwm:stumpwm)
