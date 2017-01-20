#-sbcl (error "Currently, only SBCL builts are supported")

(require 'sb-posix)

(defun get-builtin-modules ()
  (let ((*read-eval* nil))
    (handler-case
        (with-open-file (in "builtin-modules.lisp-expr")
          (read in))
      (file-error () ()))))

(defun get-target-name ()
  (sb-posix:getenv "STUMPWM_TARGET"))

(ql:quickload :stumpwm)

(let ((builtin-modules (get-builtin-modules)))
  (when builtin-modules
    (format t "Building with modules ~{~a~^, ~}~%" builtin-modules)
    (mapc #'stumpwm:load-module builtin-modules)))

(sb-ext:save-lisp-and-die (get-target-name)
                          :executable t
                          :toplevel #'stumpwm:stumpwm)
