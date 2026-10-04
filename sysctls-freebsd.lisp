(in-package #:stumpwm)

;; FIXME: Include serapeum?
(defmacro -> (name arg-types res-type)
  `(declaim (ftype (function ,arg-types ,res-type) ,name)))

(defparameter *temperature-hot-level* 55
  "Temperature above this will be shown in yellow colour")
(export '*temperature-hot-level*)

(defparameter *temperature-critical-level* 70
  "Temperature above this will be shown in red colour")
(export '*temperature-critical-level*)

(defvar *sysctl-names* nil
  "Sysctl names you want to print and their meaning, e.g.
'((sysctl1 . name1) (sysctl2 . name2))")
(export '*sysctl-names*)

(defvar *sysctl-mib-cache* (make-hash-table :test #'equal)
  "mib arrays cache")

;; TODO: export foreign-data
(-> get-sysctl (string)
    (values sc::foreign-data sc:foreign-type &optional))
(defun get-sysctl (sysctl)
  (let* ((mib (gethash sysctl *sysctl-mib-cache*))
         (mib (or mib (setf (gethash sysctl *sysctl-mib-cache*)
                            (sc:sysctl-name=>mib sysctl)))))
    (values
     (sc:sysctl      mib)
     (sc:sysctl-type mib))))

(defun format-temperature (name temp)
  (format
   nil "T(~a): ^~d ~4f^*°C" name
   (cond
     ((> temp *temperature-critical-level*) 1)
     ((> temp *temperature-hot-level*) 3)
     (t 2))
   temp))

(defun format-integer (name x)
  (format nil "~a: ~d" name x))

(defun format-string (name str)
  (format nil "~a: ~a" name str))

(-> format-sysctl (string sc::foreign-data sc::foreign-type)
    (values (or string null) &optional))
(defun format-sysctl (name data type)
  (typecase type
    ((eql :string)
     (format-string name data))
    ((or sc:signed-integer sc:unsigned-integer)
     (format-integer name data))
    (sc:temperature
     (format-temperature name data))))

(defun sysctl-modeline (ml)
  (declare (ignore ml))
  (format nil "~{~a~^, ~}"
          (mapcar
           (lambda (entry)
             (destructuring-bind (sysctl . name) entry
               (multiple-value-bind (data type)
                   (get-sysctl sysctl)
                 (format-sysctl name data type))))
           *sysctl-names*)))

(pushnew '(#\T sysctl-modeline)
         *screen-mode-line-formatters*
         :test #'equalp)
