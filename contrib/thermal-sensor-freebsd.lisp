(defpackage stumpwm.thermal-sensor-freebsd
  (:use #:cl #:stumpwm)
  (:export #:*hot-level* #:*critical-level* #:*sensors*))

(in-package :stumpwm.thermal-sensor-freebsd)

(defparameter *hot-level* 55
  "Temperature above this will be shown in yellow colour")

(defparameter *critical-level* 70
  "Temperature above this will be shown in red colour")

(defvar *sensors* nil
  "Sysctl names of the sensors and their meaning, e.g.
 '((sysctl1 . name1) (sysctl2 . name2))")

(defvar *mib-hash* (make-hash-table :test #'equal)
  "mib arrays cache")

(defun get-sensor-temperature (sysctl)
  (multiple-value-bind (mib found)
      (gethash sysctl *mib-hash* (freebsd-sysctl:sysctl-name=>mib sysctl))
    (if (not found)
        (setf (gethash sysctl *mib-hash*) mib))
    (freebsd-sysctl:sysctl mib)))

(defun format-temperature (name temp)
  (format nil "T(~a): ^~d ~4f^*°C"
          name
          (cond
            ((> temp *critical-level*) 1)
            ((> temp *hot-level*) 3)
            (t 2))
          temp))

(defun thermal-sensor-modeline (ml)
  (declare (ignore ml))
  (format nil "Thermal sensors: ~{~a~^, ~}"
          (mapcar (lambda (sensor) (format-temperature (cdr sensor)
                                                       (get-sensor-temperature (car sensor))))
          *sensors*)))

(register-module "STUMPWM.CPU-TEMPERATURE-FREEBSD"
                 :init-fn (lambda ()
                            (pushnew '(#\T thermal-sensor-modeline)
                                     *screen-mode-line-formatters*
                                     :test #'equal)))
