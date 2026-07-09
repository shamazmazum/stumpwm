(in-package #:stumpwm)

(defparameter *hot-level* 55
  "Temperature above this will be shown in yellow colour")
(export '*hot-level*)

(defparameter *critical-level* 70
  "Temperature above this will be shown in red colour")
(export '*critical-level*)

(defvar *thermal-sensors* nil
  "Sysctl names of the sensors and their meaning, e.g.
 '((sysctl1 . name1) (sysctl2 . name2))")
(export '*thermal-sensors*)

(defvar *mib-hash* (make-hash-table :test #'equal)
  "mib arrays cache")

(defun get-sensor-temperature (sysctl)
  (let* ((mib (gethash sysctl *mib-hash*))
         (mib (or mib (setf (gethash sysctl *mib-hash*)
                            (freebsd-sysctl:sysctl-name=>mib sysctl)))))
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
          (mapcar
           (lambda (sensor)
             (format-temperature
              (cdr sensor)
              (get-sensor-temperature (car sensor))))
          *thermal-sensors*)))

(pushnew '(#\T thermal-sensor-modeline)
         *screen-mode-line-formatters*
         :test #'equalp)
