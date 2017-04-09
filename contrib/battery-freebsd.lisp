(defpackage stumpwm.battery-freebsd
  (:use #:cl #:stumpwm)
  (:export #:*low-level* #:*critical-level*))

(in-package :stumpwm.battery-freebsd)

(defparameter *low-level* 30
  "Battery charge below this value will be shown in yellow colour")

(defparameter *critical-level* 15
  "Battery charge below this value will be shown in red colour")

(defparameter *life-mib* (freebsd-sysctl:sysctl-name=>mib "hw.acpi.battery.life")
  "MIB for battery life")
(defparameter *time-mib* (freebsd-sysctl:sysctl-name=>mib "hw.acpi.battery.time")
  "MIB for battery remaining time")
(defparameter *state-mib* (freebsd-sysctl:sysctl-name=>mib "hw.acpi.battery.state")
  "MIB for battery state")
(defparameter *state*
  '((0 . "A/C")
    (1 . "Battery")
    (2 . "Charging"))
  "Battery state meaning")

(defun battery-modeline (ml)
  (declare (ignore ml))
  (let ((life (freebsd-sysctl:sysctl *life-mib*))
        (time (freebsd-sysctl:sysctl *time-mib*))
        (state (freebsd-sysctl:sysctl *state-mib*)))
    (format nil "~a (^[~a~d%^]/~d min)"
            (or
             (cdr (assoc state *state*))
             "N/A")
            (bar-zone-color life *low-level* *critical-level* t)
            life time)))

(register-module "STUMPWM.BATTERY-FREEBSD"
                 :init-fn (lambda ()
                            (add-screen-mode-line-formatter #\b 'battery-modeline)))
