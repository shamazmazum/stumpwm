(defpackage stumpwm.cpu-temp-freebsd
  (:use #:cl #:stumpwm)
  (:export #:*hot-level* #:*critical-level* #:*cpus*))

(in-package :stumpwm.cpu-temp-freebsd)

(defparameter *hot-level* 55
  "Temperature above this will be shown in yellow colour")

(defparameter *critical-level* 70
  "Temperature above this will be shown in red colour")

(defvar *cpus* '(0)
  "CPUs to display")

(defparameter *indicies* '(#\SUBSCRIPT_ZERO #\SUBSCRIPT_ONE
                           #\SUBSCRIPT_TWO  #\SUBSCRIPT_THREE
                           #\SUBSCRIPT_FOUR #\SUBSCRIPT_FIVE
                           #\SUBSCRIPT_SIX  #\SUBSCRIPT_SEVEN
                           #\SUBSCRIPT_EIGHT #\SUBSCRIPT_NINE))

(defun number-to-index (n)
  (labels ((iteration (n acc)
             (if (< n 10)
               (cons (nth n *indicies*) acc)
               (multiple-value-bind (div rem)
                   (floor n 10)
                 (iteration div (cons (nth rem *indicies*) acc))))))
    (concatenate 'string
                 (iteration n nil))))

(defun format-temperature (cpu temp)
  (format nil "T~a: ^~d ~4f^*°C"
          (number-to-index cpu)
          (cond
            ((> temp *critical-level*) 1)
            ((> temp *hot-level*) 3)
            (t 2))
          temp))

(defun cpu-temperature-modeline (ml)
  (declare (ignore ml))
  (format nil "CPU temp: ~{~a~^, ~}"
          (mapcar (lambda (cpu)
                    (format-temperature cpu (cpu-temperature:get-dev.cpu-temperature cpu)))
          *cpus*)))

(register-module "STUMPWM.CPU-TEMPERATURE-FREEBSD"
                 :init-fn (lambda ()
                            (pushnew '(#\T cpu-temperature-modeline)
                                     *screen-mode-line-formatters*
                                     :test #'equal)))
