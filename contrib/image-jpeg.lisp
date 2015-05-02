(defpackage stumpwm.contrib.image-jpeg
  (:use :cl :stumpwm))

(in-package :stumpwm.contrib.image-jpeg)

(defun read-jpeg (name)
  (flet ((make-color (r g b)
           (declare (type (unsigned-byte 8) r g b))
           (logior
            (ash r 16)
            (ash g 8)
            (ash b 0))))
    (multiple-value-bind (buffer height width)
        (jpeg:decode-image name)
      (let ((array (make-array (list height width)
                               :element-type 'xlib:pixel))
            (k 0))
        (dotimes (i height)
          (dotimes (j width)
            (setf (aref array i j)
                  (make-color (aref buffer (+ k 2))
                              (aref buffer (+ k 1))
                              (aref buffer (+ k 0))))
            (incf k 3)))
        (xlib:create-image :width width :height height :depth 24 :data array)))))

(pushnew (cons "jpg" #'read-jpeg) *image-loaders*)
(pushnew (cons "jpeg" #'read-jpeg) *image-loaders*)
