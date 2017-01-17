(defpackage stumpwm.contrib.image-ppm
  (:use :cl :stumpwm))

(in-package :stumpwm.contrib.image-ppm)

(defun read-ppm-line (stream)
  (declare (optimize (speed 3)))
  (let ((line (read-line stream)))
    (if (char= (char line 0) #\#) ; A comment
        (read-ppm-line stream)
        line)))

(defun ppm-value-reader (stream)
  (declare (optimize (speed 3)))
  (flet ((delimiter-position (string)
           (position-if (lambda (char)
                          (or (char= char #\Space)
                              (char= char #\Tab)))
                        string)))
    (let ((read-new t)
          (string ""))
      (declare (type string string))
      (lambda ()
        (if read-new
            (setq string (read-ppm-line stream)
                  read-new nil))

        (let ((delim-pos (delimiter-position string)))
          (if delim-pos
              (prog1
                  (subseq string 0 delim-pos)
                (setq string (subseq string (1+ delim-pos))))
              (prog1
                  string
                (setq read-new t))))))))

;; Working
(defun read-ppm (stream)
  (declare (optimize (speed 3)))
  (let ((reader (ppm-value-reader stream)))
    (let ((head (funcall reader)))
      (if (string/= head "P3")
          (throw 'stumpwm::error "Format not supported")))

    (let* ((width  (parse-integer (funcall reader)))
           (height (parse-integer (funcall reader)))
           (colors (parse-integer (funcall reader)))
           (array (make-array (list height width)
                                 :element-type 'xlib:pixel)))
      (declare (ignore colors))

      (flet ((make-color (r g b)
               (declare (type (unsigned-byte 8) r g b))
               (logior
                (ash r 16)
                (ash g 8)
                (ash b 0))))
        (dotimes (i height)
          (dotimes (j width)
            (setf (aref array i j)
                  (make-color (parse-integer (funcall reader))
                              (parse-integer (funcall reader))
                              (parse-integer (funcall reader)))))))
      (xlib:create-image :width width :height height :depth 24 :data array))))

(register-module "STUMPWM.CONTRIB.IMAGE-PPM"
                 :init-fn
                 (lambda ()
                   (pushnew (cons "ppm" (lambda (name) (with-open-file (in name) (read-ppm in))))
                            *image-loaders*)))
