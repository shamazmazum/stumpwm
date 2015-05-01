(in-package :stumpwm)

(export '(*image-loaders*
          load-wallpaper
          supported-wallpaper-types))

(defparameter *background-pixmaps* (make-hash-table)
  "Root windows background pixmaps")

(defparameter *image-loaders* nil
  "Functions which load image files into Ximage")

(defun clear-wallpaper (screen)
  (multiple-value-bind (old-pixmap old-pixmap-p)
      (gethash screen *background-pixmaps*)
    (when old-pixmap-p
      (xlib:free-pixmap old-pixmap)
      (remhash screen *background-pixmaps*))))

(defun set-wallpaper (screen image)
  (if (not
       (and (= (screen-width screen) (xlib:image-width image))
            (= (screen-height screen) (xlib:image-height image))
            (= (screen-depth screen) (xlib:image-depth image))))
      (throw 'error "Bad Match"))

  (clear-wallpaper screen)
  (let* ((root-window (screen-root screen))
         (pixmap (xlib:image-pixmap root-window image)))

    ;; FIXME: How to put a pixmap in _XSETROOT_ID property?
    (setf (xlib:window-background root-window) pixmap
          (gethash screen *background-pixmaps*) pixmap)
    (xlib:clear-area root-window)))

(defun load-wallpaper (screen name)
  "Load a wallpaper with the NAME onto the screen"
  (declare (type screen screen)
           (type (or string pathname) name))
  (let ((pathname (if (typep name 'string)
                      (pathname name)
                      name)))

    (let ((loader (assoc (pathname-type pathname) *image-loaders*
                         :test #'string=)))
      (if loader
          (set-wallpaper screen (funcall (cdr loader) pathname))
          (throw 'error "Image format not understood")))))

(defun supported-wallpaper-types ()
  "Return supported wallpaper types"
  (mapcar #'car *image-loaders*))

(defcommand wallpaper (filename)
    ((:string "File name: "))
  "Load a wallpaper on the current screen"
  (load-wallpaper (current-screen) filename))
