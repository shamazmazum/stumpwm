(in-package :stumpwm)

(export '(*image-loaders*
          load-wallpaper
          supported-wallpaper-types))

(defparameter *image-loaders* nil
  "Functions which load image files into Ximage")

(defun free-wallpaper (screen)
  (let* ((root-window (screen-root screen))
         (old-pixmap (xlib:get-property root-window :_XSETROOT_ID
                                        :type :PIXMAP :transform
                                        (lambda (id) (xlib::lookup-pixmap (xlib:window-display root-window) id)))))
    (if old-pixmap (xlib:free-pixmap (first old-pixmap)))))

(defun set-wallpaper (screen image)
  (if (not
       (and (= (screen-width screen) (xlib:image-width image))
            (= (screen-height screen) (xlib:image-height image))
            (= (screen-depth screen) (xlib:image-depth image))))
      (throw 'error "Bad Match"))

  (free-wallpaper screen)
  (let* ((root-window (screen-root screen))
         (pixmap (xlib:image-pixmap root-window image)))

    (setf (xlib:window-background root-window) pixmap)
    ;; Update _XSETROOT_ID property so we can free the pixmap later
    (xlib:change-property root-window :_XSETROOT_ID (list pixmap) :PIXMAP 32
                          :transform #'xlib:pixmap-id)
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
