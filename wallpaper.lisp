(in-package :stumpwm)

(export '(*image-loaders*
          load-wallpaper
          clear-wallpaper
          supported-wallpaper-types))

(defparameter *image-loaders* nil
  "Functions which load image files into Ximage")

(defun set-pixmap-property (screen pixmap property)
  (xlib:change-property (screen-root screen) property (list pixmap) :PIXMAP 32
                        :transform #'xlib:pixmap-id))

(defun get-pixmap-property (screen property)
  (let ((root-window (screen-root screen)))
    (xlib:get-property root-window property
                       :type :PIXMAP :transform
                       (lambda (id) (xlib::lookup-pixmap (xlib:window-display root-window) id)))))

(defun free-foreign-wallpaper (screen)
  "Free the wallpaper set by an external program which wished
   to retain its resources (like xli)"
  (let ((old-pixmaps (get-pixmap-property screen :_XSETROOT_ID)))
    (when old-pixmaps
      (xlib:kill-client *display* (xlib:pixmap-id (car old-pixmaps)))
      (xlib:delete-property (screen-root screen) :_XSETROOT_ID)))
  ;; Also delete :_XROOTPMAP_ID here for compositing manager
  (xlib:delete-property (screen-root screen) :_XROOTPMAP_ID))

(defun set-wallpaper (image screen head)
  (declare (type screen screen)
           (type frame head)
           (type xlib:image image))
  (let ((image-width (xlib:image-width image))
        (image-height (xlib:image-height image))
        (image-depth (xlib:image-depth image))
        (screen-depth (screen-depth screen))
        (screen-width (screen-width screen))
        (screen-height (screen-height screen))
        (head-x (head-x head))
        (head-y (head-y head))
        (head-width (head-width head))
        (head-height (head-height head)))

    (if (and head
             (not (find head (screen-heads screen))))
        (throw 'error "Wrong head"))
    (if (or
         (> image-width head-width)
         (> image-height head-height))
        (throw 'error "Image size mismatch"))
    (if (/= image-depth screen-depth)
        (throw 'error "Unsupported color depth"))

    (free-foreign-wallpaper screen)
    (let* ((root-window (screen-root screen))
           (pixmap (or (screen-background-pixmap screen)
                       (xlib:create-pixmap :width screen-width
                                           :height screen-height
                                           :depth screen-depth
                                           :drawable root-window)))
           (gc (xlib:create-gcontext :drawable pixmap
                                     :foreground (screen-bg-color screen))))

      (xlib:draw-rectangle pixmap gc head-x head-y head-width head-height t)
      (xlib:put-image pixmap gc image
                      :x head-x :y head-y :width image-width :height image-height)
      (xlib:free-gcontext gc)
      (setf (xlib:window-background root-window) pixmap)
      ;; Update :_XROOTPMAP_ID property so we can re-use the pixmap later
      (set-pixmap-property screen pixmap :_XROOTPMAP_ID)
      (setf (screen-background-pixmap screen) pixmap)
      (xlib:clear-area root-window))))

(defun load-wallpaper (name &rest args)
  "Load a wallpaper with the NAME onto the screen"
  (declare (type (or string pathname) name))
  (let ((pathname (if (typep name 'string)
                      (pathname name)
                      name)))

    (let ((loader (assoc (pathname-type pathname) *image-loaders*
                         :test #'string=)))
      (if loader
          (apply #'set-wallpaper (funcall (cdr loader) pathname) args)
          (throw 'error "Image format not understood")))))

(defun clear-wallpaper (screen)
  (free-foreign-wallpaper screen)
  (with-accessors ((pixmap screen-background-pixmap)) screen
    (when pixmap
      (xlib:free-pixmap pixmap)
      (setf pixmap nil)))
  (let ((root-window (screen-root screen)))
    (setf (xlib:window-background root-window) :parent-relative)
    (xlib:clear-area root-window)))

(defun supported-wallpaper-types ()
  "Return supported wallpaper types"
  (mapcar #'car *image-loaders*))

(defcommand wallpaper (filename)
  ((:string "File name: "))
  "Load a wallpaper on the current screen
   or remove the old one if a new name is not given"
  (let ((screen (current-screen)))
    (if (string/= filename "")
        (load-wallpaper filename screen (current-head))
        (clear-wallpaper screen))))
