(in-package :stumpwm)

(export '(*image-loaders*
          load-wallpaper
          clear-wallpaper
          supported-wallpaper-types))

(defparameter *image-loaders* nil
  "Functions which load image files into Ximage")

(defun set-wallpaper-property (screen pixmap)
  (xlib:change-property (screen-root screen) :_XSETROOT_ID (list pixmap) :PIXMAP 32
                        :transform #'xlib:pixmap-id))

(defun get-wallpaper-property (screen)
  (let ((root-window (screen-root screen)))
    (xlib:get-property root-window :_XSETROOT_ID
                       :type :PIXMAP :transform
                       (lambda (id) (xlib::lookup-pixmap (xlib:window-display root-window) id)))))

(defun free-non-native-wallpaper (screen)
  "Free the wallpaper set by an external program which wished
   to retain its resources (like xli)"
  (let ((old-pixmaps (get-wallpaper-property screen)))
    (when old-pixmaps
      (xlib:kill-client *display* (xlib:pixmap-id (car old-pixmaps)))
      #+nil (xlib:free-pixmap old-pixmap)
      (xlib:delete-property (screen-root screen) :_XSETROOT_ID))))

(defun set-wallpaper (screen image)
  (let ((image-width (xlib:image-width image))
        (image-height (xlib:image-height image))
        (image-depth (xlib:image-depth image))
        (screen-width (screen-width screen))
        (screen-height (screen-height screen))
        (screen-depth (screen-depth screen)))
    (if (or
         (> image-width screen-width)
         (> image-height screen-height))
        (throw 'error "Image is too big"))
    (if (/= image-depth screen-depth)
        (throw 'error "Unsupported color depth"))

  (free-non-native-wallpaper screen)
  (let* ((root-window (screen-root screen))
         (pixmap (xlib:create-pixmap :width screen-width
                                     :height screen-height
                                     :depth screen-depth
                                     :drawable root-window))
         (gc (xlib:create-gcontext :drawable pixmap
                                   :foreground (screen-bg-color screen))))

    ;; Kludge. Fill pixmap with background color if sizes do not match
    (if (or (/= screen-width image-width)
            (/= screen-height image-height))
        (xlib:draw-rectangle pixmap gc 0 0 screen-width screen-height t))
    (xlib:put-image pixmap gc image
                    :x (floor (- screen-width image-width) 2)
                    :y (floor (- screen-height image-height) 2))
    (xlib:free-gcontext gc)
    (setf (xlib:window-background root-window) pixmap)
    ;; Update _XSETROOT_ID property so we can free the pixmap later
    ;; FIXME: Do not use it, as we can shoot ourselves later
    #+nil (set-wallpaper-property screen pixmap)
    ;; Rather free it now, as it is not used anywhere else currently
    #+t (xlib:free-pixmap pixmap)
    (xlib:clear-area root-window))))

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

(defun clear-wallpaper (screen)
  (free-non-native-wallpaper screen)
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
  (if (string/= filename "")
      (load-wallpaper (current-screen) filename)
      (clear-wallpaper (current-screen))))
