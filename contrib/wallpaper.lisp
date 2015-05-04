(defpackage stumpwm.wallpaper
  (:use :cl :stumpwm)
  (:export #:*wallpaper-dir*))

(in-package :stumpwm.wallpaper)

(defvar *wallpaper-dir* (pathname-as-directory (concat (getenv "HOME") "/.stumpwm.d/wallpapers")))

(defcommand choose-wallpaper ()
  ()
  (let* ((files (cl-fad:list-directory *wallpaper-dir*))
         (pictures (remove-if-not
                    (lambda (file)
                      (find (pathname-type file)
                            (supported-wallpaper-types)
                            :test #'string=))
                    files))
         (menu (mapcar (lambda (file)
                         (list (file-namestring file) file)) pictures))
         (selection (select-from-menu
                     (current-screen)
                     menu
                     "Select a wallpaper")))
    (if (null selection) (throw 'error "Abort."))
    (load-wallpaper (current-screen) (second selection))
    (stumpwm.preferences:set-preference
     (cons :wallpaper (stumpwm::screen-id (current-screen)))
     (second selection)
     :test #'equal)))

(flet ((restore-wallpapers ()
         (dolist (screen *screen-list*)
           (let ((wallpaper (stumpwm.preferences:get-preference
                             (cons :wallpaper (stumpwm::screen-id screen))
                             :test #'equal)))
             (if wallpaper (load-wallpaper screen wallpaper))))))
  (restore-wallpapers))
