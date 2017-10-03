(defpackage stumpwm.wallpaper
  (:use :cl :stumpwm)
  (:export #:*wallpaper-dir*))

(in-package :stumpwm.wallpaper)

(defvar *wallpaper-dir* (pathname-as-directory (concat (getenv "HOME") "/.stumpwm.d/wallpapers")))

(defcommand choose-wallpaper ()
  ()
  (let* ((screen (current-screen))
         (head (current-head))
         (files (cl-fad:list-directory *wallpaper-dir*))
         (pictures (remove-if-not
                    (lambda (file)
                      (find (pathname-type file)
                            (supported-wallpaper-types)
                            :test #'string=))
                    files))
         (menu (mapcar (lambda (file)
                         (list (file-namestring file) file)) pictures))
         (selection (select-from-menu
                     screen menu
                     "Select a wallpaper")))
    (if (null selection) (throw 'error "Abort."))
    (load-wallpaper (second selection) screen head)
    (stumpwm.preferences:set-preference
     (list :wallpaper (stumpwm::screen-id screen) (position head (screen-heads screen)))
     (second selection)
     :test #'equalp)))

(defun restore-wallpapers ()
  (dolist (screen *screen-list*)
    (dotimes (head-idx (length (screen-heads screen)))
      (let ((wallpaper (stumpwm.preferences:get-preference
                        (list :wallpaper (stumpwm::screen-id screen) head-idx)
                        :test #'equalp)))
        (if wallpaper (load-wallpaper wallpaper screen (nth head-idx (screen-heads screen))))))))

(register-module "STUMPWM.WALLPAPER"
                 :init-fn #'restore-wallpapers)
