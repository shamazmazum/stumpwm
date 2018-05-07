(defpackage stumpwm.app-menu
  (:use :cl :stumpwm)
  (:export #:set-app-list
           #:*app-list*
           #:*launcher-bindings*))

(in-package :stumpwm.app-menu)

(defvar *app-list* nil
  "Applications for app menu. This is a list in which elements are
   in the following form: '(key-binding . program-name)")

(defvar *launcher-bindings* (make-sparse-keymap)
  "Keyboard bindings for the launcher")

(defun update-bindings ()
  "Defines launcher commands and key bindings"
  (loop for binding in *app-list* do
       (destructuring-bind (key name &optional command) binding
         (let ((command-name (concat "LAUNCH-"
                                     (string-upcase name))))
           (let ((symbol (intern command-name))
                 (caller #'(lambda ()
                             (run-shell-command (or command name)))))
             (setf (symbol-function symbol) caller
                   (gethash symbol stumpwm::*command-hash*)
                   (stumpwm::make-command :name symbol :class t)))
           (define-key *launcher-bindings* (kbd key) command-name)))))

(defun set-app-list (list)
  "Set app menu list"
  (setq *app-list* list)
  (update-bindings))

;; Example:
;; (set-app-list
;;   '(("g" "gimp")
;;     ("i" "iridium" "iridium --incognito")))

(defcommand launcher-menu () ()
  "Show laucher menu"
  (let* ((options (mapcar (lambda (app)
                            (destructuring-bind (binding app-name &rest skip) app
                              (declare (ignore skip))
                              (list (concat binding " - " app-name)
                                    (concat "launch-" app-name))))
                          *app-list*))
         (selection (select-from-menu
                     (current-screen)
                     options
                     "Select a program to run")))
    (if (null selection) (throw 'error "Abort."))
    (run-commands (second selection))))
