(defpackage stumpwm.app-menu
  (:use :cl :stumpwm)
  (:export #:set-app-list
           #:*app-list*))

(in-package :stumpwm.app-menu)

(defvar *app-list* nil
  "Applications for app menu. This is a list in which elements are
   in the following form: '(key-binding . program-name)")

(defvar *launcher-bindings* (make-sparse-keymap)
  "Keyboard bindings for the launcher")

(defun update-bindings ()
  "Defines launcher commands and key bindings"
  (loop for binding in *app-list* do
       (destructuring-bind (key . prog-name) binding
         (let ((command-name (concat "LAUNCH-"
                                     (string-upcase prog-name))))
           (let ((symbol (intern command-name))
                 (caller #'(lambda ()
                             (run-shell-command prog-name))))
             (setf (symbol-function symbol) caller
                   (gethash symbol stumpwm::*command-hash*)
                   (stumpwm::make-command :name symbol :class t)))
           (define-key *launcher-bindings* (kbd key) command-name)))))

(defun set-app-list (list)
  "Set app menu list"
  (setq *app-list* list)
  (update-bindings))

;; Example:
;; (set-app-list '(("g" . "gimp") ("m" . "midori")))

(defcommand launcher-menu () ()
  "Show laucher menu"
  (let* ((options (mapcar (lambda (app)
                            (destructuring-bind (binding . app-name) app
                              (list (concat binding " - " app-name)
                                    (concat "launch-" app-name))))
                          *app-list*))
         (selection (select-from-menu
                     (current-screen)
                     options
                     "Select a program to run")))
    (if (null selection) (throw 'error "Abort."))
    (run-commands (second selection))))

(define-key *launcher-bindings* (kbd "M-m") "launcher-menu")
(define-key *root-map* (kbd "C-r") *launcher-bindings*)
