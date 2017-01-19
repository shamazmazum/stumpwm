;; This is for storing StumpWM preferences persistent between
;; X sessions. Unlike .stumpwmrc, there is no need of manual editing
;; Preferences object is a simple associative list, so only readable
;; object can be a preference

(defpackage stumpwm.preferences
  (:use :cl :stumpwm)
  (:export #:*preferences-file*
           #:get-preference
           #:set-preference))

(in-package :stumpwm.preferences)

;; FIXME: the user is responsible for creating needed directory
(defvar *preferences-file* (pathname (concat (getenv "HOME") "/.stumpwm.d/pref.lisp-obj"))
  "The location of the preferences file.")

(defvar *preferences* nil
  "StumpWM preferences")

(defvar *preferences-timer* nil)

(defun save-preferences ()
  (handler-case
      (when (get '*preferences* :dirty)
        (ensure-directories-exist *preferences-file*)
        (with-open-file (out *preferences-file*
                             :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create)
          (write *preferences* :stream out)
          (terpri out))
        (setf (get '*preferences* :dirty) nil))
    (file-error ()
      (when *preferences-timer*
          (cancel-timer *preferences-timer*)
          (setq *preferences-timer* nil))
      (message "Cannot write preferences. Make sure if needed directories are accessible"))))

(defun get-preference (key &key (test #'eql))
  "Return a preference value and t if it is set or nil nil otherwise"
  (if (typep key 'symbol)
      (setq key (intern (symbol-name key)
                        (find-package :stumpwm.preferences))))
  (let ((preference (assoc key *preferences* :test test)))
    (values
     (cdr preference)
     (if preference t))))

(defun set-preference (key preference &key (test #'eql))
  "Set or update a preference"
  (let ((key (if (symbolp key)
                 (intern (symbol-name key)
                         (find-package :stumpwm.preferences))
                 key)))
  (setf *preferences*
        (cons
         (cons key preference)
         (remove key *preferences*
                 :test test
                 :key #'car))
        ;; Set dirty flag on preferences
        (get '*preferences* :dirty) t)))

(defun init-preferences ()
  (handler-case
      (with-open-file (in *preferences-file*)
        (setq *preferences* (read in)))
    (file-error () (message "Cannot read preferences")))
  (if (null *preferences-timer*)
      (setq *preferences-timer* (run-with-timer 2 30 #'save-preferences))))

(defun stop-preferences ()
  (save-preferences)
  (when *preferences-timer*
    (cancel-timer *preferences-timer*)
    (setq *preferences-timer* nil)))

(register-module "STUMPWM.PREFERENCES"
                 :init-fn #'init-preferences
                 :stop-fn #'stop-preferences)
