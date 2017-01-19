;; Copyright (C) 2008 Julian Stecklina, Shawn Betts, Ivy Foster
;; Copyright (C) 2014 David Bjergaard
;;
;;  This file is part of stumpwm.
;;
;; stumpwm is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; stumpwm is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this software; see the file COPYING.  If not, see
;; <http://www.gnu.org/licenses/>.

;; Commentary:
;;
;; Use `set-module-dir' to set the location stumpwm searches for modules.

;; Code:

(in-package #:stumpwm)

(export '(load-module
          list-modules
	  set-module-dir
          find-module
          register-module))

(defstruct module
  name init-fn stop-fn)

(defvar *initialize-modules* nil
  "Initialize modules on load")

(defvar *loaded-modules-list* nil
  "List of loaded but not initialized modules")

(defvar *initialized-modules-list* nil
  "List of initialized modules")

(defvar *module-dir* (pathname-as-directory (concat (getenv "HOME") "/.stumpwm.d/modules"))
  "The location of the contrib modules on your system.")

(defmacro with-synced-asdf (&body body)
  "Operate with `*MODULE-DIR*' within `ASDF:*CENTRAL-REGISTRY*'"
  `(let ((asdf:*central-registry* (cons *module-dir* asdf:*central-registry*)))
     ,@body))

(defun set-module-dir (dir) 
  "Sets the location of the for StumpWM to find modules"
  (when (stringp dir)
    (setf dir (pathname (concat dir "/"))))
  (setf *module-dir* dir))

(defun list-modules ()
  "Return a list of the available modules."
  (mapcar #'pathname-name
          (remove-if-not
           (lambda (name)
             (string= (pathname-type name) "asd"))
           (list-directory-recursive *module-dir* t))))

(defun find-module (name)
  "Find a module with the given name"
  (find name (list-modules) :test #'string=) name)


(defun register-module (name &key init-fn stop-fn)
  "Register module. Should be called inside a module.
You can set initializer and finalizer for a module using this function"
  (let ((module (or (find name *loaded-modules-list* :key #'module-name :test #'string=)
                    (find name *initialized-modules-list* :key #'module-name :test #'string=))))
    (when (not module)
      (push (make-module
             :name name
             :init-fn init-fn
             :stop-fn stop-fn)
            *loaded-modules-list*))))

(defun initialize-modules (&optional starting)
  "Maybe initialize loaded modules. You can set STARTING to T if you really want
modules to be initialized"
  (if starting (setq *initialize-modules* t))
  (when *initialize-modules*
    (flet ((initialize-module (module)
             (let ((init-fn (module-init-fn module)))
               (if init-fn (funcall init-fn)))))
      (setq *initialized-modules-list*
            (nconc *initialized-modules-list*
                   (mapc #'initialize-module (reverse *loaded-modules-list*)))
            *loaded-modules-list* nil))))

(defun stop-modules ()
  "Finalize all modules before exit"
  (flet ((stop-module (module)
           (let ((stop-fn (module-stop-fn module)))
             (if stop-fn (funcall stop-fn)))))
    (mapc #'stop-module (reverse *initialized-modules-list*))
    (setq *initialized-modules-list* nil)))

(define-stumpwm-type :module (input prompt)
  (or (argument-pop-rest input)
      (completing-read (current-screen) prompt (list-modules) :require-match t)))

(defcommand load-module (name) ((:module "Load Module: "))
  "Loads the contributed module with the given NAME."
  (let ((module (find-module name)))
    (when module
      (with-synced-asdf
        (asdf:operate 'asdf:load-op module))
      (initialize-modules))))
;; End of file
