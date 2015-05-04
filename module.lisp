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
          *module-dir*
	  set-module-dir
          find-module))

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
  (if (find name (list-modules) :test #'string=) name))

(define-stumpwm-type :module (input prompt)
  (or (argument-pop-rest input)
      (completing-read (current-screen) prompt (list-modules) :require-match t)))

(defcommand load-module (name) ((:module "Load Module: "))
  "Loads the contributed module with the given NAME."
  (let ((module (find-module name)))
    (when module
      (with-synced-asdf
        (asdf:operate 'asdf:load-op module)))))
;; End of file
