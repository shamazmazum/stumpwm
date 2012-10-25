;; Copyright (C) 2008 Shawn Betts
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
;; Implementation of an interactive menu.
;;
;; Code:

;;; interactive menu

(in-package #:stumpwm)

(export '(select-from-menu))

(defvar *menu-map* nil
  "The keymap used by the interactive menu.")

(when (null *menu-map*)
  (setf *menu-map*
        (let ((m (make-sparse-keymap)))
          (define-key m (kbd "C-p") 'menu-up)
          (define-key m (kbd "Up") 'menu-up)
          (define-key m (kbd "S-Up") 'menu-scroll-up)
          (define-key m (kbd "SunPageUp") 'menu-page-up)

          (define-key m (kbd "C-n") 'menu-down)
          (define-key m (kbd "Down") 'menu-down)
          (define-key m (kbd "S-Down") 'menu-scroll-down)
          (define-key m (kbd "SunPageDown") 'menu-page-down)

          (define-key m (kbd "DEL") 'menu-backspace)

          (define-key m (kbd "C-g") 'menu-abort)
          (define-key m (kbd "ESC") 'menu-abort)
          (define-key m (kbd "RET") 'menu-finish)
          (define-key m (kbd "S-RET") 'menu-literal)
          (define-key m (kbd "TAB") 'menu-complete-prefix)
          m)))

(defstruct (menu-option
             (:constructor make-menu-option%))
  name value)

(defstruct menu-state
  prompt selected view-start view-end allow-literal
  input-action options current-options
  (current-input (make-array 10
                             :element-type 'character
                             :adjustable t
                             :fill-pointer 0)))

(defun make-menu-option (value)
  (make-menu-option% :name (if (listp value)
                               (first value)
                               value)
                     :value value))

(defun bound-check-menu (menu)
  "Adjust the menu view and selected item based
on current view and new selection."
  (let ((len (length (menu-state-current-options menu))))
    (setf (menu-state-selected menu)
          (cond ((not (menu-state-selected menu)) nil)
                ((< (menu-state-selected menu) 0) (1- len))
                ((>= (menu-state-selected menu) len) 0)
                (t (menu-state-selected menu))))
    (when (and *menu-maximum-height*
               (> len *menu-maximum-height*))  ; scrolling required
      (let ((sel (menu-state-selected menu)))
        (setf (values (menu-state-view-start menu)
                      (menu-state-view-end menu))
              (cond ((< sel *menu-maximum-height*)
                     (values 0 *menu-maximum-height*))
                    ((> sel (- len *menu-maximum-height*))
                     (values (- len *menu-maximum-height*) len))
                    ((< sel (menu-state-view-start menu))
                     (values (- sel *menu-scrolling-step*)
                             (- (+ sel *menu-maximum-height*)
                                *menu-scrolling-step*)))
                    ((>= sel (menu-state-view-end menu))
                     (values (+ (- sel *menu-maximum-height*)
                                *menu-scrolling-step*)
                             (+ sel *menu-scrolling-step*)))
                    (t
                     (values (menu-state-view-start menu)
                             (menu-state-view-end menu)))))))))

(defun menu-up (menu)
  (when (menu-state-selected menu)
    (decf (menu-state-selected menu))
    (bound-check-menu menu)))

(defun menu-down (menu)
  (when (menu-state-selected menu)
    (incf (menu-state-selected menu))
    (bound-check-menu menu)))

(defun menu-scroll-up (menu)
  (when (menu-state-selected menu)
    (decf (menu-state-selected menu) *menu-scrolling-step*)
    (bound-check-menu menu)))

(defun menu-scroll-down (menu)
  (when (menu-state-selected menu)
    (incf (menu-state-selected menu) *menu-scrolling-step*)
    (bound-check-menu menu)))

(defun menu-page-up (menu)
  (when (menu-state-selected menu)
    (when *menu-maximum-height* ;;No scrolling = no page up/down
      (decf (menu-state-selected menu) *menu-maximum-height*)
      (let ((*menu-scrolling-step* *menu-maximum-height*))
        (bound-check-menu menu)))))

(defun menu-page-down (menu)
  (when (menu-state-selected menu)
    (when *menu-maximum-height*
      (incf (menu-state-selected menu) *menu-maximum-height*)
      (let ((*menu-scrolling-step* *menu-maximum-height*))
        (bound-check-menu menu)))))


(defun menu-finish (menu)
  (throw :menu-quit (if (menu-state-selected menu)
                        (menu-option-value (elt (menu-state-current-options menu) (menu-state-selected menu)))
                        nil)))

(defun menu-abort (menu)
  (declare (ignore menu))
  (throw :menu-quit nil))

(defun menu-literal (menu)
  (when (menu-state-allow-literal menu)
      (throw :menu-quit (menu-state-current-input menu))))

(defun get-input-char (key)
  "If @var{key} is a character suitable for menu completion (e.g. not
backspace or F9), return it otherwise return nil"
  (let ((char (xlib:keysym->character *display* (key-keysym key))))
    (if (or (key-mods-p key) (null char)
            (not (characterp char)))
        nil
        char)))

(defun menu-backspace (menu)
  (when (> (fill-pointer (menu-state-current-input menu)) 0)
    (vector-pop (menu-state-current-input menu))
    (menu-input menu nil)))

(defun menu-complete-prefix (menu)
  (unless (eq (first (menu-state-input-action menu)) :nothing)
    (let* ((common-prefix (common-prefix (map 'vector #'menu-option-name (menu-state-current-options menu))))
           (common-length (length common-prefix)))
      (when (> common-length
               (length (menu-state-current-input menu)))
        (setf (fill-pointer (menu-state-current-input menu)) common-length)
        (setf (subseq (menu-state-current-input menu) 0 common-length)
              common-prefix)))))

(defun common-prefix (strings)
  (case (length strings)
    (0 "")
    (1 (elt strings 0))
    (t (let ((common-chars (loop
                              for i below (reduce #'min (map 'vector #'length strings))
                              while (every (lambda (s) (eq (char s i)
                                                           (char (elt strings 0) i)))
                                           (subseq strings 1))
                              finally (return i))))
         (subseq (elt strings 0) 0 common-chars)))))

(defun menu-input (menu key-seq &aux (action (first (menu-state-input-action menu))))
  (unless (eq action :nothing)
    ;; First append the new character
    (let ((input-char (and key-seq (get-input-char key-seq))))
      (when input-char
        (vector-push-extend input-char (menu-state-current-input menu)))
      (ecase action
        ;; The mode is to use input to generate options
        (:generate
         (setf (menu-state-current-options menu)
               (funcall (second (menu-state-input-action menu))
                        (menu-state-current-input menu))))
        ;; The mode is to use pre-existing options
        ((:search :filter)
         ;; Some common code
         (let* ((options (menu-state-options menu))
                (strings (map 'vector #'menu-option-name options))
                (input (menu-state-current-input menu))
                (max (ecase action
                       (:search 1)
                       (:filter (if *menu-maximum-height*
                                    (+ *menu-maximum-height* 1)
                                    (length options)))))
                (found-indexes (funcall (second (menu-state-input-action menu))
                                        strings input max)))
           (ecase action
             ;; Move the cursor to the first found option
             (:search (if (zerop (length found-indexes))
                          (setf (menu-state-selected menu) nil)
                          (let ((found-index (elt found-indexes 0)))
                            (when found-index
                              (setf (menu-state-selected menu) found-index)))))
             ;; Remove extra options
             (:filter (let ((filtered-indexes (funcall (second (menu-state-input-action menu))
                                                       strings input max)))
                        (setf (fill-pointer filtered-indexes) (min (length filtered-indexes) max))
                        (let ((new-options (map 'vector (lambda (idx) (aref options idx))
                                                filtered-indexes)))

                          (setf (menu-state-current-options menu) new-options)
                          (setf (menu-state-selected menu)
                                (if (> (length new-options) 0)
                                    0
                                    nil)))))))))
      (bound-check-menu menu))))

(defun menu-filter-regexp (strings input max)
  "STRINGS is an array (deal with it) of strings to be filtered through this function.
INPUT is the user input to use as a filter
MAX is how many elements you should find. Finding more is a waste of time."
  (let ((input-length (length input))
        (match-regex (ppcre:create-scanner input
                                           :case-insensitive-mode
                                           (string= (string-downcase input)
                                                    input)))
        (result (make-array max :fill-pointer 0))
        (found 0))
    (loop for i from 0
       for string across strings
       until (>= (length result) max)
       do (when (and (>= (length string) input-length)
                     (ppcre:scan match-regex string))
            (incf found)
            (vector-push i result)))
    result))

(defun select-from-menu (screen table
                         &optional prompt
                         &key      (input-prompt "Search: ")
                                   (initial-selection 0)
                                   (input-does :search)
                                   (allow-literal nil))
  "Prompt the user to select from a menu on SCREEN. TABLE can be
a list of values or an alist. If it's an alist, the CAR of each
element is displayed in the menu. What is displayed as menu items
must be strings. Returns the selected element in TABLE or nil if aborted.

See *menu-map* for menu bindings."
  (check-type screen screen)
  (check-type table list)
  (check-type prompt (or null string))
  (check-type initial-selection integer)
  (check-type input-does (or (member :nothing :search :filter)
                             (cons (member :search :filter :generate)
                                   (cons function nil))))
  (check-type allow-literal boolean)
  (let* ((input-action (if (listp input-does)
                           input-does
                           (ecase input-does
                             (:nothing (list :nothing))
                             (:search (list :search #'menu-filter-regexp))
                             (:filter (list :filter #'menu-filter-regexp)))))
         (menu-options (map 'vector #'make-menu-option table))
         (menu-require-scrolling (and *menu-maximum-height*
                                       (> (length menu-options)
                                          *menu-maximum-height*)))
         (menu (make-menu-state
                :prompt prompt
                :options menu-options
                :current-options menu-options
                :input-action input-action
                :allow-literal allow-literal
                :view-start (if menu-require-scrolling initial-selection 0)
                :view-end (if menu-require-scrolling
                              (+ initial-selection *menu-maximum-height*)
                              (length menu-options))
                :selected initial-selection))
         (*record-last-msg-override* t)
         (*suppress-echo-timeout* t))
    (when (eq (first input-action) :generate)
      (menu-input menu nil))
    (bound-check-menu menu)
    ;; Menu input-render loop
    (catch :menu-quit
      (unwind-protect
           (with-focus (screen-key-window screen)
             (loop
                (let* ((all-strings (map 'vector #'menu-option-name (menu-state-current-options menu)))
                       (strings (coerce (subseq all-strings
                                               (menu-state-view-start menu)
                                               (min (menu-state-view-end menu)
                                                    (length all-strings)))
                                        'list)) ; There won't be more than could fit on your screen
                       (highlight (when (menu-state-selected menu)
                                    (- (menu-state-selected menu)
                                       (menu-state-view-start menu)))))
                  (when menu-require-scrolling
                    (unless (= 0 (menu-state-view-start menu))
                      (push "..." strings)
                      (when highlight
                        (incf highlight)))
                    (unless (= (length (menu-state-current-options menu)) (menu-state-view-end menu))
                      (setf strings (nconc strings '("...")))))
                  (unless (= (fill-pointer (menu-state-current-input menu)) 0)
                    (push (format nil "~a~a"
                                  input-prompt
                                  (menu-state-current-input menu))
                          strings)
                    (when highlight
                      (incf highlight)))
                  (when prompt
                    (push prompt strings)
                    (when highlight
                      (incf highlight)))
                  (if highlight
                      (echo-string-list screen strings highlight)
                      (echo-string-list screen strings)))
                (multiple-value-bind (action key-seq) (read-from-keymap (list *menu-map*))
                  (if action
                      (funcall action menu)
                      (menu-input menu (first key-seq))))))
        (unmap-all-message-windows)))))
