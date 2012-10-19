
(defpackage :stumpwm.contrib.new-mode-line
  (:use :cl :stumpwm))

(in-package stumpwm.contrib.new-mode-line)

(pushnew '(#\L fmt-new-mode-line) *screen-mode-line-formatters* :test 'equal)

(defun millisecond (&optional (i 1)) (/ i 1000))
(defun minute (&optional (i 1)) (* i 60))
(defun hour (&optional (i 1)) (minute (* i 60)))
(defun day (&optional (i 1)) (hour (* i 24)))

(defvar *widgets* nil "The list of widgets")
(defvar *format* "" "Format string")

(defstruct (widget (:constructor make-widget%))
  "INTERVAL is in seconds. Can be :once which means update once or :always which means update on
each mode-line update."
  (interval :always :type (or (member :once :always) real))
  (value "" :type string)
  (func (lambda () "") :type function)
  timer)

(defun make-widget (interval func)
  "Creates a widget and immediately starts its update-timer"
  (let ((widget (make-widget% :interval interval
                              :func func)))
    (case interval
      (:once (update-widget widget))
      (:always nil)
      (t (setf (widget-timer widget)
                       (run-with-timer 0 interval #'update-widget widget))))))

(defun update-widget (widget)
  "Redraws the widget"
  (setf (widget-value widget)
        (funcall (widget-func widget))))

(defun update-always-widgets ()
  "Updates all widgets with :always interval"
  (dolist (widget (remove :always *widgets* :test-not #'eq))
    (update-widget widget)))

(defun make-new-mode-line (mode-line funcs)
  "Tears down the old mode-line and sets up the new one"
  (dolist (widget *widgets*)
    (when (timer-p (widget-timer widget))
      (cancel-timer (widget-timer widget))))
  (multiple-value-bind (timeouts format)
      (parse-mode-line mode-line)
    (setf *format* format)
    (loop for timeout in timeouts
       for func in funcs
       do (push (make-widget timeout func) *widgets*))))

(defun parse-mode-line (mode-line)
  "Parses the mode-line format string. Returns (values TIMEOUTS FORMAT)"
  (let ((timeouts nil)
        (format (make-string (length mode-line)))
        (pos 0))
    (flet ((write-format-char (c)
             (setf (aref format pos) c)
             (incf pos))
           (read-timeout (s)
             ;; First check if it's a simple not-really-interval
             (ecase (peek-char nil s)
               (#\o (read-char s)
                    :once)
               (#\a (read-char s)
                    :always)
               ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                ;; It's not. Read the interval-string
                (let ((timeout-str (with-output-to-string (timeout-str)
                                     (loop with c = (read-char s)
                                        do (ecase c
                                             ((#\i #\s #\m #\h #\d)
                                              (write-char c timeout-str)
                                              (return))
                                             ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\.)
                                              (write-char c timeout-str)))))))
                  ;; Split
                  (let ((number (read-from-string timeout-str nil nil
                                                  :end (1- (length timeout-str))))
                        (letter (aref timeout-str (1- (length timeout-str)))))
                    ;; And make the result
                    (ecase letter
                      (#\i (millisecond number))
                      (#\s number)
                      (#\m (minute number))
                      (#\h (hour number))
                      (#\d (day number)))))))))
      (with-input-from-string (mode-line mode-line)
        (loop
           (let ((c (read-char mode-line nil nil)))
             (unless c (return))
             (case c
               (#\~ (write-format-char #\~)  ; The FORMAT string will be used in FORMAT
                    (write-format-char #\~)) ; Need to escape ~
               (#\% (case (peek-char nil mode-line)
                      ;; Newlines
                      (#\n
                       (read-char mode-line)
                       (write-format-char #\~)
                       (write-format-char #\%))
                      ;; Escape the %
                      (#\%
                       (read-char mode-line)
                       (write-format-char #\%))
                      ;; Literal character
                      (t
                       (push (read-timeout mode-line) timeouts)
                       (write-format-char #\~)
                       (write-format-char #\A))))
               (t (write-char c))))))
      (values (nreverse timeouts) format))))

(defun fmt-new-mode-line ()
  "Hook to the existing mode-line system"
  (update-always-widgets)
  (format nil *format* (mapcar #'widget-value *widgets*)))

;; For User API
(defmacro stumpwm::set-mode-line (format &rest funcs)
  "Works like the FORMAT function except (except it's a macro) it executes the respective
forms each time a part of the string needs to be updated.

Special character sequences:
%% - literal %
%n - newline
%o - a widget that updates once on startup
%a - a widget that updates every time the mode-line is redrawn
%<number><scale> - a widget that updates at specified intervals.
    Number can be any *real* number, scale is:
        i - millisecond
        s - second
        m - minute
        h - hour
        d - day"
  `(make-new-mode-line ,format
                       ,(loop for func in funcs
                           collect `(lambda () ,func))))
