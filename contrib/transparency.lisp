

(in-package :stumpwm)

(defun %set-message-window-transparency (value)
  (dolist (screen *screen-list*)
    (setf (xlib:window-transparency (screen-message-window screen)) value)))

(defun %set-mode-line-transparency (value)
  (dolist (screen *screen-list*)
    (dolist (head (screen-heads screen))
      (setf (xlib:window-transparency (mode-line-window (head-mode-line head))) value))))

(defcommand set-message-window-transparency (value)
    ((:rest "Value: "))
    "Set opacity for message window"
  (%set-message-window-transparency (read-from-string value)))

(defcommand set-mode-line-transparency (value)
    ((:rest "Value: "))
    "Set opacity for mode line"
  (%set-mode-line-transparency (read-from-string value)))


