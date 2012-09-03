
(in-package :xlib)

;;; Transparency support
(let ((opaque #xFFFFFFFF))
  (defun window-transparency (window)
    "Return the window transparency"
    (float (/ (or (first (xlib:get-property window :_NET_WM_WINDOW_OPACITY)) opaque)  opaque)))
  
  (defun (setf window-transparency) (value window)
    "Set the window transparency"
    (when (numberp value)
      (xlib:change-property window :_NET_WM_WINDOW_OPACITY
                            (list (min (round (* opaque value)) opaque))
                                   :cardinal 32))))

(export '(window-transparency))

(defun window-shadow (window)
  "Return the window transparency"
  (xlib:get-property window :_NET_WM_WINDOW_SHADOW))

(defun (setf window-shadow) (value window)
  "Set the window transparency"
  (when (numberp value)
    (xlib:change-property window :_NET_WM_WINDOW_SHADOW
                          (list value)
                                 :cardinal 32)))
