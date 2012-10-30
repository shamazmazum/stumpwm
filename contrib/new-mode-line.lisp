
(defpackage :stumpwm.contrib.new-mode-line
  (:use :cl :stumpwm)
  (:export #:set-screen-mode-line
           #:defwidget))

(in-package stumpwm.contrib.new-mode-line)

(pushnew '(#\L fmt-mode-line) *screen-mode-line-formatters* :test 'equal)

(defvar *mode-lines-format* nil)

(defun set-screen-mode-line (&rest lines)
  "Specifies the format for screen-mode-line. Accepts a list of lines where each line is
  a list of widgets where each widget is either a constant string or a symbol (which should
  name a widget subclass or a list (which should be a widget subclass with its initargs"
  (handler-bind ((error #'stumpwm::restarts-menu))
    (with-simple-restart (continue "Revert the previous mode-line format")
      (setf *mode-lines-format*
            (loop for line in lines
               collect (loop for widget in line
                          collect (etypecase widget
                                    (list (apply #'make-instance
                                                 (intern (string (first widget))
                                                         :stumpwm.contrib.new-mode-line)
                                                 (rest widget)))
                                    (symbol (make-instance (intern (string widget)
                                                                   :stumpwm.contrib.new-mode-line)))
                                    (string (make-instance 'constant-string :value widget))))))
      (setf *screen-mode-line-format* "%L"))))

(defclass widget ()
  ((last-update-time :initform (get-internal-real-time))
   (update-interval :type (or number (member :once :always))
             :initform :always
             :initarg :update-interval
             :documentation "How often this widget needs to be updated in seconds. May actually be less often than this but not more.")
   (content :initform nil))
  (:documentation "A class implementing some generic widget behaviour. All mode-line
  widgets should subclass it."))

(defmethod initialize-instance :after ((widget widget) &key)
  (let ((*stump-ml* (stumpwm::head-mode-line (current-head))))
    (render widget)))

(defgeneric render (widget)
  (:documentation "Called when the widget has to render itself. Return the new string
  representation")
  (:method :around ((widget widget))
    (handler-bind ((error #'stumpwm::restarts-menu))
      (with-slots (last-update-time content) widget
        (setf content (call-next-method))
        (setf last-update-time (get-internal-real-time))
        content))))

(defgeneric maybe-render (widget)
  (:documentation "Decides if it's time to render a widget")
  (:method ((widget widget))
    (with-slots (update-interval last-update-time content) widget
      (case update-interval
        (:once (unless content (render widget)))
        (:always (render widget))
        (t (when (<= (+ last-update-time
                        (* update-interval internal-time-units-per-second))
                     (get-internal-real-time))
             (render widget))))
      content)))

(defvar *stump-ml* nil "The current mode-line")
(defun fmt-mode-line (ml)
  (let ((*stump-ml* ml))
    (format nil "~{~{~A~}~^~%~}"
           (loop for line in *mode-lines-format*
              collect (loop for widget in line
                         collect (maybe-render widget))))))

(defmacro defwidget (name (&key slots default-update-interval) &body render-body)
  "A simple helper macro for simple widgets"
  (let ((widget-var (gensym "WIDGET"))
        (class (intern (string name) #.*package*)))
    `(progn (defclass ,class (widget) ,(loop for slot in slots
                                          collect (if (listp slot)
                                                      `(,(first slot) :initarg ,(intern (string (first slot)) :keyword)
                                                              :initform ,(second slot))
                                                      `(,slot :initarg ,(intern (string slot) :keyword)
                                                              :initform nil)))
              ,@(when default-update-interval
                      (list `(:default-initargs :update-interval ,default-update-interval))))
            (defmethod render ((,widget-var ,class))
              (with-slots ,(loop for slot in slots
                                collect (if (listp slot) (first slot) slot))
                  ,widget-var
                ,@render-body)))))

(pushnew :stumpwm.new-mode-line *features*)

;;;; Standard widgets

(defwidget spacer (:default-update-interval :once)
  "^>")

(defwidget constant-string (:default-update-interval :once
                            :slots (value))
  value)

(defwidget urgent-window-list (:slots (window-format))
  (let ((*window-format* (or window-format *window-format*)))
    (stumpwm::fmt-urgent-window-list *stump-ml*)))

(defwidget head-window-list (:slots (window-format))
  (let ((*window-format* (or window-format *window-format*)))
    (stumpwm::fmt-head-window-list *stump-ml*)))

(defwidget head-window-list-hidden-windows (:slots (window-format hidden-window-color))
  (let ((*window-format* (or window-format *window-format*))
        (*hidden-window-color* (if hidden-window-color hidden-window-color *hidden-window-color*)))
    (stumpwm::fmt-head-window-list-hidden-windows *stump-ml*)))

(defwidget window-list (:slots (window-format))
  (let ((*window-format* (or window-format *window-format*)))
    (stumpwm::fmt-window-list *stump-ml*)))

(defwidget group-list (:slots (group-format))
  (let ((*group-format* (or group-format *group-format*)))
    (stumpwm::fmt-group-list *stump-ml*)))

(defwidget head ()
  (stumpwm::fmt-head *stump-ml*))

(defwidget group ()
  (stumpwm::fmt-group *stump-ml*))

(defwidget datetime (:default-update-interval 1)
  (stumpwm::fmt-modeline-time *stump-ml*))
