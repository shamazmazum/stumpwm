;; This is an example (and my own) .stumpwmrc file

;; Summary:
;; 1) Cool contribution modules are loaded.
;;
;; 2) You can close a window with C-t d and close all windows with C-t ]
;; Also there is "safe quit" command accessible with C-t q
;; It makes stumpwm to quit only if there are no windows left
;;
;; 3) App launcher on C-t C-r
;;
;; 4) Nice switching between displays with C-t C-s n, C-t C-s p

(load-module "stumpwm.image-jpeg")
(load-module "stumpwm.image-ppm")
(load-module "stumpwm.app-menu")
(load-module "stumpwm.preferences")
(load-module "stumpwm.wallpaper")
(load-module "stumpwm.cpu-temp-freebsd")
(set-font (list
           "-*-terminus-medium-r-normal-*-16-*-*-*-*-*-iso10646-1"
           (make-instance 'xft:font :family "DejaVu Sans Mono" :subfamily "Book" :size 11)))

;; Program launcher
(stumpwm.app-menu:set-app-list '(("b" . "vncviewer")
                                 ("f" . "firefox")
                                 ("t" . "uxterm")
                                 ("e" . "evince")
                                 ("v" . "VirtualBox")
                                 ("g" . "gimp")))
(define-key stumpwm.app-menu:*launcher-bindings* (kbd "M-m") "launcher-menu")
(define-key *root-map* (kbd "C-r") stumpwm.app-menu:*launcher-bindings*)

(define-key *root-map* (kbd "d") "delete-window")

;; Screen settings
(defvar *screen-bindings* (make-sparse-keymap))

(defcommand my-snext () ()
  (snext)
  (message "Current Screen"))

(defcommand my-sprev () ()
  (sprev)
  (message "Current Screen"))

(define-key *screen-bindings* (kbd "n") "my-snext")
(define-key *screen-bindings* (kbd "p") "my-sprev")
(define-key *root-map* (kbd "C-s") *screen-bindings*)

;; Safe quit

(defcommand safe-quit () ()
  (dolist (screen *screen-list*)
    (dolist (group (screen-groups screen))
      (if (/= 0 (length (group-windows group))) (throw 'error "You must close all windows first"))))
  (run-commands "quit"))
(define-key *root-map* (kbd "q") "safe-quit")

;; Close all windows (so you can quit safely)
(defcommand delete-all () ()
  (dolist (screen *screen-list*)
    (dolist (group (screen-groups screen))
      (dolist (window (group-windows group))
        (delete-window window)))))
(define-key *root-map* (kbd "]") "delete-all")

(setq *screen-mode-line-format* "%w^>%T %d"
      *mode-line-timeout* 1)

;; Show mode line on each screen
(dolist (screen *screen-list*)
  (toggle-mode-line screen
                    (group-current-head
                     (stumpwm::screen-current-group screen))))

;; Focus follows mouse click
(setq *mouse-focus-policy* :click)
