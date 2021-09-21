;;; fc-screen.el --- setup screen -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(defvar *fc-fringe-width* 16)
(defvar *fc-enable-nyan* nil)
(defvar *fc-screen-ratio-split-threshold* (if *is-gui* (/ 4.0 3) 2.5))
(defvar *fc-enable-beacon* t)
(defvar *fc-enable-golden-ratio* nil)
(defvar *fc-enable-zoom* nil)
(defvar *fc-window-title-format* "🐰 Emacs {%s} 🐱")
(defvar *fc-screensave-backup-winconf*)

;; display line number
(setf display-line-numbers-width-start t)

;; ring
(setf visible-bell nil
      ring-bell-function (lambda ()))

;; setup screen
(when *is-gui*
  (scroll-bar-mode -1)
  (tool-bar-mode -1)

  (setq-default indicate-buffer-boundaries 'left
                indicate-empty-lines 1)

  (defun -fc-setup-fringe ()
    (fringe-mode (cons *fc-fringe-width* 0)))

  (add-hook '*fc-after-theme-hook* #'-fc-setup-fringe))

(unless (and *is-mac* *is-gui*)
  (menu-bar-mode -1))

(column-number-mode t)
(line-number-mode t)

(when *is-laptop*
  (display-battery-mode))

(setf blink-cursor-interval 0.6)

(mouse-avoidance-mode 'animate)
(setf sr-speedbar-skip-other-window-p t)

(defun fc-split-unsplit-window ()
  "Splite/unsplite window."
  (interactive)

  (if (one-window-p)
      (fc-split-window)
    (delete-other-windows)))

(defun fc-window-ratio ()
  "Calculate window ratio."
  (/ (* (window-pixel-width) 1.0) (window-pixel-height)))

(defun fc-split-window ()
  "Splte window."
  (interactive)
  (if (> (fc-window-ratio) *fc-screen-ratio-split-threshold*)
      (split-window-horizontally)
    (split-window-vertically)))

(fc-load 'beacon
  :enable *fc-enable-beacon*

  :after (progn
           (unless *is-gui*
             (setf beacon-color 0))

           (setf beacon-blink-when-window-scrolls nil)
           (beacon-mode -1)))

(fc-load 'nyan-mode
  :enable *fc-enable-nyan*
  :idle t

  :after (progn
           (setf nyan-animation-frame-interval 4)
           (nyan-mode 1)))

(fc-load 'golden-ratio
  :enable *fc-enable-golden-ratio*
  :autoload t

  :after (progn
           (setf golden-ratio-auto-scale t)
           (golden-ratio-mode t)))

(fc-load 'zoom
  :enable *fc-enable-zoom*)

;; screen saver
(fc-load 'fireplace
  :autoload t

  :bind '((fireplace-mode-map
           ("-" fireplace-down)
           ("=" fireplace-up)
           ("`" fireplace-off)
           ("<escape>" fireplace-off)))
  :after (progn
           (fc-unbind-keys '("q" "Q") fireplace-mode-map)

           (setf fireplace-fury 0.01)
           (fireplace-toggle-smoke)

           (fullframe fireplace
                      fireplace-off
                      t
                      (lambda ()
                        (when *fc-enable-nyan*
                          (nyan-mode 0))

                        (when (minibufferp (current-buffer))
                          (other-window 1)
                          (deactivate-mark)
                          (minibuffer-keyboard-quit))

                        (column-number-mode -1)
                        (line-number-mode -1)))

           (defun after-screen-saver ()
             "Setup after screen saver."
             (set-window-configuration *fc-screensave-backup-winconf*)
             (setf *fc-screensave-backup-winconf* nil)

             (when *fc-enable-nyan*
               (nyan-mode 1))

             (display-time-mode -1)
             (column-number-mode t)
             (line-number-mode t))

           (advice-add 'fireplace-off
                       :after #'after-screen-saver)))

(defun fc-screen-saver ()
  "Run screen saver."
  (interactive)

  (setf *fc-screensave-backup-winconf* (current-window-configuration))

  (delete-other-windows)

  (display-time-mode)
  (fireplace)
  (garbage-collect))

(when *fc-enable-screen-saver*
  (fc-add-idle-hook #'fc-screen-saver))

(fc-load 'mode-icons
  :enable *is-gui*
  :idle t
  :after (progn
           (mode-icons-mode 1)))

(fc-load 'info-colors
  :idle t
  :after (progn
           (add-hook 'Info-selection-hook
                     'info-colors-fontify-node)))

(cl-defun fc-setup-window-title (title)
  "Setup window title.
TITLE: new title."
  (if *is-gui*
      (setf frame-title-format title)
    (send-string-to-terminal (format "\033]0;%s\007"
                                     title))))

(defun -fc-setup-window-title ()
  "Hook function."
  (fc-setup-window-title
   (format *fc-window-title-format*
           *fc-project-name*)))

(defun -fc-setup-window-title-before-exit ()
  "Hook function."
  (unless *is-gui*
    (when *is-colorful*
      (fc-setup-window-title "bash"))))

(add-hook '*fc-project-hook* #'-fc-setup-window-title)
(add-hook 'kill-emacs-hook #'-fc-setup-window-title-before-exit)
(add-hook 'kill-emacs-hook #'(lambda () (fc-modal-set-cursor-shape 'box)))

(unless *is-gui*
  (defun fc-change-window-divider ()
    (let ((display-table (or buffer-display-table standard-display-table)))
      (when (and (selected-window)
                 display-table)
        (set-display-table-slot display-table 5 ?│)
        (set-window-display-table (selected-window) display-table))))

  (add-hook 'window-configuration-change-hook 'fc-change-window-divider))

(defun fc-real-column ()
  "Return real column in the screen."
  (let ((col (current-column))
        (width (min fill-column (window-width))))
    (if (< col width)
        col
      (% col width))))

(defun fc-highlight-cursor ()
  "Highlight cursor location."
  (let* ((right-size (/ (- (window-width) (fc-real-column)) 2))
         (beacon-size (if (> right-size beacon-size)
                          beacon-size
                        right-size)))

    (when (> beacon-size 0)
      (beacon-blink))))

(provide 'fc-screen)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-screen.el ends here
