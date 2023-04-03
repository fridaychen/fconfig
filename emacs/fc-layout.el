;;; fc-layout.el --- layout management -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(defvar *fc-layout-stack* nil)
(defvar *fc-layout-current* nil)
(defvar *fc-layout-map* (make-hash-table))
(defvar *fc-layout-spotlight-around-advice* nil)

(defun fc--layout-push (conf)
  (push conf *fc-layout-stack*)

  (when (length> *fc-layout-stack* 6)
    (setq *fc-layout-stack* (butlast *fc-layout-stack* 6))))

(defun fc-layout-push (&rest _)
  "Push the current layout."
  (fc--layout-push (current-window-configuration)))

(defun fc-layout-pop ()
  "Pop the last layout."
  (let ((conf (pop *fc-layout-stack*)))
    (when conf
      (set-window-configuration conf))))

(defun fc-layout-save (name)
  "Save current layout with a specific name.
NAME: name for the current layout."
  (let ((s (if (stringp name) (intern name) name)))
    (remhash s *fc-layout-map*)
    (puthash s (current-window-configuration) *fc-layout-map*)))

(defun fc-layout-load (name)
  "Load specified layout.
NAME: the name of layout"
  (let* ((s (if (stringp name) (intern name) name))
         (conf (gethash s *fc-layout-map*)))
    (setf *fc-layout-current* name)
    (when conf
      (set-window-configuration conf))))

(defun fc-layout-switch (name)
  "Switch to another layout.
NAME: target layout."
  (fc-layout-save *fc-layout-current*)
  (fc-layout-load name))

(cl-defun fc-layout-split (&key (h t) size)
  "Split the current window, and the new window will be selected.
H: horizontal.
SIZE: size of the current window."
  (let* ((func (if h #'split-window-horizontally
                 #'split-window-vertically))
         (wsize (cond
                 ((integerp size)
                  size)
                 ((floatp size)
                  (round (* (if h (frame-width)
                              (frame-height))
                            size))))))
    (select-window (apply func (list wsize)))))

(cl-defun fc-layout-clean (&optional (window (selected-window)))
  "Clean all windows except specified window.
WINDOW: the special window need to be reserved."
  (delete-other-windows window)
  (selected-window))

(cl-defmacro fc-walk-sibling-windows (init-acc form window)
  "Walk throuth all sibling windows, do accumulation.
INIT-ACC: initial accumulation value.
FORM: the form will run for each sibling.
WINDOW: the sibling of this window will be iterated."
  `(cl-loop with it = (window-child (window-parent ,window))
            with acc = ,init-acc
            do (setq acc ,form
                     it (window-next-sibling it))
            while it
            finally return acc))

(defun fc-list-sibling-windows (window)
  "Create a list consist of all sibling windows.
WINDOW: the sibling of the this specific window will be put into the list."
  (fc-walk-sibling-windows nil
                           (cons it acc)
                           window))

(cl-defun fc--maximize-window-in-box-p (window)
  "Test if the specific window is maximized in its box.
WINDOW: target window."
  (let* ((vertical (window-combined-p window))
         (test-func (if vertical
                        (lambda (w)
                          (if (> (window-height w) window-min-height) 1 0))
                      (lambda (w)
                        (if (> (fc-get-window-width w) window-min-width) 1 0)))))
    (<= (fc-walk-sibling-windows 0 (+ acc (apply test-func (list it))) window)
        1)))

(cl-defun fc--maximize-window-in-box (window)
  "Maximize the window in its box.
WINDOW: target window."
  (let* ((vertical (window-combined-p window)))
    (fc-walk-sibling-windows
     nil
     (unless (eq it window)
       (if vertical
           (fc-set-window-height window-min-height it)
         (fc-set-window-width window-min-width it)))
     window)))

(cl-defun fc-toggle-maximize-window-in-box (&optional (window (selected-window)))
  "Toggle the maximization state of the window.
WINDOW: target window."
  (if (fc--maximize-window-in-box-p window)
      (balance-windows (window-parent window))
    (fc--maximize-window-in-box window)))

(cl-defun fc-side-window-p (&optional (window (get-buffer-window)))
  (window-parameter window 'window-side))

(cl-defun fc-side-window-exist-p ()
  (--first (fc-side-window-p it) (window-list)))

(cl-defun fc-close-all-side-window ()
  (--each (window-list)
    (when (fc-side-window-p it)
      (delete-window it))))

(cl-defun fc-close-other-normal-window ()
  (let ((current-win (get-buffer-window)))
    (--each (window-list)
      (unless (or (eq it current-win)
                  (fc-side-window-p it)
                  (eq (window-main-window) (get-buffer-window)))
        (delete-window it)))))

(cl-defun fc-layout-setup-style (style)
  "Setup layout style.
STYLE: new style."
  (let ((setup-func (intern (format "fc-layout-%s-setup" style)))
        (around (intern (format "fc-layout-%s-spotlight-around-advice" style))))

    (message "Setup layout style to %s." style)

    (apply setup-func nil)
    (setq *fc-layout-spotlight-around-advice* around)))

(cl-defun fc-layout-spotlight (&rest rest)
  "Setup spotlight mode for functions.
REST: functions."
  (--each rest
    (advice-add it :around *fc-layout-spotlight-around-advice*)))

;; Predefined Window Layout
(defconst *fc-buf-info-regex* "\\*\\(help\\|info\\|vc-diff\\)\\*\\|\\*Man.*\\|\\magit-\\(diff\\|log\\|revision\\)")
(defconst *fc-buf-shell-regex* "\\*eshell\\*")
(defconst *fc-buf-state-regex* "\\*fc-dict-buffer\\*")

(defvar *fc-left-side-width* 0.4)

;; Simple style
(cl-defun fc-layout-simple-setup ()
  "Setup simple layout."
  (let ((upper-param `((side . right) (slot . -1) (window-width . ,*fc-left-side-width*) (window-height . 0.6)))
        (lower-param `((side . right) (slot . 1) (window-width . ,*fc-left-side-width*) (window-height . 0.4))))
    (setq display-buffer-alist
          `(
            (,*fc-buf-info-regex*
             display-buffer-in-side-window
             ,@upper-param)

            (fc--next-error-buffer-p
             display-buffer-in-side-window
             ,@upper-param)

            (,*fc-buf-shell-regex*
             display-buffer-in-side-window
             ,@lower-param)

            (,*fc-buf-state-regex*
             display-buffer-in-side-window
             ,@lower-param)))))

(cl-defun fc-layout-simple-spotlight-around-advice (orig-fun &rest args)
  (let ((old-conf (current-window-configuration))
        (old-win-list (cl-copy-list (window-list))))
    (apply orig-fun args)

    (when (or (cl-set-difference (window-list) old-win-list)
              (cl-set-difference old-win-list (window-list)))
      (fc--layout-push old-conf)
      (fc-close-other-normal-window))))

(provide 'fc-layout)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-layout.el ends here
