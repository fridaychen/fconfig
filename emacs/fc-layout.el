;;; fc-layout.el --- layout management -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(defvar *fc-use-layout-stack* nil)

;; layout functions
(let ((layout-stack nil)
      (current nil)
      (named-map (make-hash-table)))

  (defun fc-layout-push (&rest rest)
    "Push the current layout.
REST: not used, for advice."
    (when *fc-use-layout-stack*
      (push (current-window-configuration) layout-stack)))

  (defun fc-layout-pop ()
    "Pop the last layout."
    (when *fc-use-layout-stack*
      (let ((conf (pop layout-stack)))
        (when conf
          (set-window-configuration conf)))))

  (defun fc-layout-current ()
    current)

  (defun fc-layout-save (name)
    "Save current layout with a specific name.
NAME: name for the current layout."
    (let ((s (if (stringp name) (intern name) name)))
      (remhash s named-map)
      (puthash s (current-window-configuration) named-map)))

  (defun fc-layout-load (name)
    "Load specified layout.
NAME: the name of layout"
    (let* ((s (if (stringp name) (intern name) name))
           (conf (gethash s named-map)))
      (setf current name)
      (when conf
        (set-window-configuration conf))))

  (defun fc-layout-switch (name)
    "swith to another layout"

    (fc-layout-save current)
    (fc-layout-load name)))

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
                        (lambda (w) (if (> (window-height w) window-min-height) 1 0))
                      (lambda (w) (if (> (fc-get-window-width w) window-min-width) 1 0)))))
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

;; Predefined Window Layout
(defun fc-use-code-layout ()
  (setq display-buffer-alist
        `(
          ("\\*\\(vc-diff\\|help\\|info\\|man\\|Occur\\|xref\\)\\*\\|\\*Man.*\\|magit-\\(diff\\|log\\)\\|\\*fc text retrieve.*"
           display-buffer-in-side-window
           (side . right) (slot . -1) (window-width . 0.4) (window-height . 0.6))

          ("\\*compilation\\*"
           display-buffer-in-side-window
           (side . right) (slot . 1) (window-width . 0.4) (window-height . 0.4))
          ))
  )

(provide 'fc-layout)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-layout.el ends here
