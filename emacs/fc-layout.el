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

(provide 'fc-layout)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-layout.el ends here
