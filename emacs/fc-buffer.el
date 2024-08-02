;;; fc-buffer.el --- buffer management -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(defvar *fc-buffer-count-threshold* 4)
(defvar *fc-buffer-pop-display-action* '(display-buffer-reuse-window
                                         display-buffer-pop-up-window))

;; buffer extension
(cl-defun fc-buffer-visible-p (&optional (bufname (current-buffer)))
  "Test if a buffer is visible.
BUFNAME: to be tested."
  (when-let ((buf (get-buffer bufname)))
    (get-buffer-window buf)))

(cl-defun fc--list-buffer (pred &key (buffers (buffer-list)) one)
  "List BUFFERS tested with pred.
PRED: pred function.
BUFFERS: candidates.
ONE: only request one buffer."
  (cl-loop
   for buf in buffers
   while (or (not one) (not result))
   if (funcall pred buf)
   collect buf into result
   finally return result))

(cl-defun fc--buffer-pred (&key not-file dir regex file-regex modified filter mode var no-current)
  "NOT-FILE: buf is not normal file.
DIR: buf is under this dir.
REGEX: regex for match name of buffer.
FILE-REGEX: regex for match file name of buffer.
MODIFIED: test buf modified state.
FILTER: func for filter.
MODE: specify target major-mode.
VAR: test buffer local var.
NO-CURRENT: not include current buffer in result."
  (when dir
    (setf dir (expand-file-name dir)))

  (lambda (buf)
    (and (or (and not-file
                  (not dir)
                  (not file-regex)
                  (not modified))
             (buffer-file-name buf))
         (or (not not-file)
             (not (buffer-file-name buf)))
         (or (not dir)
             (string-prefix-p dir (buffer-file-name buf)))
         (or (not regex)
             (string-match regex (buffer-name buf)))
         (or (not file-regex)
             (string-match file-regex (buffer-file-name buf)))
         (or (not modified)
             (buffer-modified-p buf))
         (or (not mode)
             (fc-member (buffer-local-value 'major-mode buf) mode))
         (or (not var)
             (buffer-local-value var buf))
         (or (not no-current)
             (not (eq buf (current-buffer))))
         (or (not filter)
             (with-current-buffer buf
               (fc-funcall filter))))))

(cl-defmacro fc-list-buffer (&rest rest)
  `(fc--list-buffer (fc--buffer-pred ,@rest)))

;; select buffers to show
(defun -show-buffers (bufs)
  "Show BUFFERS.
  BUFS: buffer list."
  (let* ((count (length bufs))
         (first-buf (cl-first bufs))
         (size (/ (frame-height) count)))
    (when (or (<= count *fc-buffer-count-threshold*)
              (fc-user-confirm (concat (s-join " "
                                               (mapcar #'buffer-name bufs))
                                       " ")))
      (fc-layout-push)

      (switch-to-buffer first-buf)
      (delete-other-windows)

      (--each (cl-rest bufs)
        (split-window-vertically size)
        (other-window 1)
        (switch-to-buffer it)))))

(defun fc-select-files-to-show (pattern)
  "Select BUFFERS to show.
  PATTERN: buffer name pattern."
  (interactive "MFilename pattern : ")

  (let* ((bufs (fc-list-buffer :file-regex pattern))
         (count (length bufs)))
    (if (> count 0)
        (-show-buffers bufs)
      (message "No file matchs your pattern '%s'" pattern))))

(cl-defun fc-refresh-buffer-content (buffer-or-name del-win &rest rest)
  "Refresh buffer content.
  BUFFER-OR-NAME: buffer or name.
  DEL-WIN: delete the window of buffer.
  REST: new content."
  (let* ((buf (if buffer-or-name
                  (get-buffer-create buffer-or-name)
                (current-buffer)))
         (win (get-buffer-window buf))
         (buflen 0))
    (with-current-buffer buf
      (erase-buffer)

      (--each rest
        (when it
          (insert it)))
      (setq buflen (buffer-size)))

    (when (and win
               del-win
               (zerop buflen))
      (delete-window win))))

(cl-defun fc-pop-buf (buffer-or-name &key mode dir read-only highlight select escape local-vars)
  "Popup buf.
  BUFFER-OR-NAME: buffer or name.
  MODE: specify mode.
  DIR: default-directory.
  READ-ONLY: set buffer to read-only mode.
  HIGHLIGHT: highlight regex.
  SELECT: focus in new window.
  ESCAPE: decode ansi escape sequence.
  LOCAL-VARS: list of local-vars."
  (with-current-buffer buffer-or-name
    (when dir
      (setq-local default-directory dir))

    (when (and (eq mode 'auto) local-vars)
      (save-excursion
        (goto-char (point-min))
        (insert "-*- ")
        (--each local-vars
          (let* ((v (cdr it))
                 (s (if (stringp v)
                        (format "\"%s\"" v)
                      (fc-string v))))
            (insert (car it) ": " s "; ")))
        (insert "-*-\n")))

    (when escape
      (ansi-color-apply-on-region (point-min) (point-max)))

    (cond
     ((eq mode 'auto)
      (normal-mode t))

     (mode
      (fc-funcall mode)))

    (when read-only
      (read-only-mode))

    (--each highlight
      (highlight-regexp it)))

  (display-buffer buffer-or-name *fc-buffer-pop-display-action*)

  (when select
    (select-window (get-buffer-window buffer-or-name))))

;; special buffer
(cl-defun fc-create-side-window (buffer-or-name pos size)
  "Create new side window.
BUFFER-OR-NAME: buffer or name.
POS: side position.
SIZE: width or height depending on POS."
  (let* ((buf (if buffer-or-name
                  (save-window-excursion
                    (get-buffer-create buffer-or-name))
                (current-buffer)))
         (old-win (get-buffer-window buf))
         (win (display-buffer-in-side-window
               buf
               `((side . ,pos) (slot . 0)
                 (window-width . fit-window-to-buffer)
                 (preserve-size . (t . nil))))))
    (delete-window old-win)
    (select-window win)

    (when size
      (cond
       ((member pos '(left right))
        (fc-set-window-width size win))

       (t
        (fc-set-window-height win size))))

    (set-window-parameter win 'no-delete-other-windows t)
    (set-window-dedicated-p win t)))

(cl-defun fc-undo-side-window (&optional (buffer-or-name (current-buffer)))
  (let ((win (get-buffer-window buffer-or-name)))
    (set-window-parameter win 'no-delete-other-windows nil)
    (set-window-dedicated-p win nil)))

(defun fc-one-window-p ()
  (or (one-window-p)
      (eq (window-main-window) (get-buffer-window))))

(provide 'fc-buffer)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-buffer.el ends here
