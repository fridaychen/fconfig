;;; fc-buffer.el --- buffer management -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(defvar *fc-buffer-count-threshold* 4)

;; buffer extension
(cl-defun fc-rm-current-buf (buffers)
  "Remove current buffer from list buffers.
BUFFERS: list of buffer."
  (let ((current (current-buffer)))
    (--remove (eq current it) buffers)))

(cl-defun fc-list-buffer (&key not-file dir regex file-regex sort modified filter)
  "List buffer accroding the arguments.
NOT-FILE: buf is not normal file.
DIR: buf is under this dir.
REGEX: regex for match name of buffer.
FILE-REGEX: regex for match file name of buffer.
SORT: sort the result.
MODIFIED: test buf modified state.
FILTER: func for filter."
  (let* ((t-dir (if dir (expand-file-name dir) nil))
         (result))
    (setf result
          (--filter (and (or (and not-file
                                  (not dir)
                                  (not file-regex)
                                  (not modified))
                             (buffer-file-name it))
                         (or (not not-file)
                             (not (buffer-file-name it)))
                         (or (not dir)
                             (string-prefix-p t-dir (buffer-file-name it)))
                         (or (not regex)
                             (string-match regex (buffer-name it)))
                         (or (not file-regex)
                             (string-match file-regex (buffer-file-name it)))
                         (or (not modified)
                             (buffer-modified-p it))
                         (or (not filter)
                             (with-current-buffer it
                               (fc-funcall filter))))
                    (buffer-list)))

    (if sort
        (--sort (string< (buffer-name it) (buffer-name other)) result)
      result)))

(cl-defun fc-switch-to-buffer-re (regex &optional (n 0))
  "Switch to recent buffer which name match regex.
REGEX: regex.
N: nth."
  (switch-to-buffer
   (cl-nth-value n
                 (fc-list-buffer :file-regex regex))))

(cl-defun fc-switch-to-recent-buffer ()
  "Create a window and show a recent buf which is not showed."
  (let ((buf (--first (and (buffer-file-name it)
                           (null (get-buffer-window it)))
                      (cdr (buffer-list)))))
    (when buf
      (switch-to-buffer buf))))

(cl-defun fc-switch-to-buffer (prompt buffers &key root pop error-msg)
  "Select buffer to switch.
PROMPT: prompt string.
BUFFERS: buffer list for selection.
ROOT: root directory for showing file path.
POP: show the selected buffer side-by-side.
ERROR-MSG: error message."
  (unless buffers
    (let ((msg (or error-msg "Buffer list is empty !!!")))
      (message msg)
      (cl-return-from fc-switch-to-buffer msg)))

  (let ((buf (fc-user-select prompt
                             (--map (cons (if root
                                              (file-relative-name (buffer-file-name it)
                                                                  root)
                                            (buffer-name it))
                                          it)
                                    buffers))))
    (if pop
        (fc-pop-buf buf)
      (switch-to-buffer buf))))

;; select buffers to show
(defun -show-buffers (bufs)
  "Show buffers.
BUFS: buffer list."
  (let* ((count (length bufs))
         (first-buf (cl-first bufs))
         (size (/ (frame-height) count)))
    (when (or (<= count *fc-buffer-count-threshold*)
              (fc-user-confirm (concat (s-join " "
                                               (-map #'buffer-name bufs))
                                       " ")))
      (fc-layout-push)

      (switch-to-buffer first-buf)
      (delete-other-windows)

      (--each (cl-rest bufs)
        (split-window-vertically size)
        (other-window 1)
        (switch-to-buffer it)))))

(defun fc-select-files-to-show (pattern)
  "Select buffers to show.
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
DEL-WIN: if delete the window of buffer.
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

(cl-defun fc-pop-buf (buffer-or-name &key automode read-only highlight select)
  "Popup buf.
BUFFER-OR-NAME: buffer or name.
AUTOMODE: if run 'normal-mode'.
READ-ONLY: set buffer to read-only mode.
HIGHLIGHT: highlight regex.
SELECT: focus in new window."
  (display-buffer buffer-or-name 'display-buffer-pop-up-window)

  (with-current-buffer buffer-or-name
    (when automode
      (normal-mode))

    (when read-only
      (read-only-mode))

    (--each highlight
      (highlight-regexp it)))

  (when select
    (select-window (get-buffer-window buffer-or-name))))

;; favorite buffers
(let ((favorite-buffers nil))
  (cl-defun fc-add-remove-favorite-buffer (&optional (buffer (current-buffer)))
    "Add or remove current buffer to/from favoriate buffer list.
BUFFER: buuffer to add."
    (if (member buffer favorite-buffers)
        (progn
          (setf favorite-buffers
                (remove buffer favorite-buffers))
          (message "Remove from favorite buffer list"))
      (progn
        (add-to-list 'favorite-buffers buffer)
        (message "Add to favorite buffer list"))))

  (cl-defun fc-goto-favorite-buffer ()
    "Show favorite buffer."
    (setf favorite-buffers
          (-filter #'buffer-live-p
                   favorite-buffers))
    (fc-switch-to-buffer "Favorite buffer"
                         (-remove #'get-buffer-window-list
                                  favorite-buffers))))

;; project buffer
(defun fc-switch-within-project ()
  "Switch buffers within same project."
  (interactive)

  (let ((root (fc-proj-root)))
    (fc-switch-to-buffer "Switch within project"
                         (fc-rm-current-buf
                          (fc-list-buffer :dir root :sort t))
                         :root root)))

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
        (fc-set-window-width :window win :width size))

       (t
        (fc-set-window-height :window win :height size))))

    (set-window-parameter win 'no-delete-other-windows t)
    (set-window-dedicated-p win t)))

(cl-defun fc-undo-side-window (&optional (buffer-or-name (current-buffer)))
  (let ((win (get-buffer-window buffer-or-name)))
    (set-window-parameter win 'no-delete-other-windows nil)
    (set-window-dedicated-p win nil)))

(provide 'fc-buffer)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-buffer.el ends here
