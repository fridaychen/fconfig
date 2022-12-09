;;; fc-format.el --- DESCRIPTION -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(defconst *fc-fmt* (fc-make-hash-table
                    '(
                      (mhtml-mode ("tidy" "-utf8" "-upper" "-indent" "-quiet") nil)
                      (nxml-mode ("tidy" "-xml" "-utf8" "-indent" "-quiet") nil)
                      (sh-mode ("shfmt" "-ci" "-i" "4") nil))))

(defconst *fc-fmt-verbose* nil)

(defun fc-indent-all ()
  "Indent the whole buffer."
  (interactive)

  (indent-region (point-min) (point-max)))

(defun fc--remove-empty-line ()
  "Remove empty lines."
  (fc-replace-regexp "^\n\\{2,\\}"
                     "\n"
                     :from-start t))

(cl-defun fc--default-fmt ()
  "Default file formatter, clean extra space and emtry line."
  (fc-call-mode-func "whitespace-cleanup" #'whitespace-cleanup)
  (fc-call-mode-func "remove-empty-line" #'fc--remove-empty-line))

(cl-defun fc--default-fmt-with-indent ()
  (fc--default-fmt)
  (fc-indent-all))

(cl-defun fc--run-external-fmt (command-args)
  "Run external tool to format buffer.
COMMAND-ARGS: command args."
  (let* ((outbuf (get-buffer-create "*fc-format*"))
         (errfile (make-temp-file "fc-format-")))

    (when *fc-fmt-verbose*
      (message "run command: [%S] with args: [%S]"
               (car command-args)
               (cdr command-args)))

    (with-current-buffer outbuf
      (erase-buffer))

    (cl-values
     (apply #'call-process-region
            (point-min)
            (point-max)
            (car command-args)
            nil
            (list outbuf errfile)
            command-args)
     (with-current-buffer outbuf
       (buffer-string))
     (with-temp-buffer
       (insert-file-contents errfile)
       (delete-file errfile)
       (buffer-string)))))

(cl-defun fc-fmt-buffer ()
  "Format current buffer."
  (cl-multiple-value-bind (external internal)
      (gethash major-mode *fc-fmt* '(nil fc--default-fmt))

    (when (and (null external)
               (null internal))
      (cl-return-from fc-fmt-buffer))

    (widen)

    (when (and (not (null external))
               (symbolp external))
      (setq external (fc-funcall external)))

    (when external
      (cl-multiple-value-bind (ret output err)
          (save-window-excursion
            (fc--run-external-fmt external))

        (let ((errbuf (get-buffer-create "*fc-format-error*")))
          (if (= ret 0)
              (let ((pos (point)))
                (fc-refresh-buffer-content errbuf t)

                (fc-refresh-buffer-content nil nil output)

                (goto-char pos))

            (fc-refresh-buffer-content errbuf
                                       nil
                                       (when (zerop (length err))
                                         output)
                                       err)
            (fc-pop-buf errbuf)))))

    (when internal
      (save-excursion
        (funcall internal)))))

(cl-defun fc--fmt-hook-func ()
  "Hook function for save."
  (when (and *fc-format-at-save*
             (not (fc-big-buffer-p)))
    (fc-fmt-buffer)))

(cl-defun fc-add-fmt (mode external internal)
  "Add new format for mode.
MODE: 'major-mode.
EXTERNAL: external command.
INTERNAL: internal command."
  (puthash mode (list external internal) *fc-fmt*))

(add-hook 'before-save-hook #'fc--fmt-hook-func)

(provide 'fc-format)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-format.el ends here
