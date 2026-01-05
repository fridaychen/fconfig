;;; fc-info.el --- infomation -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(defun fc-info--convert (info)
  "Convert info to string.
INFO: info obj."
  (seq-mapcat (lambda (it)
                (concat "│"
                        (fc-text (format "%11s" (cl-first it))
                                 :face 'font-lock-keyword-face)
                        (format " : %s\n" (fc-string (cl-second it)))))
              info
              'string))

(defun fc-info-show (info-seq)
  "Show info.
INFO-SEQ: list of infos."
  (seq-mapcat #'identity
              (cl-loop for x in info-seq
                       collect (when (fboundp x)
                                 (fc-info--convert
                                  (funcall x))))
              'string))

(cl-defun fc-info--file ()
  "Create file info."
  `(("Name" ,buffer-file-name)
    ("Basic" ,(format "%s %d bytes, %d lines, %s, point %d, %d"
                      major-mode
                      (buffer-size)
                      (fc-buffer-lines)
                      buffer-file-coding-system
                      (point)
                      (point-max)))))

(cl-defun fc-info--vc ()
  "VC info."
  `(("VC" ,(if vc-mode
               (fc-text
                (list
                 (fc-vc-branch)
                 (fc-string (when buffer-file-name
                              (vc-state buffer-file-name))))
                :separator ", "
                :face 'highlight)
             "Untracked"))))

(defun fc-info--buffer ()
  "Create buffer info."
  `(("Tag/Xref" ,(format "%s %s"
                         (if (boundp 'fc-proj-tag) fc-proj-tag nil)
                         xref-backend-functions))
    ("Format" ,(format "IndentTab %S [%d], Auto %S, Spacing %d, Scale %3.1f"
                       indent-tabs-mode
                       tab-width
                       *fc-format-at-save*
                       line-spacing
                       text-scale-mode-amount))))

(defun fc-info--sys-gui ()
  "Create sys info."
  (let ((user (format "%s@%s" user-login-name (system-name))))
    `(
      ("Emacs" ,(format "%s (%s), DPI %d, fringe %d"
                        emacs-version
                        (format-time-string "%Y-%m-%d" emacs-build-time)
                        (fc-display-ppi) *fc-fringe-width*))
      ("User" ,(format "%s, %s" user *fc-location*))
      ("Font" ,(format "%s, %s, %d"
                       *fc-default-font*
                       *fc-font-weight-of-default*
                       *fc-font-height*))
      ("Theme" ,(format "%s, %s, fg %s, bg %s"
                        *fc-current-theme*
                        (if (fboundp 'fc-modeline-mode)
                            "fc-modeline"
                          (symbol-name powerline-default-separator))
                        (fc-get-face 'default :foreground)
                        (fc-get-face 'default :background))))))

(defun fc-info--sys-txt ()
  "Create sys info."
  (let ((user (format "%s@%s" user-login-name (system-name))))
    `(
      ("Emacs" ,(format "%s, colorful %S" emacs-version *is-colorful*))
      ("User" ,user)
      ("Loc" ,*fc-location*)
      ("Theme" ,*fc-current-theme*))))

(defun fc-info--process ()
  "Return list of process info."
  `(("Process"
     ,(string-join (cl-loop for i in (and (fboundp 'process-list)
                                          (process-list))
                            for j from 1
                            collect (format "[%d] %s" j (process-name i)))
                   ", "))))

(defun fc-info--battery ()
  "Return list of battery info."
  `(("Battery"
     ,(string-join (cl-loop for (key value) on (fc-app-get-power-info) by #'cddr
                            collect (format "%s=[%s]" key value))
                   ", "))))

(add-to-list '*fc-info-buffer* #'fc-info--file t)
(add-to-list '*fc-info-buffer* #'fc-info--buffer t)
(add-to-list '*fc-info-buffer* #'fc-info--vc t)

(add-to-list '*fc-info-system*
             (if *is-gui*
                 #'fc-info--sys-gui
               #'fc-info--sys-txt)
             t)

(when *is-linux*
  (add-to-list '*fc-info-system* #'fc-info--battery t))
(add-to-list '*fc-info-system* #'fc-info--process t)

(provide 'fc-info)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-info.el ends here
