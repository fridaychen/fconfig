;;; fc-gitignore-mode.el --- DESCRIPTION -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'conf-mode)

(defvar fgitignore-mode-font-lock-keywords
  '(("^\\s<.*$"   . font-lock-comment-face)
    ("^!"         . font-lock-negation-char-face) ; Negative pattern
    ("/"          . font-lock-constant-face)      ; Directory separators
    ("[*?]"       . font-lock-keyword-face)       ; Glob patterns
    ("\\[.+?\\]"  . font-lock-keyword-face)))     ; Ranged glob patterns

(define-derived-mode fgitignore-mode conf-unix-mode "Fgitignore"
  "A major mode for editing .gitignore files."
  (conf-mode-initialize "#")
  ;; Disable syntactic font locking, because comments are only valid at
  ;; beginning of line.
  (setq font-lock-defaults '(fgitignore-mode-font-lock-keywords t t))
  (set (make-local-variable 'conf-assignment-sign) nil))

(cl-defun fc-gitignore-exclude-subdir (subdir)
  (interactive "sExclude dir: ")

  (insert "# allow " subdir "\n")

  (let ((path ""))
    (--each (split-string subdir "/")
      (setq path (concat path "/" it))
      (insert (concat "!" path "\n")
              (concat path "/*\n"))))
  (kill-line -1))

(defconst *fc-gitignore-map*
  (fc-make-keymap
   `(("e" fc-gitignore-exclude-subdir)
     )
   "fc-gitignore-map"
   *fc-func-mode-map*)
  "KEYS e: exclude subdir.")

(defun fc--fgitignore-mode-func ()
  "Mode func."
  (fc-modal-head-key "Fgitignore" '*fc-gitignore-map*))

(provide 'fc-gitignore-mode)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-gitignore-mode.el ends here
