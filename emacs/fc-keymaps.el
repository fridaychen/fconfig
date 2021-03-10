;;; fc-keymaps.el --- DESCRIPTION -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(cl-defun fc-make-keymap (keydefs &optional (name "new") (parent nil))
  "Create a keymap.
KEYDEFS: default key bindings."
  (let ((keymap (make-sparse-keymap name)))
    (when parent
      (set-keymap-parent keymap parent))

    (fc-bind-keys keydefs keymap)
    keymap))

(defun fc-bind-keys (keydefs &optional keymap)
  "Bind keys.
KEYDEFS: key binding maps.
KEYMAP: keymap to operate with"
  (--each keydefs
    (let* ((key (cl-first it))
           (func (cl-second it))
           (key-def (if (stringp key) (kbd key) key)))
      (if keymap
          (define-key keymap key-def func)
        (global-set-key key-def func)))))

(defun fc-unbind-keys (keydefs &optional keymap)
  "Unbind keys.
KEYDEFS: key list.
KEYMAP: keymap to operate with"
  (--each keydefs
    (if keymap
        (define-key keymap (kbd it) nil)
      (global-set-key (kbd it) nil))))

(defconst *fc-punctuation-map*
  (fc-make-keymap
   `(("`" ,(fc-manual (insert "·")))
     (";" ,(fc-manual (insert "；")))
     ("<" ,(fc-manual (insert "《")))
     (">" ,(fc-manual (insert "》")))
     (":" ,(fc-manual (insert "：")))
     ("^" ,(fc-manual (insert "……")))
     ("." ,(fc-manual (insert "。")))
     ("," ,(fc-manual (insert "，")))
     ("!" ,(fc-manual (insert "！")))
     ("?" ,(fc-manual (insert "？")))
     ("-" ,(fc-manual (insert "——")))
     ("~" ,(fc-manual (insert "～")))
     ("\\" ,(fc-manual (insert "、"))))
   "Punctuation"))

(defconst *fc-func-mode-map*
  (fc-make-keymap
   `(
     ("L" org-store-link)
     ("Q" org-edit-src-exit)
     ("T" fc-insert-todo-block)
     ("S" fc-insert-figlet)
     )
   "Basic func mode"
   *fc-punctuation-map*))

(cl-defun fc-common-mode-func ()
  (fc-modal-head-key "Common" '*fc-func-mode-map*))

(provide 'fc-keymaps)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-keymaps.el ends here
