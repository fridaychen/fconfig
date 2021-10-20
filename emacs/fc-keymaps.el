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

(cl-defmacro fc-cond-key (&key normal region proj one work prefix fold dev preregion)
  "Run operation conditionaly.
NORMAL: normal mode.
REGION: region mode.
PROJ: project mode.
ONE: one-window mode.
WORK: work on project mode.
PREFIX: fc prefix mode.
FOLD: function return function.
DEV: dev mode.
PREREGION: prefix and region mode"
  `(lambda ()
     (interactive)

     (cond
      ((and ,preregion (region-active-p) *fc-ergo-prefix*)
       (fc-funcall ,preregion))

      ((and ,region (region-active-p))
       (fc-funcall ,region))

      ((and ,prefix *fc-ergo-prefix*)
       (fc-funcall ,prefix))

      ((and *fc-dev-mode* ,dev)
       (fc-funcall ,dev))

      ((and ,work *fc-project*)
       (fc-funcall ,work))

      ((and ,proj (fc-proj-root))
       (fc-funcall ,proj))

      ((and ,one (one-window-p))
       (fc-funcall ,one))

      (,fold
       (let ((f (fc-funcall ,fold)))
         (fc-funcall f)))

      (,normal
       (fc-funcall ,normal)))))

(defmacro fc-mode-key (mode-func)
  "Run operation according to the major mode.
MODE-FUNC: mode and function definitions."
  `(lambda ()
     (interactive)

     (cl-block find-mode
       (--each ,mode-func
         (let ((mode (car it))
               (func (cdr it)))
           (when (or (and (cl-typep mode 'symbol)
                          (or (eq major-mode mode)
                              (eq mode '_)))
                     (and (cl-typep mode 'list)
                          (member major-mode mode)))
             (fc-funcall func)
             (cl-return-from find-mode)))))))

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
     ("7" ,(fc-manual (fc-run-hook '*fc-common-fact-act-hook*)))

     ("H" ,(fc-cond-key :normal 'fc-string2hex
                        :region 'fc-hex2string))
     ("L" org-store-link)
     ("T" fc-insert-todo-block)
     ("S" fc-insert-figlet)
     )
   "Basic func map"
   *fc-punctuation-map*))

(cl-defun fc-common-mode-func ()
  (fc-modal-head-key "Common" '*fc-func-mode-map*))

(defvar *fc-common-fact-act-hook* nil "Common fast action hook.")

(provide 'fc-keymaps)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-keymaps.el ends here
