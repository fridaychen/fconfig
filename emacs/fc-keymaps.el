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

(cl-defun fc-bind-keys (keydefs &optional (keymap (current-global-map)))
  "Bind keys.
KEYDEFS: key binding maps.
KEYMAP: keymap to operate with"
  (fc-each keydefs
    (let* ((key (cl-first it))
           (func (cl-second it))
           (key-def (if (stringp key) (kbd key) key)))
      (define-key keymap key-def func))))

(defun fc-unbind-keys (keydefs &optional keymap)
  "Unbind keys.
KEYDEFS: key list.
KEYMAP: keymap to operate with"
  (fc-each keydefs
    (if keymap
        (define-key keymap (kbd it) nil)
      (global-set-key (kbd it) nil))))

(cl-defmacro fc-cond-key (&key normal region proj one main side work prefix fold dev preregion)
  "Run operation conditionaly.
NORMAL: normal mode.
REGION: region mode.
PROJ: project mode.
ONE: one-window mode.
MAIN: main-window mode.
SIDE: side-window mode.
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

      ((and ,main (eq (window-main-window) (get-buffer-window)))
       (fc-funcall ,main))

      ((and ,side (window-parameter (get-buffer-window) 'window-side))
       (fc-funcall ,side))

      (,fold
       (let ((f (fc-funcall ,fold)))
         (fc-funcall f)))

      (,normal
       (fc-funcall ,normal)))))

(cl-defmacro fc-mode-key (mode-func)
  "Run operation according to the major mode.
MODE-FUNC: mode and function definitions."
  `(lambda ()
     (interactive)

     (cl-block find-mode
       (fc-each ,mode-func
         (let ((mode (car it))
               (func (cdr it)))
           (when (or (and (cl-typep mode 'symbol)
                          (or (eq major-mode mode)
                              (eq mode '_)))
                     (and (cl-typep mode 'list)
                          (member major-mode mode)))
             (fc-funcall func)
             (cl-return-from find-mode)))))))

(cl-defmacro fc-with-keymap (keymap &rest rest)
  (declare (indent 1))
  (let ((ret (make-symbol "ret")))
    `(condition-case ex
         (progn
           (internal-push-keymap (symbol-value ,keymap)
                                 'overriding-terminal-local-map)
           (setq ,ret (progn ,@rest))
           (internal-pop-keymap (symbol-value ,keymap)
                                'overriding-terminal-local-map)
           ,ret)
       ('quit (internal-pop-keymap (symbol-value ,keymap)
                                   'overriding-terminal-local-map)))))

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
     ("7" ,(fc-manual (fc-run-hook '*fc-common-fast-act-hook*)))

     ("B" ,(fc-cond-key :normal 'fc--auto-bookmark
                        :region 'base64-encode-region
                        :preregion 'base64-decode-region))
     ("H" ,(fc-cond-key :normal 'fc-string2hex
                        :region 'fc-hex2string))
     ("T" fc-insert-todo-block)
     ("S" fc-insert-figlet)
     ("V" ,(fc-manual
            (when (and (boundp 'lsp-mode)
                       (lsp-mode))
              (if lsp-semantic-tokens-mode
                  (lsp-semantic-tokens-mode -1)
                (lsp-semantic-tokens-mode 1)))))
     )
   "Basic func map"
   *fc-punctuation-map*))

(cl-defun fc-common-mode-func ()
  (fc-modal-head-key "Common" '*fc-func-mode-map*))

(provide 'fc-keymaps)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-keymaps.el ends here
