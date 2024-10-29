;;; fc-theme-beautify.el --- Beautify theme -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(defvar *fc-soothe-light-deltas* '((light -0.08 -0.05 -0.02)
                                   (dark 0.06 0.03 0.02)
                                   (deep-dark 0.08 0.05 0.03)))
(defvar *fc-default-face-bg-light* '((light 0.9)
                                     (dark 0.2)
                                     (deep-dark 0.2)))
(defvar *fc-default-face-fg-light* '((light 0.05)
                                     (dark 0.95)
                                     (deep-dark 0.75)))

(defvar *fc-face-contrast-lower-limit* '(0.6 0.56 0.52))

(cl-defun fc--theme-mode ()
  (when (or (and (eq *fc-theme-mode* 'light)
                 (not (fc-dark-theme-p)))
            (xor (eq *fc-theme-mode* 'light)
                 (fc-dark-theme-p)))
    (cl-return-from fc--theme-mode *fc-theme-mode*))

  (if (fc-dark-theme-p)
      'dark
    'light))

(defun fc--beautify-soothe-theme ()
  "Soothe theme.
PERCENT: produce background color by darken this percent.
COLOR: background color."
  (cl-multiple-value-bind (level-1 level-2 level-3)
      (alist-get (fc--theme-mode) *fc-soothe-light-deltas*)

    (fc-each '(font-lock-keyword-face
               font-lock-function-name-face)
      (when (facep it)
        (fc--adjust-face-bg-light it level-1)))

    (fc-each '(font-lock-string-face
               font-lock-type-face
               font-lock-constant-face
               font-lock-property-name-face
               font-lock-variable-name-face

               font-lock-preprocessor-face
               font-lock-function-call-face
               font-lock-variable-use-face
               font-lock-property-use-face)
      (when (facep it)
        (fc--adjust-face-bg-light it level-2)))

    (fc-each '(font-lock-builtin-face
               font-lock-doc-face)
      (when (facep it)
        (fc--adjust-face-bg-light it level-3)))))

(defun fc-beautify-theme-before-theme-changed ()
  (fc-set-faces '(font-lock-keyword-face
                  font-lock-function-name-face

                  font-lock-string-face
                  font-lock-doc-face
                  font-lock-type-face
                  font-lock-constant-face
                  font-lock-property-name-face
                  font-lock-variable-name-face

                  font-lock-builtin-face
                  font-lock-warning-face
                  font-lock-comment-face
                  font-lock-preprocessor-face
                  font-lock-function-call-face
                  font-lock-variable-use-face
                  font-lock-property-use-face)
                :foreground 'unspecified
                :background 'unspecified
                :box 'unspecified
                :underline 'unspecified
                :overline 'unspecified
                :slant 'unspecified))

(defun fc--beautify-face-hl-line ()
  (fc-set-faces '(highlight hl-line)
                :underline 'unspecified))

(defun fc--beautify-face-type ()
  (fc-set-face 'font-lock-type-face nil :slant 'italic))

(defun fc--beautify-face-func-name ()
  (fc-set-face 'font-lock-function-name-face nil :overline t))

(defun fc--beautify-face-mode-line ()
  (fc-set-faces '(mode-line mode-line-active mode-line-inactive)
                :box 'unspecified)
  (fc-set-face 'mode-line-inactive nil
               :foreground (if (fc-dark-face-p 'mode-line-inactive)
                               "cornsilk"
                             "black")))

(defun fc--beautify-theme-whitespace-trailing ()
  (fc--set-face-bg-light 'whitespace-trailing 0.37))

(defun fc--beautify-face-default ()
  (fc--set-face-bg-light 'default
                         (car (alist-get (fc--theme-mode)
                                         *fc-default-face-bg-light*)))
  (fc--set-face-fg-light 'default
                         (car (alist-get (fc--theme-mode)
                                         *fc-default-face-fg-light*))))

(defun fc--do-enhance-contrast (faces threshold)
  (fc-each faces
    (fc--enhance-face-contrast it threshold)))

(defun fc--beautify-enhance-contrast ()
  (cl-multiple-value-bind (level-1 level-2 level-3)
      *fc-face-contrast-lower-limit*

    (fc--do-enhance-contrast '(font-lock-keyword-face
                               font-lock-function-name-face
                               font-lock-warning-face
                               font-lock-builtin-face)
                             level-1)

    (fc--do-enhance-contrast '(font-lock-preprocessor-face
                               font-lock-function-call-face
                               font-lock-variable-use-face
                               font-lock-property-use-face

                               font-lock-string-face
                               font-lock-type-face
                               font-lock-constant-face
                               font-lock-property-name-face
                               font-lock-variable-name-face)
                             level-2)

    (fc--do-enhance-contrast '(font-lock-doc-face
                               font-lock-comment-face)
                             level-3)))

(defun fc--beautify-face-aw-leading ()
  (fc-set-face 'aw-leading-char-face nil
               :height (* *fc-font-height* 2)
               :foreground "red"))

(defvar *fc-beautify-theme-hook* (list #'fc--beautify-face-hl-line
                                       #'fc--beautify-face-type
                                       #'fc--beautify-face-func-name
                                       #'fc--beautify-face-mode-line
                                       #'fc--beautify-theme-whitespace-trailing
                                       #'fc--beautify-face-default
                                       #'fc--beautify-face-aw-leading
                                       #'fc--beautify-soothe-theme
                                       #'fc--beautify-enhance-contrast
                                       ))

(defun fc-beautify-theme ()
  "Beautify theme."
  (fc-funcall #'fc--beautify-user-func)

  (run-hooks '*fc-beautify-theme-hook*))

(defun fc-beautify-theme-before ()
  (fc-set-face 'vertico-current nil
               :foreground 'unspecified
               :background 'unspecified
               :inherit 'highlight
               :extend t)

  (fc-set-faces '(font-lock-keyword-face
                  font-lock-function-name-face

                  font-lock-string-face
                  font-lock-doc-face
                  font-lock-type-face
                  font-lock-constant-face
                  font-lock-property-name-face
                  font-lock-variable-name-face

                  font-lock-builtin-face
                  font-lock-comment-face
                  font-lock-preprocessor-face
                  font-lock-function-call-face
                  font-lock-variable-use-face
                  font-lock-property-use-face)
                :foreground 'unspecified
                :background 'unspecified
                :box 'unspecified
                :underline 'unspecified
                :overline 'unspecified
                :slant 'unspecified))

(provide 'fc-theme-beautify)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-theme-beautify.el ends here
