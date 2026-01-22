;;; fc-vertico.el --- Vertico -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(defconst *fc-vertico--base-keymap*
  (fc-make-keymap
   `(
     ("s-j" vertico-next)
     ("s-k" vertico-previous)
     ("C-j" vertico-next)
     ("C-k" vertico-previous))
   "fc vertico base"))

(defun fc-vertico--clear ()
  "Clear current line."
  (interactive)

  (when (not (zerop (current-column)))
    (beginning-of-line-text)
    (kill-line)
    (vertico--update)))

(fc-load 'vertico
  :after (progn
           (setf consult-project-function #'fc-proj-root
                 vertico-sort-function nil)

           (vertico-mode 1)
           (vertico-mouse-mode 1))

  :bind '((vertico-map
           ("M-d" fc-vertico--clear)
           ("s-j" vertico-next)
           ("s-k" vertico-previous)
           ("C-j" vertico-next)
           ("C-k" vertico-previous)
           ("M-i" vertico-previous)
           ("M-k" vertico-next)
           ("M-j" vertico-scroll-down)
           ("M-l" vertico-scroll-up))))

(fc-load 'vertico-posframe
  :after (progn
           (cl-defun fc-vertico--posframe-theme-changed ()
             (fc-set-face 'vertico-posframe-border nil
                          :background (if (fc-dark-theme-p)
                                          "#FEBA07"
                                        "SkyBlue3"))

             (setq-default vertico-posframe-border-width 6))

           (fc-vertico--posframe-theme-changed)

           (setf vertico-count 20)

           (vertico-posframe-mode 1)))

(fc-load 'consult
  :after (progn
           (add-hook 'consult-after-jump-hook #'fc--show-recenter-block t)))

(fc-load 'orderless
  :after (progn
           (setq completion-styles '(orderless partial-completion)
                 completion-category-defaults nil
                 completion-category-overrides '((file (styles . (partial-completion)))))))

(defalias 'fc-bookmark 'fc-vertico-bookmark)
(defalias 'fc-recentf 'recentf-open)
(defalias 'fc-buffers-list 'consult-buffer)
(defalias 'fc-imenu 'imenu)
(defalias 'fc-yank-pop 'yank-pop)
(defalias 'fc-find-files 'find-file)
(defalias 'fc-M-x 'execute-extended-command)
(defalias 'fc-outline 'consult-outline)
(defalias 'fc-flycheck 'flycheck-list-errors)

(defun fc-vertico--delete-bookmark ()
  "Delete bookmark."
  (interactive)

  (when-let* ((name (seq-elt vertico--candidates
                             vertico--index))
              (confirm (fc-user-confirm (format "Delete bookmark %s" name))))
    (bookmark-delete name)

    (setq vertico--candidates
          (seq-remove-at-position vertico--candidates
                                  vertico--index))))

(defconst *fc-vertico--bookmark-keymap*
  (fc-make-keymap
   `(
     ("M-d" fc-vertico--delete-bookmark))
   "fc vertico bookmap"
   *fc-vertico--base-keymap*))

(cl-defun fc-vertico--read (prompt collection &key (keymap '*fc-vertico--base-keymap*))
  (fc-with-keymap keymap
    (completing-read prompt collection)))

(defun fc-vertico-bookmark ()
  "Jump and change."
  (interactive)

  (bookmark-maybe-load-default-file)

  (when-let* ((name (fc-vertico--read
                     "Bookmarks"
                     (mapcar #'car (bookmark-maybe-sort-alist))
                     :keymap '*fc-vertico--bookmark-keymap*)))
    (cond
     ((member name (bookmark-all-names))
      (bookmark-jump name))

     (t
      (bookmark-set name)))))

(provide 'fc-vertico)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-vertico.el ends here
