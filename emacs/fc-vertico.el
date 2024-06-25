;;; fc-vertico.el --- DESCRIPTION -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(fc-load 'vertico
  :after (progn
           (ivy-mode -1)
           (vertico-mode 1))

  :bind '((vertico-map
           ("C-j" vertico-next)
           ("C-k" vertico-previous)
           ("M-i" vertico-previous)
           ("M-k" vertico-next)
           ("M-j" vertico-scroll-down)
           ("M-l" vertico-scroll-up))))

(fc-load 'vertico-posframe
  :after (progn
           (fc-set-face 'vertico-posframe-border nil
                        :background
                        (if (fc-dark-theme-p)
                            "#FEBA07"
                          "SkyBlue3"))
           (setq-default vertico-posframe-border-width 6)
           (vertico-posframe-mode 1)))

(fc-load 'consult)

(fc-load 'orderless
  :after (progn
           (setq completion-styles '(orderless basic))))

(defalias 'fc-bookmark 'fc-vertico-bookmark)
(defalias 'fc-show-buffer 'consult-buffer)
(defalias 'fc-recentf 'consult-recent-file)
(defalias 'fc-buffers-list 'consult-buffer)
(defalias 'fc-imenu 'consult-imenu)
(defalias 'fc-yank-pop 'consult-yank-pop)
(defalias 'fc-find-files 'find-file)
(defalias 'fc-M-x 'execute-extended-command)
(defalias 'fc-outline 'consult-outline)

(cl-defun fc--vertico--delete-bookmark ()
  (interactive)

  (when-let* ((name (seq-elt vertico--candidates
                             vertico--index))
              (confirm (fc-user-confirm (format "Delete bookmark %s" name))))
    (bookmark-delete name)

    (setq vertico--candidates
          (seq-remove-at-position vertico--candidates
                                  vertico--index))))

(cl-defun fc-vertico-bookmark ()
  (interactive)

  (fc-bind-keys `(("M-d" fc--vertico--delete-bookmark))
                vertico-map)

  (when-let ((name (completing-read
                    "Bookmarks"
                    (-map #'car (bookmark-maybe-sort-alist)))))
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
