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

(cl-defun fc-vertico-bookmark ()
  (interactive)

  (fc-bind-keys `(("M-d" ,(fc-manual
                           (let* ((name (seq-elt vertico--candidates
                                                 vertico--index)))
                             (when (fc-user-confirm (format "Delete bookmark %s" name))
                               (bookmark-delete name)))

                           (setq vertico--candidates
                                 (seq-remove-at-position vertico--candidates
                                                         vertico--index)))))
                vertico-map)

  (when-let ((name (completing-read "Bookmarks"
                                    (-map #'car (bookmark-maybe-sort-alist)))))
    (cond
     ((member name (bookmark-all-names))
      (bookmark-jump name))

     (t
      (bookmark-set name)))))

(cl-defun fc-show-buffer ()
  (when-let* ((bufname (completing-read "Switch to buffer: "
                                        #'internal-complete-buffer
                                        :keymap ivy-switch-buffer-map
                                        :preselect (buffer-name (other-buffer (current-buffer)))
                                        :matcher #'ivy--switch-buffer-matcher
                                        :caller 'ivy-switch-buffer))
              (buf (get-buffer bufname))
              (win (progn
                     (when (fc-side-window-p)
                       (select-window (or (window-child (window-main-window))
                                          (window-main-window))))
                     (display-buffer buf
                                     '(display-buffer-same-window
                                       display-buffer-use-some-window
                                       display-buffer-reuse-window)))))
    (select-window win)))

(provide 'fc-vertico)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-vertico.el ends here
