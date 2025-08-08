;;; fc-dired.el --- setup dired-mode -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(fc-load 'dired
  :local t

  :after (progn
           ;; enable omit submode
           (add-hook 'dired-mode-hook #'dired-omit-mode)

           (setf dired-dwim-target t
                 dired-recursive-copies 'always
                 dired-recursive-deletes 'top
                 global-auto-revert-non-file-buffers t
                 auto-revert-verbose nil
                 dired-omit-verbose nil
                 dired-compress-file-default-suffix ".bz2"
                 dired-listing-switches "-aBhl --group-directories-first")

           (when *is-mac*
             (setf insert-directory-program "gls"))

           (defun fc-dired-sys-open ()
             (interactive)

             (apply #'fc-exec-command "fj" "--open" (dired-get-marked-files)))

           (defun fc-dired-preview-file ()
             (interactive)

             (apply #'fc-exec-command "fj" "--preview" (dired-get-marked-files)))

           (defun fc-dired--marked-p ()
             (fc-do-looking-at "\\*" :line t
                               t))

           (defun fc-dired-toggle-mark ()
             (interactive)

             (if (fc-dired--marked-p)
                 (fc-funcall #'dired-unmark)
               (fc-funcall #'dired-mark)))

           (defun fc-dired-sort ()
             (interactive)

             (dired-sort-other (pcase (fc-select "Sort by"
                                          '("date" "size" "name" "folder"))
                                 ("name" "-aBhl ")
                                 ("date" "-aBhl -t")
                                 ("size" "-aBhl -S")
                                 ("folder" "-aBhl --group-directories-first")))))

  :bind `((dired-mode-map
           ("4" ,(fc-cond-key :normal 'fc-switch-to-buffer))
           ("9" ,(fc-manual (kill-new (dired-get-filename))))
           ("c" dired-do-compress)
           ("d" dired-do-delete)
           ("f" fc-basic-key)
           ("i" previous-line)
           ("k" next-line)
           ("m" fc-dired-toggle-mark)
           ("o" fc-dired-sys-open)
           ("r" dired-do-find-regexp-and-replace)
           ("s" fc-dired-sort)
           ("p" fc-dired-preview-file)
           ("w" ,(fc-manuals #'wdired-change-to-wdired-mode
                             #'fc-modal-enable))
           ("D" fc-kill-current-buffer)
           ("N" make-directory)
           (";" fc-fast-switch-window)
           ("C-o" fc-modal-run)
           ("<tab>" ,(fc-manuals
                      #'dired-toggle-read-only
                      #'fc-modal-enable)))))

(require 'dired-x)

(defun fc-file-no-zh-to-number ()
  (interactive)

  (fc-replace-regexp
   "\\([零一二两三四五六七八九十百千万]+\\)\\. "
   #'(lambda ()
       (replace-match
        (concat
         (number-to-string
          (fc-zh-to-number (match-string 1)))
         ". ")))
   :from-start t))

(defconst *fc-wdired-map*
  (fc-make-keymap
   `(
     ("f" fc-file-no-zh-to-number)
     )))

(cl-defun fc--wdired-mode-func ()
  "Mode func."
  (fc-modal-head-key "Wdired" '*fc-wdired-map*))

(fc-load 'wdired
  :autoload t
  :bind '((wdired-mode-map
           ("<tab>" wdired-exit))))

(fc-load 'diredfl
  :idle t
  :after (diredfl-global-mode))

(provide 'fc-dired)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-dired.el ends here
