;;; fc-git.el --- setup git -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(fc-load 'magit
  :autoload t
  :after (progn
           (fc-modal-exclude-mode 'magit-status-mode
                                  'magit-popup-mode)

           (magit-auto-revert-mode 0)
           (fullframe magit-status magit-mode-quit-window)

           (setf magit-diff-arguments "-w")))

(fc-install 'git-messenger 'git-timemachine 'git-lens)

(setf vc-git-diff-switches "-w"
      ;; disable vc backends, because of performance problem
      vc-handled-backends '(Git))

(cl-defun fc--run-git-command (&rest args)
  (apply #'fc-exec-command-to-string "git" args))

(cl-defun fc-git-add ()
  (interactive)

  (when buffer-file-name
    (save-buffer)
    (fc--run-git-command "add" buffer-file-name)))

(cl-defun fc-git-amend (mesg)
  (interactive "MNew message : ")

  (fc--run-git-command "commit" "--amend" "-m" mesg))

(cl-defun fc-git-cancel-last-commit ()
  (fc--run-git-command "reset" "@~"))

(cl-defun fc-git-search (target)
  (interactive "MSearch : ")

  (fc--run-git-command "log" "--grep" target))

;;(remove-hook 'find-file-hook #'vc-refresh-state)

(cl-defun fc-git-commit (msg)
  (interactive "MCommit message : ")

  (unless (fc-void-p msg)
    (shell-command-to-string
     (concat
      "fit -c "
      (shell-quote-argument msg)))))

(cl-defun fc-git-pull ()
  (unless (fc-network-connected-p)
    (message "No network connection !")
    (cl-return-from fc-git-pull))

  (message "Git pulling ...")
  (if (and (boundp 'fc-proj-work)
           (not *fc-location-work*))
      (shell-command (format
                      "git pull remote %s"
                      (magit-get-current-branch)))
    (shell-command "git pull")))

(cl-defun fc-git-push ()
  (unless (fc-network-connected-p)
    (message "No network connection !")
    (cl-return-from fc-git-push))

  (message "Git pushing ...")
  (if (and (boundp 'fc-proj-work)
           (not *fc-location-work*))
      (shell-command (format
                      "git push remote %s"
                      (magit-get-current-branch)))
    (shell-command "git push")))

(cl-defun fc-git-diff-repo ()
  (interactive)

  (let ((filename (file-name-nondirectory buffer-file-name))
        (buf "*fit diff*"))
    (fc-exec-command-to-buffer buf "fit" "-s" "-v")

    (with-current-buffer buf
      (goto-char (point-min))
      (search-forward filename nil t)
      (diff-mode))

    (fc-pop-buf buf :read-only t)))

(cl-defun fc-vc-rename-file ()
  (interactive)

  (let* ((old (file-relative-name buffer-file-name (fc-proj-root)))
         (new (read-string "New file name : " old)))
    (unless (equal old new)
      (vc-rename-file old new))))

(provide 'fc-vc)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-vc.el ends here
