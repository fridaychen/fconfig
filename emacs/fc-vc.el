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

           (fc-add-to-hook '*fc-ergo-restore-hook* #'magit-blame-quit)
           (setf magit-diff-arguments "-w")))

(fc-install 'git-messenger 'git-timemachine 'git-lens)

(fc-load 'vc-git
  :local t
  :after (progn
           (setf vc-git-diff-switches "-w"
                 ;; disable vc backends, because of performance problem
                 vc-handled-backends '(Git))

           (defun fc--vc-git-log-view-mode-func ()
             "Mode func."
             (fc-modal-head-key "Git Log" 'vc-git-log-view-mode-map))))

(cl-defun fc--run-git-command (&rest args)
  (apply #'fc-exec-command-to-string "git" args))

(cl-defun fc-git-add ()
  (interactive)

  (when buffer-file-name
    (save-buffer)
    (fc--run-git-command "add" buffer-file-name)))

(cl-defun fc-git-amend (mesg)
  (interactive "MNew message : ")

  (if (string-empty-p mesg)
      (fc--run-git-command "commit" "--amend" "--no-edit")
    (fc--run-git-command "commit" "--amend" "-m" mesg)))

(cl-defun fc-git-cancel-last-commit ()
  (fc--run-git-command "reset" "@~"))

(cl-defun fc-git-search (target)
  (interactive "MSearch : ")

  (fc--run-git-command "log" "--grep" target))

(cl-defun fc-git-commit (msg)
  (interactive "MCommit message : ")

  (unless (fc-void-p msg)
    (shell-command-to-string
     (concat
      "fit -c "
      (shell-quote-argument msg)))))

(cl-defun fc-git-pull ()
  "Pull current repo."
  (message "Git pulling ...")
  (if (and (boundp 'fc-proj-work)
           (not (fc-location-p 'work)))
      (shell-command (format
                      "git pull remote %s"
                      (magit-get-current-branch)))
    (shell-command "git pull")))

(cl-defun fc-git-push ()
  "Push current repo."
  (message "Git pushing ...")
  (if (and (boundp 'fc-proj-work)
           (not (fc-location-p 'work)))
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

    (fc-pop-buf buf :read-only t :escape t)))

(cl-defun fc-vc-rename-file ()
  (interactive)

  (let* ((old (file-relative-name buffer-file-name (fc-vc-root)))
         (new (read-string "New file name : " old)))
    (unless (equal old new)
      (vc-rename-file old new))))

(cl-defun fc-vc-refresh-repo-state (&rest _rest)
  "Refresh vc state for all files in current repo."
  (fc-with-each-buffer
   :buffers (fc-list-buffer :dir (fc-vc-root))
   (vc-refresh-state)))

(cl-defun fc-vc-revert-repo ()
  (fc-with-each-buffer
   :buffers (fc-list-buffer :dir (fc-vc-root))
   (revert-buffer t t)))

(cl-defun fc-vc-root (&optional (dir (expand-file-name default-directory)))
  "Find repo directory of current buffer."
  (unless dir
    (cl-return-from fc-vc-root nil))

  (fc-locate-file-in-path '(".git" ".svn") dir))

(cl-defun fc-vc-branch ()
  "Return current vc branch."
  (when vc-mode
    (let* ((backend (symbol-name (vc-backend (buffer-file-name))))
           (branch (substring-no-properties vc-mode (+ (length backend) 2))))
      branch)))

(cl-defun fc-git-current-branch ()
  "Return current git branch name."
  (s-trim
   (fc-exec-command-to-string "git" "rev-parse" "--abbrev-ref" "HEAD")))

(cl-defun fc-vc-select-branch (&optional remote)
  "Select git branch.
REMOTE: select from local or remote branchs."
  (fc-user-select (format "%s (current: %s)"
                          (if remote "Remote branch" "Branch")
                          (fc-git-current-branch))
                  (split-string
                   (with-temp-buffer
                     (shell-command (format "git branch %s | sed -e \"/^\\*/d\" | cut -b 3-"
                                            (if remote "-r" ""))
                                    (current-buffer))
                     (s-trim (buffer-string)))
                   "\n")))

(cl-defun fc-vc-switch-branch ()
  "Switch to other branch."
  (when-let ((branch (fc-vc-select-branch)))
    (shell-command (format "git checkout %s" branch))))

(cl-defun fc-vc-diff-with-other-branch ()
  "Diff work dir with other branch."
  (interactive)

  (let* ((branch (fc-vc-select-branch))
         (buf (get-buffer-create (format "*git-diff with branch %s*" branch))))
    (with-current-buffer buf
      (erase-buffer)

      (shell-command (format "git diff %s" branch) (current-buffer)))

    (fc-pop-buf buf :automode t :select t :local-vars `(("mode" . diff)
                                                        ("default-directory" . ,(fc-vc-root))))))

(cl-defun fc-vc-diff-file-with-other-branch ()
  "Diff current file with other branch."
  (interactive)

  (let* ((branch (fc-vc-select-branch))
         (filename buffer-file-name)
         (buf (get-buffer-create (format "*git-diff %s with branch %s*" (buffer-name) branch))))
    (with-current-buffer buf
      (erase-buffer)
      (shell-command (format "git diff %s -- %s" branch filename) (current-buffer)))

    (fc-pop-buf buf :automode t :select t :local-vars `(("mode" . diff)
                                                        ("default-directory" . ,(fc-vc-root))))))

(fc-add-network-advice 'fc-git-pull 'fc-git-push)

(--each '(fc-git-commit)
  (advice-add it :after #'fc-vc-refresh-repo-state))

(provide 'fc-vc)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-vc.el ends here
