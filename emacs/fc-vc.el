;;; fc-git.el --- setup git -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(fc-load 'magit
  :autoload t
  :after (progn
           (fc-add-mode-name 'magit-diff-mode "2️⃣")
           (fc-add-mode-name 'magit-log-mode "🪵")
           (fc-add-mode-name 'magit-revision-mode "2️⃣")
           (fc-add-mode-name 'magit-status-mode "🍱")

           (fc-modal-exclude-mode 'magit-status-mode
                                  'magit-popup-mode)

           (magit-auto-revert-mode 0)
           (fullframe magit-status magit-mode-quit-window)

           (fc-add-to-hook '*fc-ergo-restore-hook* #'magit-blame-quit)
           (setf magit-diff-arguments "-w")))

(fc-install 'git-timemachine 'git-lens)

(fc-load 'git-messenger
  :after (progn
           (defun fc-git-messenger-cb (arg)
             (fc-with-existing-buffer "*git-messenger*"
               (diff-mode)))

           (add-hook 'git-messenger:after-popup-hook #'fc-git-messenger-cb)))

(add-to-list 'auto-mode-alist '("\\*git-messenger\\*" . diff-mode))

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

(cl-defun fc-git-amend (auto-add mesg)
  (interactive (let* ((y (fc-user-confirm "Auto-add"))
                      (m (read-string "New message")))
                 (list y m)))

  (when auto-add
    (fc--run-git-command "add" "-u"))

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
      (search-forward filename nil t))

    (fc-pop-buf buf :read-only t :escape t :mode 'diff-mode)))

(cl-defun fc-git-rename-branch ()
  (interactive)

  (when-let ((name (read-string "New name for current branch")))
    (magit-branch-rename (fc-vc-branch) name)))

(cl-defun fc-git-delete-branch ()
  (interactive)

  (when-let ((branch (fc-vc-select-branch)))
    (magit-branch-delete (list branch))))

(fc-load 'diff-hl
  :after (progn
           (setf diff-hl-update-async t)
           (add-to-list 'diff-hl-global-modes 'org-mode)
           (add-to-list 'diff-hl-global-modes 'markdown-mode)

           (global-diff-hl-mode 1)
           (diff-hl-flydiff-mode 1)

           (unless *is-gui*
             (setq diff-hl-side 'left)
             (diff-hl-margin-mode 1))

           (fc-bind-keys
            '(("[" diff-hl-previous-hunk)
              ("]" diff-hl-next-hunk))
            *ergo-vc-map*)))

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
  (string-trim
   (fc-exec-command-to-string "git" "rev-parse" "--abbrev-ref" "HEAD")))

(cl-defun fc-vc-select-branch (&optional remote)
  "Select git branch.
REMOTE: select from local or remote branchs."
  (fc-select
      (format "%s (current: %s)"
              (if remote "Remote branch" "Branch")
              (fc-git-current-branch))
      (split-string
       (with-temp-buffer
         (shell-command (format "git branch %s | sed -e \"/^\\*/d\" | cut -b 3-"
                                (if remote "-r" ""))
                        (current-buffer))
         (string-trim (buffer-string)))
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

    (fc-pop-buf buf :mode 'diff-mode :select t :dir (fc-vc-root))))

(cl-defun fc-vc-diff-file-with-other-branch ()
  "Diff current file with other branch."
  (interactive)

  (let* ((branch (fc-vc-select-branch))
         (filename buffer-file-name)
         (buf (get-buffer-create (format "*git-diff %s with branch %s*" (buffer-name) branch))))
    (with-current-buffer buf
      (erase-buffer)
      (shell-command (format "git diff %s -- %s" branch filename) (current-buffer)))

    (fc-pop-buf buf :mode 'diff-mode :select t :dir (fc-vc-root))))

(cl-defun fc-vc-show-commit-blame-with-line ()
  (interactive)

  (let ((buf (get-buffer-create "*git-messenger*")))
    (fc-exec-command-to-buffer
     buf
     "fit" "-S" "-L" (fc-string (fc-line-num)) buffer-file-name)

    (fc-pop-buf buf :mode 'diff-mode :select t)
    (with-current-buffer buf
      (goto-char (point-min)))))

(fc-add-network-advice 'fc-git-pull 'fc-git-push)

(fc-each '(fc-git-commit)
  (advice-add it :after #'fc-vc-refresh-repo-state))

(provide 'fc-vc)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-vc.el ends here
