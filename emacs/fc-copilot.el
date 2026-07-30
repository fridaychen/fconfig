;;; fc-copilot.el --- DESCRIPTION -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(fc-load 'copilot
  :after (progn
           (cl-defun fc-run-copilot (prompt)
             (interactive "MPrompt : ")

             (let ((result-buf (get-buffer-create "*fc-copilot*")))
               (with-current-buffer result-buf
                 (setq-local default-directory (fc-proj-root))

                 (fc-async-exec-command-to-buffer
                  (current-buffer)
                  #'(lambda (process event)
                      (fc-pop-buf result-buf :mode 'markdown-ts-mode :escape t))
                  "copilot"
                  "--no-color"
                  "--continue"
                  "-p"
                  prompt
                  "--allow-all"))))

           (add-hook 'prog-mode-hook #'copilot-mode)
           (setf copilot-chat-use-agent-mode t
                 copilot-chat-model "auto"
                 copilot-chat-enable-semantic-search t
                 )

           (fc-add-mode-name 'copilot-chat-mode "‍👩‍✈️"))

  :bind `((*fc-modal-keymap*
           ("s-k" ,(fc-cond-key :normal #'copilot-chat
                                :region #'copilot-chat-send-region))
           ("s-j" ,(fc-cond-key :normal #'fc-run-copilot)))))

(setopt copilot-chat-presets
        '(("fast"  . (:model "gpt-4o" :agent-mode nil))
          ("agent" . (:model "gpt-5-codex" :agent-mode t
                             :auto-approve-tools ("get_errors" "copilot.read_file")))))

(fc-load 'copilot-chat
  :after (progn
           (setf copilot-chat-backend 'curl)

           (add-hook 'copilot-chat-mode-hook #'copilot-chat-add-workspace)
           ))

(provide 'fc-copilot)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-copilot.el ends here
