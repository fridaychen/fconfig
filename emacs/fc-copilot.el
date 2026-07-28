;;; fc-copilot.el --- DESCRIPTION -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(fc-load 'copilot
  :after (progn
           (add-hook 'prog-mode-hook #'copilot-mode)
           (setf copilot-chat-use-agent-mode t
                 copilot-chat-model "auto"
                 copilot-chat-enable-semantic-search t
                 )

           (fc-add-mode-name 'copilot-chat-mode "‍👩‍✈️"))

  :bind `((*fc-modal-keymap*
           ("s-k" ,(fc-cond-key :normal #'copilot-chat
                                :region #'copilot-chat-send-region))))
  )

(setopt copilot-chat-presets
        '(("fast"  . (:model "gpt-4o" :agent-mode nil))
          ("agent" . (:model "gpt-5-codex" :agent-mode t
                             :auto-approve-tools ("get_errors" "copilot.read_file")))))

(fc-load 'copilot-chat
  :after (progn
           (setf copilot-chat-backend 'curl)

           (add-hook 'copilot-chat-mode-hook #'copilot-chat-add-workspace)
           ))

;; (fc-load 'gh-copilot-chat
;;   :raw https://github.com/chep/gh-copilot-chat.el
;;   :after (progn
;;            (add-hook 'copilot-chat-mode-hook #'gh-copilot-chat-add-workspace)))

(provide 'fc-copilot)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-copilot.el ends here
