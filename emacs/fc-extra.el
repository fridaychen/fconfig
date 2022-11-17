;;; fc-extra.el --- extra code -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(fc-load '2048-game
  :after
  (progn
    (advice-add #'2048-init :after #'(lambda () (text-scale-set 3)))
    (fc-modal-exclude-mode '2048-mode))
  :bind '((2048-mode-map
           ("i" 2048-up)
           ("j" 2048-left)
           ("k" 2048-down)
           ("l" 2048-right))))

(fc-load 'tetris
  :local t
  :after (fc-modal-exclude-mode 'tetris-mode)
  :bind '((tetris-mode-map
           ("i" tetris-rotate-prev)
           ("j" tetris-move-left)
           ("k" tetris-move-down)
           ("l" tetris-move-right))))

(fc-load 'speed-type
  :after (fc-modal-exclude-mode 'speed-type-mode))

(defvar *fc-enable-snails* nil)

(fc-load 'snails
  :local t
  :enable *fc-enable-snails*
  :after (fc-modal-exclude-mode 'snails-mode)
  :bind '((snails-mode-map
           ("C-j" snails-select-next-item)
           ("C-k" snails-select-prev-item))))

(fc-load 'vterm
  :after (fc-modal-exclude-mode 'vterm-mode))

(provide 'fc-extra)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-extra.el ends here
