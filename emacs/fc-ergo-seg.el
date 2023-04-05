;;; fc-ergo-seg.el --- Ergo modeline segments -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(defvar *fc--modal-mark* (fc-visible "" "M"))

(defun fc-modeline-extra-state (state)
  "Mode-line info func."
  (when buffer-file-name
    (concat (if fc-modal-mode *fc--modal-mark* "-")
            (if *fc-ergo-prefix* "P" "-")
            (if *fc-dev-mode* "D" "-")
            state
            (upcase *fc-layout-current*))))

(defun fc-modeline-proj-name ()
  "Get project name of current buffer."
  (bound-and-true-p fc-proj-name))

(defun fc-user-select-theme-mode ()
  (let ((mode (fc-user-select (format "Theme mode")
                              '(dark light)
                              :mouse t)))
    (when mode
      (setf *fc-theme-mode* mode))))

(defconst *fc-menu*
  (fc-create-pop-menu
   "Start"
   '(
     (fc-user-select-control-mode "Control")
     (fc-user-select-theme-mode "Theme mode")
     (fc-user-select-project "Projects"))))

(defvar *fc--menu-seg-format* (fc-visible "⟨%s⟩" "{%s}"))

(defconst *fc--menu-seg-keymap*
  (fc-make-keymap
   `(([mode-line mouse-1]
      ,(lambda () (interactive) (fc-eval-pop-menu *fc-menu*))))))

(defun fc--menu-seg ()
  "Menu segment."
  (and (boundp '*fc-project-name*)
       (fc-text (format *fc--menu-seg-format*
                        *fc-project-name*)
                :face 'fc-modeline-hl-face
                :keys *fc--menu-seg-keymap*)))

(add-to-list '*fc-modeline-most-right-string* '(t (:eval (fc--menu-seg))))

(defun fc--tomato-seg ()
  "Return the tomate status."
  (when (and (fc--wide-window-p) (fc--right-bottom-window-p))
    *fc-tomato-bar*))

(add-to-list 'global-mode-string '(t (:eval (fc--tomato-seg))))

(defun fc--battery-seg ()
  "Return the battery status."
  (when-let* ((level-str (cdr (assq ?p (battery-pmset))))
              (level (cl-parse-integer level-str)))
    (fc-text (format (cond ((>= level 90)
                            "🔋")
                           ((>= level 30)
                            "🔋%d")
                           (t
                            "🪫%d"))
                     level))))

(when *has-battery*
  (add-to-list '*fc-modeline-most-right-string* '(t (:eval (fc--battery-seg)))))

(defun fc--player-tip ()
  (let ((meta (fc-player--get-metadata *fc-player*)))
    (fc--text "\n│ "
              (oref *fc-player* name)
              (alist-get 'artist meta)
              (alist-get 'album meta)
              (alist-get 'title meta)
              (format "Volume %d" (fc-player--get-volume *fc-player*)))))

(defconst *fc--player-seg-keymap*
  (fc-make-keymap
   `(
     ([mode-line mouse-1] ,(fc-manual (fc-player--play-pause *fc-player*)))
     ([mode-line mouse-2] ,(fc-manual (fc-player--next *fc-player*)))
     ([mode-line mouse-3] ,(fc-manual (fc-player--previous *fc-player*)))
     ([mode-line mouse-4] ,(fc-manual (fc-player--volume-up *fc-player*)))
     ([mode-line mouse-5] ,(fc-manual (fc-player--volume-down *fc-player*)))
     )
   "fc-player-keymap"))

(defun fc--player-seg ()
  "Return the player states."
  (when (and *is-gui* (fc--right-bottom-window-p) (fc--wide-window-p) *fc-player*)
    (fc-text (pcase (fc-player--get-play-status *fc-player*)
               ('Playing "⏸️")
               ((or 'Paused 'Stopped) "▶️")
               (_ ""))
             :tip '(fc--player-tip)
             :keys *fc--player-seg-keymap*)))

(defun fc--player-seg-cb ()
  (force-mode-line-update))

(when *is-gui*
  (add-hook '*fc-player-hook* #'fc--player-seg-cb)
  (add-to-list '*fc-modeline-most-right-string* '(t (:eval (fc--player-seg)))))

(provide 'fc-ergo-seg)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-ergo-seg.el ends here
