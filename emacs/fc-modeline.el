;;; fc-modeline.el --- DESCRIPTION -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'cl-lib)
(require 'fc-modeline-separator)

(defvar *fc-ml-icon-height* *fc-font-height*)

(defvar *fc-modeline-active-hl-bg* "#FEBA07")
(defvar *fc-modeline-active-hl-fg* "#1E3124")
(defvar *fc-modeline-dark-active-hl-bg* "#887322")
(defvar *fc-modeline-dark-active-hl-fg* "#1E3124")

(defvar *fc-narrow-window-threshold* 65 "Criteria for narrow window.")
(defvar *fc-extreme-narrow-window-threshold* 40 "Criteria for extreme narrow window.")
(defconst fc--modeline-hl-face 'fc-modeline-hl-face)
(defconst fc--modeline-hl-sep (fc-text " " :face fc--modeline-hl-face))
(defconst fc--modeline-hl-inactive-face 'fc-modeline-hl-inactive-face)
(defconst fc--modeline-hl-inactive-sep (fc-text " " :face fc--modeline-hl-inactive-face))

(defvar *fc-selected-window* (frame-selected-window))

(defvar *fc-buffer-id-keymap* (fc-make-keymap
                               '(([mode-line mouse-1] ivy-switch-buffer))
                               "fc-buffer-id-keymap"))
(defvar *fc-pos-keymap* (fc-make-keymap
                         '(([mode-line mouse-1] counsel-imenu))
                         "fc-pos-keymap"))

(defun fc--active-window-p ()
  "Test if current window is active."
  (equal *fc-selected-window* (selected-window)))

(defun fc--modeline-base-face ()
  "Return base face for modeline."
  (if (fc--active-window-p)
      'mode-line
    'mode-line-inactive))

(defun fc--modeline-get-hl-face ()
  "Get approparite highlight face."
  (if (fc--active-window-p)
      fc--modeline-hl-face
    fc--modeline-hl-inactive-face))

(defun fc--modeline-get-hl-sep ()
  "Get approparite highlight separator."
  (if (fc--active-window-p)
      fc--modeline-hl-sep
    fc--modeline-hl-inactive-sep))

(defun fc--narrow-window-p ()
  "Test if current window is narrow."
  (>= *fc-narrow-window-threshold* (window-width)))

(defun fc--wide-window-p ()
  "Test if current window is wide."
  (not (fc--narrow-window-p)))

(defun fc--extreme-narrow-window-p ()
  "Test if current window is extreme narrow."
  (>= *fc-extreme-narrow-window-threshold* (window-width)))

(defun fc--left-bottom-window-p ()
  "Test if current window is at left-bottom."
  (or (one-window-p)
      (and (window-at-side-p (get-buffer-window) 'left)
           (window-at-side-p (get-buffer-window) 'bottom))))

(defun fc--right-bottom-window-p ()
  "Test if current window is at right-bottom."
  (or (one-window-p)
      (and (window-at-side-p (get-buffer-window) 'right)
           (window-at-side-p (get-buffer-window) 'bottom))))

(defvar-local *fc-enable-state-seg* t)

(cl-defun fc--state-seg ()
  "Buffer state segment."
  (unless (and *fc-enable-state-seg*
               (not (fc--extreme-narrow-window-p)))
    (cl-return-from fc--state-seg))

  (let* ((state (concat (if buffer-read-only "%%" "-")
                        (if (buffer-modified-p)  "*" "-"))))
    (when (fboundp 'fc-modeline-extra-state)
      (setf state (fc-modeline-extra-state state)))

    state))

(defvar-local *fc-enable-major-mode-seg* t)

(defun fc--major-mode-seg ()
  "The name of the major mode."
  (when (and *fc-enable-major-mode-seg*
             (fc--wide-window-p))
    (fc-text-propertize
     (fc-mode-name)
     `(face (:height ,*fc-ml-icon-height*))
     :copy t)))

(defvar-local *fc-buffer-title-seg* nil)

(defun fc--remote-buffer-p ()
  "Test if current buffer is remote."
  (and default-directory
       (file-remote-p default-directory 'host)))

(defun fc--buffer-full-id ()
  "Generate buffer full id."
  (let ((rhost (fc--remote-buffer-p))
        (proj (fc-modeline-proj-name)))
    (fc-text
     (list
      (when rhost
        (concat rhost "/"))
      (when proj
        (concat proj "::"))
      (buffer-name))
     :limit (ceiling
             (* (window-width) 0.5))
     :separator ""
     :keys *fc-buffer-id-keymap*)))

(defun fc--buffer-short-id ()
  "Generate buffer short id."
  (fc-text (buffer-name)
           :limit (ceiling
                   (* (window-width) 0.35))
           :keys *fc-buffer-id-keymap*))

(defun fc--buffer-title-seg ()
  "Buffer title segment."
  (cond
   (*fc-buffer-title-seg*
    (fc-funcall *fc-buffer-title-seg*))

   ((fc--narrow-window-p)
    (fc-text
     (list
      (fc--vc-seg)
      (fc--buffer-short-id))))

   (t
    (fc-text
     (list
      (fc--vc-seg)
      (fc--buffer-full-id))))))

(defun fc-vc-seg-tip ()
  "Return VC seg tip message."
  (fc-text
   (list
    (fc-vc-branch)
    (vc-state buffer-file-name))))

(defvar *fc--vc-mark*
  (fc-visible (fc-nerd-icon "\xE725")
              ""
              "VC"))
(defvar *fc--vc-modified-mark*
  (fc-visible (fc-nerd-icon "\xE725" :foreground "red1")
              (fc-text "" :face '(:foreground "#cf6a4c"))
              (fc-text "VC" :face '(:foreground "#cf6a4c"))))
(defvar *fc--vc-merge-mark*
  (fc-visible (fc-nerd-icon "\xE726" :foreground "red1")
              (fc-text "" :face '(:foreground "#ff0066"))
              (fc-text "VC" :face '(:foreground "#ff0066"))))

(defun fc--vc-seg ()
  "VC state segment."
  (when (and (fc-main-thread-p)
             vc-mode)
    (pcase (vc-state buffer-file-name)
      ('edited *fc--vc-modified-mark*)
      ((or 'needs-merge 'conflict) *fc--vc-merge-mark*)
      (_ *fc--vc-mark*))))

(defun fc--line-col-seg ()
  "Line column segment."
  (when (fc--wide-window-p)
    (cond
     ((eq 'pdfc-view-mode major-mode)
      (if (fboundp #'spaceline--pdfview-page-number)
          (spaceline--pdfview-page-number)
        "_"))

     ((and column-number-mode line-number-mode)
      (concat
       "%3l:%c"
       (cond
        ((< (current-column) 10)
         "  ")
        ((< (current-column) 100)
         " ")
        (t
         ""))))

     (column-number-mode
      "_:%03c")

     (line-number-mode
      "%03l:_")

     (t
      "_:_"))))

(defun fc--pos-seg ()
  "Position seg."
  (when (> (buffer-size) 10240)
    (list -3
          (fc-text
           "%p"
           :face (fc--modeline-get-hl-face)
           :keys *fc-pos-keymap*))))

(defvar *fc-flycheck-seg-keymap*
  (fc-make-keymap
   '(([mode-line mouse-1] flycheck-list-errors))
   "fc-flycheck-seg-keymap"))

(cl-defun fc--flycheck-seg()
  "Flycheck seg."
  (when (bound-and-true-p flycheck-mode)
    (let* ((result (flycheck-count-errors flycheck-current-errors))
           (err (cdr (assoc 'error result)))
           (warning (cdr (assoc 'warning result))))
      (unless (or err warning)
        (cl-return-from fc--flycheck-seg nil))

      (list
       "🪲"
       (fc-text err
                :face '(:foreground "red2" :weight bold)
                :keys *fc-flycheck-seg-keymap*)
       (when (and err warning)
         "|")
       (fc-text warning
                :keys *fc-flycheck-seg-keymap*)))))

(defun fc--modeline-format-left ()
  "Format left modeline."
  (let ((hi-face (fc--modeline-get-hl-face))
        (hl-sep (fc--modeline-get-hl-sep)))
    (list
     hl-sep
     (fc-text
      (fc--line-col-seg)
      :face hi-face
      :keys *fc-pos-keymap*)
     hl-sep
     (fc--pos-seg)
     hl-sep
     (fc-text-propertize
      (fc--state-seg)
      `(face (:inherit ,hi-face)))
     (fc-ml-left-sep)
     (fc--major-mode-seg)
     " ")))

(defvar *fc-comp-exit-code* 0)

(defun fc--compilation-exit (status code msg)
  (setq *fc-comp-exit-code* code)
  (cons msg code)

  (setq compilation-exit-message-function #'fc--compilation-exit))

(defun fc--compilation-seg ()
  (when (eq major-mode 'compilation-mode)
    (list
     (cond
      ((bound-and-true-p compilation-in-progress)
       (propertize "🚧‍"))

      ((eq *fc-comp-exit-code* 0)
       "")

      (t
       "❌"))

     compilation-mode-line-errors)))

(defun fc--modeline-format-center ()
  "Format center modeline."
  (list
   (fc-text
    (fc--buffer-title-seg))))

(defun fc--modeline-format-right ()
  "Format right modeline."
  (list
   " "
   'global-mode-string
   current-input-method-title
   (fc--flycheck-seg)
   (fc--compilation-seg)
   " "))

(defvar *fc-modeline-most-right-string* nil)

(defun fc--modeline-format-main ()
  "Format modeline."
  (let* ((left (fc--modeline-format-left))
         (center (fc--modeline-format-center))
         (right (fc--modeline-format-right))
         (right-str (format-mode-line right))
         (most-right (cons '(t (:eval (fc-ml-right-sep)))
                           *fc-modeline-most-right-string*))
         (most-right-str (if (fc--right-bottom-window-p)
                             (fc-text-propertize
                              (format-mode-line most-right)
                              `(face (:background ,*fc-modeline-active-hl-bg* :height ,*fc-ml-icon-height*)))
                           ""))
         (right-len (if (fboundp #'string-pixel-width)
                        (/ (+
                            (string-pixel-width right-str)
                            (string-pixel-width most-right-str))
                           (string-pixel-width " ")
                           1.0)
                      (+
                       (string-width right-str)
                       (string-width  most-right-str))))
         (padding (propertize " "
                              'display `(space :align-to (- (+ scroll-bar scroll-bar) ,right-len)))))
    (nconc left center
           (list padding
                 right-str
                 most-right-str))))

(cl-defun fc-modeline-mode ()
  "Setup mode line."
  (sit-for 1)

  (unless (and (facep 'default)
               (color-defined-p (face-attribute 'default :background))
               (color-defined-p (face-attribute 'default :foreground)))
    (cl-return-from fc-modeline-mode))

  (unless (facep 'fc-modeline-hl-face)
    (make-face 'fc-modeline-hl-face))

  (unless (facep 'fc-modeline-icon-hl-face)
    (make-face 'fc-modeline-icon-hl-face))

  (unless (facep 'fc-modeline-hl-inactive-face)
    (make-face 'fc-modeline-hl-inactive-face))

  (let* ((deep-dark (fc-deep-dark-theme-p))
         (bg (if deep-dark
                 *fc-modeline-dark-active-hl-bg*
               *fc-modeline-active-hl-bg*))
         (fg (if deep-dark
                 *fc-modeline-dark-active-hl-fg*
               *fc-modeline-active-hl-fg*)))
    (set-face-attribute 'fc-modeline-hl-face nil
                        :foreground fg
                        :background bg
                        :weight 'medium
                        :inherit 'mode-line)
    (set-face-attribute 'fc-modeline-hl-inactive-face nil
                        :foreground bg
                        :background fg
                        :weight 'medium
                        :inherit 'mode-line))

  (setf *fc-ml-icon-height* (round (* (fc-get-face-attribute 'mode-line :height) 0.9)))

  (set-face-attribute 'fc-modeline-icon-hl-face nil
                      :inherit 'fc-modeline-hl-face
                      :height *fc-ml-icon-height*)

  (fc-ml-sep-reset)

  (fc-funcall #'mode-icons-mode :args (list -1))
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (fc--modeline-format-main)))))

(defun fc-modeline-set-selected-window (&rest _)
  "Set the variable `*fc-selected-window*' appropriately."
  (when (not (minibuffer-window-active-p (frame-selected-window)))
    (setq *fc-selected-window* (frame-selected-window))
    (force-mode-line-update)))

(defun fc-modeline-unset-selected-window (&rest _)
  "Set the variable `*fc-selected-window*' appropriately."
  (when (not (minibuffer-window-active-p (frame-selected-window)))
    (setq *fc-selected-window* nil)
    (force-mode-line-update)))

(when (facep 'mode-line-active)
  (set-face-attribute 'mode-line-active nil :inherit 'mode-line))

(when (facep 'mode-line-inactive)
  (set-face-attribute 'mode-line-inactive nil :inherit 'mode-line))

(add-hook 'after-make-frame-functions #'fc-modeline-set-selected-window)
(add-hook 'buffer-list-update-hook #'fc-modeline-set-selected-window)
(add-hook 'window-configuration-change-hook #'fc-modeline-set-selected-window)
(add-hook 'window-selection-change-functions #'fc-modeline-set-selected-window)

(provide 'fc-modeline)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-modeline.el ends here
