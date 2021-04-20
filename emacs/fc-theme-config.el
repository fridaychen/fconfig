;;; fc-theme.el --- setup theme -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(defvar *fc-dark-theme* 'wombat "Perfer dark theme.")
(defvar *fc-deep-dark-theme* 'wombat "Prefer deep dark theme.")
(defvar *fc-light-theme* 'tango "Prefer light theme.")
(defvar *fc-enable-spaceline* t "Spaceline switch.")
(defvar *fc-modeline-separator* '(wave zigzag brace chamfer contour) "Prefer modeline separator style.")
(defvar *fc-current-theme* nil "Current theme.")
(defvar *fc-after-theme-hook* nil "After theme hook.")
(defvar *fc-narrow-window-threshold* 65 "Criteria for narrow window.")
(defvar *fc-extreme-narrow-window-threshold* 40 "Criteria for extreme narrow window.")

(defvar-local *fc-show-line-col-mode* t)

(defun fc-right-bottom-window-p ()
  "Test if current window is at right-bottom."
  (or (one-window-p)
      (and (not (window-right (get-buffer-window)))
           (not (window-right (window-parent)))
           (not (window-next-sibling)))))

(fc-load 'powerline
  :run t
  :enable *fc-enable-spaceline*)

(fc-load 'spaceline
  :run t
  :enable *fc-enable-spaceline*
  :after
  (progn
    (require 'spaceline-segments)

    (defun -fc-narrow-window-p ()
      (>= *fc-narrow-window-threshold* (window-width)))

    (defun -fc-wide-window-p ()
      (not (-fc-narrow-window-p)))

    (defun -fc-extreme-narrow-window-p ()
      (>= *fc-extreme-narrow-window-threshold* (window-width)))

    (spaceline-define-segment fc-line-col-seg
      (fc-text
       (list
        (when (and
               *fc-show-line-col-mode*
               (-fc-wide-window-p))
          (cond
           ((eq 'pdfc-view-mode major-mode)
            (spaceline--pdfview-page-number))

           ((and column-number-mode line-number-mode)
            (concat
             "%3l:%c"
             (cond
              ((> (current-column) 99)
               " ")
              ((> (current-column) 10)
               "  ")
              (t
               "   "))))

           (column-number-mode
            "_:%3c")

           (line-number-mode
            "%3l:_")

           (t
            "_:_")))

        (when (or (> (buffer-size) 10240)
                  (fc-viewer-mode-p))
          "%p"))))

    (spaceline-define-segment fc-info-seg
      (if (and (not (-fc-extreme-narrow-window-p))
               (fboundp 'fc-info-func)
               (not (fc-viewer-mode-p)))
          (fc-text (fc-info-func)
                   :face 'hi-black-b)
        ""))

    (spaceline-define-segment fc-major-mode-seg
      "The name of the major mode."
      (if (-fc-wide-window-p)
          (powerline-major-mode)
        ""))

    (defun fc--remote-buffer-p ()
      (and default-directory
           (file-remote-p default-directory 'host)))

    (defun fc--vc-str ()
      (when vc-mode
        (let* ((color (pcase (vc-state buffer-file-name)
                        ('edited "#cf6a4c")
                        ('up-to-date nil)
                        ((or 'needs-merge 'conflict) "#ff0066")
                        (_ "#00cc66")))
               (face (if color
                         `(:foreground ,color :inherit bold)
                       nil)))
          (fc-text (if *is-colorful* "" "VC")
                   :face face))))

    (defun fc--viewer-str ()
      (let* ((which (which-function))
             (chapter (if (null which) "" which)))
        (fc-text (list chapter
                       (file-name-sans-extension
                        (buffer-name)))
                 :face '(:foreground "#cf6a4c" :inherit bold)
                 :limit (- (window-width) 10)
                 :separator " :")))

    (defun fc--buffer-full-id ()
      (let ((rhost (fc--remote-buffer-p))
            (proj (and (boundp 'fc-proj-name) fc-proj-name)))
        (fc-text
         (list
          (when rhost
            (concat rhost "/"))
          (when proj
            (concat proj "::"))
          (buffer-name))
         :separator "")))

    (defun fc--buffer-short-id ()
      (fc-text (buffer-name)
               :limit (ceiling
                       (* (window-width) 0.35))))

    (spaceline-define-segment fc-buffer-title-seg
      (cond
       ((fc-viewer-mode-p)
        (fc--viewer-str))

       ((-fc-narrow-window-p)
        (fc-text
         (list
          (fc--vc-str)
          (fc--buffer-short-id))))

       (t
        (fc-text
         (list
          (fc--vc-str)
          (fc--buffer-full-id))))))

    (spaceline-define-segment fc-vc-seg
      (when vc-mode
        (let ((color (pcase (vc-state buffer-file-name)
                       ('edited "#cf6a4c")
                       ('up-to-date "gray")
                       ((or 'needs-merge 'conflict) "#ff0066")
                       (_ "#00cc66"))))
          (fc-text (if *is-colorful* "" "VC")
                   :face `(:foreground ,color)))))

    (spaceline-define-segment fc-layout-seg
      (if (and (fboundp 'fc-layout-current)
               (-fc-wide-window-p)
               (fc-right-bottom-window-p))
          (fc-text (format ":%s" (fc-layout-current))
                   :face '(:foreground "#cf6a4c" :inherit bold))
        ""))

    (spaceline-define-segment fc-proj-seg
      (and (fboundp 'fc-proj-update-mode-line)
           (-fc-wide-window-p)
           (fc-right-bottom-window-p)
           (fc-proj-update-mode-line)))

    (spaceline-define-segment fc-work-seg
      (when (and (boundp '*fc-tomato-bar*)
                 (-fc-wide-window-p)
                 (fc-right-bottom-window-p))
        *fc-tomato-bar*))))

(defun fc-setup-term-mode-line ()
  "Setup mode line for terminal."
  (setq-default mode-line-format
                (list '(:propertize " %4l:" face (:background "grey" :foreground "blue"))
                      '(:propertize "%3c " face (:background "grey" :foreground "blue"))
                      " "
                      '("%e" (:eval (if (fboundp 'fc-info-func)
                                        (fc-info-func)
                                      "")))
                      " "
                      'mode-line-mule-info
                      "  "
                      'mode-line-modified
                      "  "
                      '("%e" (:eval (let ((name (fc-proj-name)))
                                      (if name
                                          (concat name "::")))))
                      'mode-line-buffer-identification
                      "  "
                      'mode-name
                      "  "
                      'global-mode-string
                      "      "
                      '("%e" (:eval (fc-proj-update-mode-line))))))

(defun fc-setup-powerline (&optional seperator)
  "Setup powerline.
SEPERATOR: powerline seperator."
  (setf powerline-default-separator
        (or seperator
            (seq-random-elt *fc-modeline-separator*))))

(defun fc-setup-spaceline-mode-line ()
  "Setup spaceline."
  (spaceline-helm-mode)

  (spaceline-install
    'fc
    ;; left
    `((fc-line-col-seg :face highlight-face)
      (fc-info-seg input-method)
      (fc-major-mode-seg fc-buffer-title-seg)
      nyan-cat)
    ;; right
    `((flycheck-error)
      (flycheck-warning)
      ;; (process :when active)
      (org-clock :when active)
      (battery :when active)
      (global :when active)
      (fc-layout-seg fc-work-seg)
      (fc-proj-seg :face highlight-face)))

  (setq-default mode-line-format
                '("%e" (:eval (spaceline-ml-fc)))))

(defun fc-setup-reading-mode-line ()
  "Set reading spaceline mode-line."
  (spaceline-install
    'fc-reading
    ;; left
    `((fc-line-col-seg :face highlight-face)
      (fc-buffer-title-seg))
    ;; right
    `())

  (setq mode-line-format
        (if (fc-viewer-mode-p)
            '("%e" (:eval (spaceline-ml-fc-reading)))
          '("%e" (:eval (spaceline-ml-fc))))))

(defun fc-reset-theme ()
  "Reset color theme."
  (interactive)

  (load-theme *fc-current-theme* t)

  (fc-run-hook '*fc-after-theme-hook*))

(defun fc--after-load-theme ()
  "Load color theme."
  (load-theme *fc-current-theme* t)

  (fc-run-hook '*fc-after-theme-hook*
               (if *fc-booting* 2 1)))

(cl-defun fc-load-theme (theme &optional powerline-seperator)
  "Load theme.
THEME: new theme.
POWERLINE-SEPERATOR: new seperator for powerline."
  (when theme
    (-map #'disable-theme custom-enabled-themes)

    (cond
     ((symbolp theme)
      (setf *fc-current-theme* theme)
      (fc--after-load-theme))

     ((consp theme)
      (setf *fc-current-theme* (car theme))
      (fc--after-load-theme))

     (t
      (message "Unknown theme type")
      (cl-return-from fc-load-theme))))

  (cond
   ((fboundp 'fc-modeline-mode)
    (fc-modeline-mode))

   (*fc-enable-spaceline*
    (fc-setup-powerline powerline-seperator)
    (fc-setup-spaceline-mode-line))

   (t
    (fc-setup-term-mode-line))))

(defun fc-auto-select-theme (themes)
  "Auto select and load theme from THEMES.
THEMES: list of themes."
  (fc-load-theme
   (cl-loop
    with theme = nil
    do
    (setf theme (seq-random-elt themes))
    while (and (> (length themes) 1)
               (if (consp theme)
                   (eq (car theme) *fc-current-theme*)
                 (eq theme *fc-current-theme*)))
    finally return theme)))

(defun fc-light-theme ()
  "Randomly select a light theme."
  (interactive)

  (fc-auto-select-theme *fc-light-theme*))

(defun fc-dark-theme ()
  "Randomly select a dark theme."
  (interactive)

  (fc-auto-select-theme *fc-dark-theme*))

(defun fc-deep-dark-theme ()
  "Randomly select a deep dark theme."
  (interactive)

  (fc-auto-select-theme *fc-deep-dark-theme*))

(when *is-mac*
  (setf ns-use-srgb-colorspace nil))

(let ((install-theme (lambda (x)
                       (and (consp x)
                            (fc-install (cdr x))))))
  (-each *fc-dark-theme* install-theme)
  (-each *fc-deep-dark-theme* install-theme)
  (-each *fc-light-theme* install-theme))

(provide 'fc-theme-config)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-theme-config.el ends here
