;;; fc-modeline.el --- DESCRIPTION -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(defconst fc--modeline-hi-face 'fc-modeline-highlight-face)
(defconst fc--modeline-hl-sep (fc-text " " :face fc--modeline-hi-face))
(defconst fc--modeline-sep " ")

(defun fc--narrow-window-p ()
  "Test if current window is narrow."
  (>= *fc-narrow-window-threshold* (window-width)))

(defun fc--wide-window-p ()
  "Test if current window is wide."
  (not (fc--narrow-window-p)))

(defun fc--extreme-narrow-window-p ()
  "Test if current window is extreme narrow."
  (>= *fc-extreme-narrow-window-threshold* (window-width)))

(defun fc--right-bottom-window-p ()
  "Test if current window is at right-bottom."
  (or (one-window-p)
      (and (not (window-right (get-buffer-window)))
           (not (window-right (window-parent)))
           (not (window-next-sibling)))))

(defun fc--state-seg ()
  "Buffer state segment."
  (if (and (not (fc--extreme-narrow-window-p))
           (fboundp 'fc-info-func)
           (not (fc-viewer-mode-p)))
      (fc-info-func)
    ""))

(defun fc--major-mode-seg ()
  "The name of the major mode."
  (if (fc--wide-window-p)
      mode-name
    ""))

(defun fc--remote-buffer-p ()
  "Test if current buffer is remote."
  (and default-directory
       (file-remote-p default-directory 'host)))

(defun fc--viewer-seg ()
  "Generate viewer state string."
  (let* ((which (which-function))
         (chapter (if (null which) "" which)))
    (fc-text (list chapter
                   (file-name-sans-extension
                    (buffer-name)))
             :face '(:foreground "#cf6a4c" :inherit bold)
             :limit (- (window-width) 10)
             :separator " :")))

(defun fc--buffer-full-id ()
  "Generate buffer full id."
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
  "Generate buffer short id."
  (fc-text (buffer-name)
           :limit (ceiling
                   (* (window-width) 0.35))))

(defun fc--buffer-title-seg ()
  "Buffer title segment."
  (cond
   ((fc-viewer-mode-p)
    (fc--viewer-seg))

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

(defun fc--vc-seg ()
  "VC state segment."
  (when vc-mode
    (let ((color (pcase (vc-state buffer-file-name)
                   ('edited "#cf6a4c")
                   ('up-to-date "gray")
                   ((or 'needs-merge 'conflict) "#ff0066")
                   (_ "#00cc66"))))
      (fc-text (if *is-colorful* "" "VC")
               :face `(:foreground ,color)))))

(defun fc--layout-seg ()
  "Layout segment."
  (if (and (fboundp 'fc-layout-current)
           (fc--wide-window-p))
      (fc-text (format ":%s" (fc-layout-current))
               :face '(:foreground "#cf6a4c" :inherit bold))
    ""
    ))

(defun fc--proj-seg ()
  "Project segment."
  (and (fboundp 'fc-proj-update-mode-line)
       (fc--wide-window-p)
       (fc-proj-update-mode-line)))

(defun fc--work-seg ()
  "Work segment."
  (when (and (boundp '*fc-tomato-bar*)
             (fc--wide-window-p))
    *fc-tomato-bar*))

(defun fc--line-col-seg ()
  "Line column segment."
  (when (and *fc-show-line-col-mode*
             (fc--wide-window-p))
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
  (when (or (> (buffer-size) 10240)
            (fc-viewer-mode-p))
    (list -3
          (fc-text
           "%p"
           :face fc--modeline-hi-face))))

(defun fc--modeline-format-left ()
  "Format left modeline."
  (list
   fc--modeline-hl-sep
   (fc-text
    (fc--line-col-seg) :face fc--modeline-hi-face)
   fc--modeline-hl-sep
   (fc--pos-seg)
   fc--modeline-hl-sep
   (fc-text
    (fc--state-seg) :face fc--modeline-hi-face)
   fc--modeline-hl-sep
   fc--modeline-sep
   (fc--major-mode-seg)
   fc--modeline-sep))

(defun fc--modeline-format-center ()
  "Format center modeline."
  (list
   (fc-text
    (fc--buffer-title-seg))))

(defun fc--modeline-format-right ()
  "Format right modeline."
  (list
   " "
   'global-mode-string))

(defun fc--modeline-format-most-right ()
  "Format most right modeline."
  (list
   " "
   (fc--layout-seg)
   " "
   (fc--work-seg)
   " "
   (fc-text
    (fc--proj-seg) :face fc--modeline-hi-face)))

(defun fc--modeline-format-main ()
  "Format modeline."
  (let* ((left (fc--modeline-format-left))
         (center (fc--modeline-format-center))
         (right (fc--modeline-format-right))
         (most-right (if (fc--right-bottom-window-p)
                         (fc--modeline-format-most-right)
                       nil))
         (available-width (-
                           (window-width)
                           (string-width (format-mode-line left))
                           (string-width (format-mode-line center))
                           (string-width (format-mode-line right))
                           (string-width (format-mode-line most-right))))
         (padding (if (> available-width 0)
                      (list (make-string available-width ? ))
                    nil)))
    (nconc left center padding right most-right)))

(defun fc-modeline-mode ()
  "Setup mode line."
  (unless (facep 'fc-modeline-highlight-face)
    (make-face 'fc-modeline-highlight-face)
    (set-face-attribute 'fc-modeline-highlight-face nil
                        :foreground "black"
                        :background "DarkGoldenrod2"
                        :weight 'medium
                        :inherit 'mode-line))

  (fc-funcall #'mode-icons-mode :args (list -1))
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (fc--modeline-format-main)))))

(provide 'fc-modeline)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-modeline.el ends here
