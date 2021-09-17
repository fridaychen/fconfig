;;; fc-modal.el --- modal mode -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)
(require 'color)

(defvar *fc-modal-command-cursor-color* "#ff71b5")
(defvar *fc-modal-command-cursor-shape* 'box)
(defvar *fc-modal-command-fringe-color* "#df8a76")

(defvar *fc-modal-edit-cursor-color* (if *is-colorful* "#3de1ad" "#ffffff"))
(defvar *fc-modal-edit-cursor-shape* (if *is-colorful* 'box 'bar))
(defvar *fc-modal-edit-fringe-color* "#8DD4E8")

(defvar *fc-modal-cur-fringe-color* *fc-modal-command-fringe-color*)

(defconst *fc-modal-fringe-faces* '(fringe
                                    flycheck-fringe-error
                                    flycheck-fringe-info
                                    flycheck-fringe-warning))

(defvar *fc-modal-idle-timeout* 60)
(defvar *fc-modal-keymap* (fc-make-keymap nil "fc-modal") "Keymap of Modal mode.")

(defvar *fc-modal-space-click-count* 0)

(defvar *fc-modal-exclude-modes*
  '(eshell-mode
    term-mode
    dired-mode
    vc-git-log-edit-mode
    Custom-mode))

(defvar *fc-modal-exclude-names*
  '("*fireplace*"
    "COMMIT_EDITMSG"))

;; modal minor mode, make it easy
(define-minor-mode fc-modal-mode
  "Modalnomic kyeboard mode."
  :global nil
  :lighter " Modal"
  :keymap *fc-modal-keymap*)

(defun fc-modal-exclude-mode (&rest modes)
  "Exclude mode.
MODES: modes to be excluded."
  (--each modes
    (add-to-list '*fc-modal-exclude-modes* it)))

(defun fc-modal-activate ()
  "Activate modal."
  (unless (or (minibufferp)
              (member major-mode
                      *fc-modal-exclude-modes*)
              (member (buffer-name)
                      *fc-modal-exclude-names*))
    (fc-modal-mode 1)))

(define-globalized-minor-mode fc-modal-global-mode fc-modal-mode fc-modal-activate)
(add-to-list 'emulation-mode-map-alists `((fc-modal-mode . ,*fc-modal-keymap*)))

(defun fc-modal-set-cursor-color (color)
  "Set cursor COLOR."
  (cond (*is-gui*
         (set-cursor-color color))

        (t
         (send-string-to-terminal
          (format "\033]12;%s\007" color)))))

(setf *ansi-cursor-map*
      #s(hash-table
         data (
               box 0 hbar 3 nil 4)))

(defun fc-modal-set-cursor-shape (shape)
  "Set cursor SHAPE."
  (if *is-gui*
      (setf cursor-type shape)
    (send-string-to-terminal
     (format "\033[%d q"
             (gethash shape *ansi-cursor-map* 5)))))

(defun fc--modal-visual-feedback-enter ()
  "Enter modal mode."
  (hl-line-mode -1)
  (blink-cursor-mode -1)

  (fc-modal-set-cursor-color *fc-modal-command-cursor-color*)
  (fc-modal-set-cursor-shape *fc-modal-command-cursor-shape*)

  (--each *fc-modal-fringe-faces*
    (when (facep it)
      (set-face-attribute it nil
                          :background
                          *fc-modal-cur-fringe-color*
                          :foreground
                          (apply #'color-rgb-to-hex
                                 (color-complement *fc-modal-cur-fringe-color*))))))

(defun fc--modal-visual-feedback-leave ()
  "Leave modal mode."
  (hl-line-mode 1)
  (blink-cursor-mode 1)

  (fc-modal-set-cursor-color *fc-modal-edit-cursor-color*)
  (fc-modal-set-cursor-shape *fc-modal-edit-cursor-shape*)

  (--each *fc-modal-fringe-faces*
    (when (facep it)
      (set-face-attribute it nil
                          :background
                          (face-attribute 'default :background)
                          :foreground
                          (face-attribute 'font-lock-keyword-face :foreground)))))

(defun fc-modal-visual-feedback ()
  "Setup modal mode ui on GUI."
  (interactive)

  (if fc-modal-mode
      (fc--modal-visual-feedback-enter)
    (fc--modal-visual-feedback-leave)))

(defun fc-modal-after-theme-change ()
  "Hook function for after theme change."
  (let* ((default-bg (face-attribute 'default :background))
         (bg (or
              (and (facep 'fringe)
                   (face-attribute 'fringe :background))
              (fc-get-face-attribute 'highlight :foreground)))
         (fringe-bg (if (> (fc-color-difference default-bg bg) 30000)
                        bg
                      *fc-modal-command-fringe-color*)))
    (setf *fc-modal-cur-fringe-color* fringe-bg))

  (fc-modal-visual-feedback))

(defun fc-modal-advice (orig-fun &rest args)
  "Setup modal advice.
ORIG-FUN: original function.
ARGS: original arguments."
  (interactive)

  (let ((rval (apply orig-fun args)))
    (fc-modal-visual-feedback)
    rval))

(defun fc-modal-idle-timer ()
  "IDLE timer function."
  (unless fc-modal-mode
    (fc-modal-global-mode)))

(defun fc-modal-keys (keydefs)
  "Bind keys.
KEYDEFS: new key definitions for modal."
  (--each keydefs
    (define-key *fc-modal-keymap*
      (kbd (cl-first it))
      (cl-second it))))

(defun fc-parse-head-key-doc (keymap)
  "Generate KEYMAP help string."
  (with-temp-buffer
    (insert (documentation-property keymap 'variable-documentation))

    (save-excursion
      (goto-char (point-min))
      (when (looking-at "KEYS")
        (mark-word)
        (kill-region (region-beginning) (region-end))))

    (fc-replace-regexp "\\([^ \n:]+\\): +" "\\1→" :from-start t)

    (cl-loop
     with items = (split-string (buffer-string) "  +" t " +")
     with end = (length items)
     with width = (/ (frame-width) 28)
     for i from 0 to end by width
     initially (erase-buffer)
     do
     (--each (cl-subseq items i (min (+ i width) end))
       (let ((info (split-string it "→")))
         (insert
          (format "%4s"
                  (propertize (cl-first info) 'face '(:foreground "tomato" :inherit bold)))
          (propertize " │ " 'face '(:foreground "tomato" :inherit bold))
          (format "%-20s"
                  (cl-second info)))))
     (insert "\n")
     finally return (buffer-string))))

(cl-defun fc-modal-head-key (prompt keymap &key (timeout 3) (repeat nil) (around nil))
  "Wait user input then run keymap.
PROMPT: user prompt.
KEYMAP: target keymap.
TIMEOUT: input timeout in seconds.
REPEAT: run once or repeat.
AROUND: advice function."
  (cl-loop
   with keys = ""
   with showing-doc = nil
   with cur-prompt = (fc-prompt prompt)
   with key = nil
   with repeat-prop = nil
   with ret = nil
   initially (unless fc-modal-global-mode
               (fc-modal-set-cursor-color *fc-modal-command-cursor-color*))

   do
   (setf key (read-char cur-prompt
                        nil
                        timeout))

   (cond ((or (null key)                     ; read timeout
              (eql key 27)                   ; escape key
              (eql key 9))                   ; tab key

          (message "")
          (unless fc-modal-global-mode
            (fc-modal-set-cursor-color *fc-modal-edit-cursor-color*))
          (cl-return))

         ((equal key 13)                     ; return
          (unless showing-doc
            (setf cur-prompt (concat (fc-parse-head-key-doc keymap) cur-prompt)
                  timeout 20
                  showing-doc t)))

         (t                                  ; other keys
          (setf keys (concat keys (char-to-string key))
                ret (lookup-key (symbol-value keymap) keys))

          (cond
           (;; undefined key
            (null ret)
            (message "%s %s" prompt "undefined key")
            (unless fc-modal-global-mode
              (fc-modal-set-cursor-color *fc-modal-edit-cursor-color*))
            (cl-return))

           ;; function or closure
           ((or (cl-typep ret 'byte-code-function)
                (cl-typep ret 'symbol)
                (and (consp ret)
                     (equal (car ret)
                            'closure)))
            (if around
                (fc-funcall around :args (list ret))
              (fc-funcall ret))

            (when (cl-typep ret 'symbol)
              (setq repeat-prop (get ret 'fc-repeat)))

            (if (or repeat repeat-prop)
                (let ((m (current-message)))
                  (when m
                    (setf cur-prompt (format "%s [%s] :" prompt m)))
                  (setf keys ""
                        repeat-prop nil))
              (unless fc-modal-global-mode
                (fc-modal-set-cursor-color *fc-modal-edit-cursor-color*))
              (cl-return))))))))

(cl-defun fc-modal-mark-repeat (&rest rest)
  "Mark function repeatable.
REST: list of functions."
  (--each rest
    (put it 'fc-repeat t)))

(cl-defun fc-modal-run (&optional (timeout 2))
  "Run modal input.
TIMEOUT: user input timeout in seconds."
  (interactive)

  (fc-modal-head-key "Modal"
                     '*fc-modal-keymap*
                     timeout
                     t))

(defun fc-modal-input ()
  "Simple input function under modal."
  (interactive)

  (cl-loop
   with c = nil
   initially (fc-modal-set-cursor-shape 'hbar)
   do
   (setf c (read-char))

   (when (member c '(13 27))
     (fc-modal-set-cursor-shape *fc-modal-command-cursor-shape*)
     (cl-return))

   (cond
    ((member c '(8 127))
     (delete-char -1))
    (t (insert c)))))

(defun fc--modal-handle-space-timeout ()
  "Handle space timeout."
  (if (eq *fc-modal-space-click-count* 2)
      (progn
        (when (buffer-modified-p)
          (backward-delete-char 2))
        (fc-modal-global-mode 1))

    (unless (buffer-modified-p)
      (insert " ")))

  (setf *fc-modal-space-click-count* 0))

(defun fc--modal-handle-space-press ()
  "Handle space press."
  (interactive)

  (when (buffer-modified-p)
    (selfc-insert-command 1))

  (when (eq *fc-modal-space-click-count* 0)
    (run-at-time "0.2sec" nil 'fc--modal-handle-space-timeout))

  (cl-incf *fc-modal-space-click-count*))

;; setup
(run-with-idle-timer *fc-modal-idle-timeout* t 'fc-modal-idle-timer)

(advice-add 'fc-modal-mode :around 'fc-modal-advice)

;; (global-set-key (kbd "SPC") 'fc--modal-handle-space-press)

(provide 'fc-modal)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-modal.el ends here
