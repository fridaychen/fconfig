;;; fc-ergo.el --- setup ergo -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)
(require 'hi-lock)

(defconst *fc-undef-key* '(lambda () (interactive)))
(defconst *ergo-head-key-timeout* 3)
(defconst *ergo-prefix-timeout* 1)

(defvar *fc-ergo-prefix* nil)
(defvar *fc-ergo-prefix-timer* nil)
(defvar *fc-ergo-pre-cursor-color* "#cc0000")
(defvar *fc-ergo-restore-hook* nil)

(defconst *fc-repeat-key* "t")
(defconst *fc-repeat-orignal-func* #'fc-translate-word)

(defvar *fc-work-themes* '(zenburn monokai))
(defvar *fc-work-dark-themes* '(sanityinc-tomorrow-night))

(fc-load 'compile
  :local t
  :after (progn
           (setf compilation-scroll-output t
                 compilation-auto-jump-to-first-error nil
                 compilation-auto-jump-to-next t)))

(defconst *fc-book-zh-single-quote*
  (fc-decorate-region
   (lambda ()
     (when (seq-contains-p "“”‘’'\"" (char-after))
       (delete-char 1))
     (insert "‘"))
   (lambda ()
     (when (seq-contains-p "“”‘’'\"" (char-before))
       (delete-char -1))
     (insert "’"))))

(defconst *fc-book-zh-quote*
  (fc-decorate-region
   (lambda ()
     (when (seq-contains-p "“”‘’'\"" (char-after))
       (delete-char 1))
     (insert "“"))
   (lambda ()
     (when (seq-contains-p "“”‘’'\"" (char-before))
       (delete-char -1))
     (insert "”"))))

(cl-defun fc-file-info ()
  "Create file info."
  `(("Name" ,buffer-file-name)
    ("Basic" ,(format "%d bytes, %d lines, %s, point %d, %d"
                      (buffer-size)
                      (fc-buffer-lines)
                      buffer-file-coding-system
                      (point)
                      (point-max)))
    ("VC" ,(fc-text
            (list
             (fc-vc-branch)
             (fc-string (when buffer-file-name
                          (vc-state buffer-file-name))))
            :separator ", "))))

(defun fc-buffer-info ()
  "Create buffer info."
  `(("Major" ,major-mode)
    ("Tag/Xref" ,(format "%s %s"
                         (if (boundp 'fc-proj-tag) fc-proj-tag nil)
                         xref-backend-functions))
    ("Company" ,(s-join " "
                        (--map
                         (s-chop-prefix "company-" (symbol-name it))
                         company-backends)))
    ("Format" ,(format "IndentTab %S, Auto %S"
                       indent-tabs-mode
                       *fc-format-at-save*))))

(defun fc-match-paren ()
  "Jump to another bracket."
  (interactive)

  (cond
   ((looking-at "\\s\(")
    (forward-list 1)
    (backward-char 1))

   ((looking-at "\\s\)")
    (forward-char 1)
    (backward-list 1))

   (t
    (backward-char 1)

    (if (looking-at "\\s\)")
        (progn
          (forward-char 1)
          (backward-list 1))
      (forward-char 1)))))

(defun fc-toggle-window-maximize ()
  "Toggle window maximize."
  (interactive)

  (unless (one-window-p)
    (fc-toggle-maximize-window-in-box)))

(cl-defun fc--query-replace (&key backward from-beginning)
  "Replace.
BACKWARD: backward direction.
FROM-BEGINNING: start from beginnning."
  (let* ((from-str (fc-current-thing :regq t :prompt "Regex Query replace from"))
         (to-str (read-string (format "Regex Query replace from %s to : "
                                      from-str))))
    (cond
     (from-beginning
      (goto-char (point-min)))

     (backward
      (end-of-line))

     (t
      (beginning-of-line)))

    (query-replace-regexp from-str to-str nil nil nil backward)))

(defun fc-ergo-restore ()
  "Restore ergo state."
  (interactive)

  (deactivate-mark)
  (run-hooks '*fc-ergo-restore-hook*)
  (fc-highlight-cursor))

(fc-load 'ace-window
  :autoload t
  :after (setf aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(fc-load 'expand-region
  :idle t
  :before (autoload #'er/mark-symbol "expand-region")
  :autoload t)

(fc-install 'imenu-list 'swiper)

(fc-load 'move-text
  :autoload t
  :after (move-text-default-bindings))

(fc-load 'avy
  :autoload t
  :after (progn
           (setf avy-background t)))

(fc-load 'deft
  :autoload t
  :after
  (progn
    (add-to-list '*fc-modal-exclude-modes*
                 'deft-mode)

    (setq deft-default-extension "org"
          deft-extensions '("org")
          deft-directory "~/org"
          deft-recursive t
          deft-use-filename-as-title nil
          deft-use-filter-string-for-filename t
          deft-file-naming-rules '((noslash . "-")
                                   (nospace . "-")
                                   (case-fn . downcase))
          deft-text-mode 'org-mode))
  :bind '((deft-mode-map
            ("<escape>" quit-window)
            ("C-j" next-line)
            ("C-k" previous-line)
            )))

(defun fc-switch-layout ()
  "Switch layout."
  (let ((key (read-char "Layout: ")))
    (fc-layout-switch (format "%c" key))))

(defun fc-ergo-prefix-visual-feedback ()
  "Setup ergo prefix mode ui."
  (interactive)

  (if *fc-ergo-prefix*
      (fc-modal-set-cursor-color *fc-ergo-pre-cursor-color*)
    (fc-modal-set-cursor-color *fc-modal-command-cursor-color*)))

(defun fc-ergo-prefix-on ()
  "Ergo prefix mode on."
  (interactive)

  (fc-toggle-var '*fc-ergo-prefix*)
  (fc-ergo-prefix-visual-feedback)
  (force-mode-line-update)

  (cond
   ((and (not *fc-ergo-prefix*)
         *fc-ergo-prefix-timer*)
    (cancel-timer *fc-ergo-prefix-timer*)
    (setf *fc-ergo-prefix-timer* nil))

   (*fc-ergo-prefix*
    (setf *fc-ergo-prefix-timer*
          (fc-idle-delay-task 'fc-ergo-prefix-off
                              *ergo-prefix-timeout*)))))

(defun fc-ergo-prefix-off (&rest _rest)
  "Ergo prefix mode off."
  (when *fc-ergo-prefix*
    (cancel-timer *fc-ergo-prefix-timer*)
    (setf *fc-ergo-prefix* nil
          *fc-ergo-prefix-timer* nil)
    (fc-ergo-prefix-visual-feedback)
    (force-mode-line-update)))

(defun fc-escape-key ()
  "Escape key function."
  (interactive)

  (cond
   ((active-minibuffer-window)
    (if (minibufferp (current-buffer))
        (progn
          (minibuffer-keyboard-quit)
          (other-window 1)
          (deactivate-mark))
      (with-selected-window (active-minibuffer-window)
        (minibuffer-keyboard-quit))))

   (iedit-mode
    (iedit-quit)
    (fc-modal-global-mode 1))

   ((region-active-p)
    (kill-region (region-beginning) (region-end)))

   (fc-modal-global-mode
    (fc-modal-global-mode -1))

   ((not fc-modal-global-mode)
    (fc-modal-global-mode 1))))

(cl-defun fc-tab-key (&optional (indent-func #'indent-for-tab-command))
  "Tab key function.
INDENT-FUNC: function for indent."
  (interactive)

  (cond
   ((and (buffer-modified-p)
         (not fc-modal-global-mode)
         (looking-back "y" (- (point) 10)))
    (backward-delete-char-untabify 1)
    (unless (yas-expand)
      (insert "y")))

   ((looking-at "[\"'(){}\\[]\\|\\]")
    (forward-char))

   ((looking-at-p "[[:space:]]*$")
    (if fc-modal-global-mode
        (delete-blank-lines)
      (indent-for-tab-command)))

   ((or fc-modal-global-mode
        (not *fc-dev-mode*)
        (not (looking-at "$")))
    (fc-funcall indent-func))

   ;; only do company when <tab> at the end of the line
   ((looking-at ".?$")
    (company-complete))))

(cl-defun fc-basic-key ()
  "Basic key function."
  (interactive)

  (cond
   (*fc-ergo-prefix*
    (fc-modal-head-key "Basic" '*ergo-basic-map*))

   ((region-active-p)
    (fc-region (region-beginning) (region-end)
      (goto-char (point-min))
      (fc-modal-head-key
       "Basic"
       '*ergo-basic-map*
       *ergo-head-key-timeout*)))

   (t
    (scroll-down-command))))

(cl-defun fc-ctrl-enter-key ()
  "Ctrl-Enter key function."
  (interactive)

  (end-of-line)
  (newline-and-indent)

  (when fc-modal-global-mode
    (fc-modal-global-mode -1)))

;; shell
(defalias 'fc-show-hide-term (fc-manual (fc-show-hide-buffer "*ansi-term*"
                                                             (lambda () (ansi-term "/bin/bash")))))
(defalias 'fc-show-hide-eshell (fc-manual (fc-show-hide-buffer "*eshell*"
                                                               #'eshell)))

;; movement
(defun fc-beginning-of-line ()
  "Jump to beginning of line."
  (interactive)

  (fc-funcall (if (zerop (current-column))
                  'beginning-of-line-text
                'beginning-of-line)))

(defun fc-begin-of-func ()
  "Jump to begin of func."
  (interactive)

  (pcase major-mode
    ((guard (derived-mode-p 'prog-mode)) (beginning-of-defun))
    ('diff-mode (diff-file-prev))
    (_ (if outline-minor-mode
           (outline-previous-heading)
         (backward-sentence)))))

(defun fc-end-of-func ()
  "Jump to end of func."
  (interactive)

  (pcase major-mode
    ((guard (derived-mode-p 'prog-mode)) (end-of-defun))
    ('diff-mode (diff-file-next))
    (_ (if outline-minor-mode
           (outline-next-heading)
         (forward-sentence)))))

(defun fc-begin-of-semantic ()
  "Jump to begin of semantic."
  (interactive)

  (pcase major-mode
    ((or 'c-mode 'c++mode) (c-beginning-of-statement-1))
    ((guard (derived-mode-p 'prog-mode)) (beginning-of-sexp))
    ('org-mode (outline-next-visible-heading -1))
    (_ (if outline-minor-mode
           (outline-previous-heading))
       (backward-sentence))))

(defun fc-end-of-semantic ()
  "Jump to end of semantic."
  (interactive)

  (pcase major-mode
    ((or 'c-mode 'c++mode) (c-end-of-statement))
    ((guard (derived-mode-p 'prog-mode)) (end-of-sexp))
    ('org-mode (outline-next-visible-heading 1))
    (_ (if outline-minor-mode
           (outline-next-heading))
       (forward-sentence))))

(defun fc-find-viewer-window ()
  (car
   (fc-first-window (with-current-buffer (cdr it)
                      fc-viewer-minor-mode))))

(defun fc-navi-prev ()
  "Navi previous."
  (interactive)

  (cond
   ((eql major-mode 'image-mode)
    (call-interactively 'image-previous-file))

   ((fc-find-viewer-window)
    (with-selected-window (fc-find-viewer-window)
      (scroll-down)))

   ((fc--next-error-find-buffer)
    (call-interactively 'previous-error))

   ((one-window-p)
    (scroll-down-command))

   (t
    (scroll-other-window '-))))

(defun fc-navi-next ()
  "Navi next."
  (interactive)

  (cond
   ((eql major-mode 'image-mode)
    (call-interactively 'image-next-file))

   ((fc-find-viewer-window)
    (with-selected-window (fc-find-viewer-window)
      (scroll-up)))

   ((fc--next-error-find-buffer)
    (call-interactively 'next-error))

   ((one-window-p)
    (scroll-up-command))

   (t
    (scroll-other-window))))

(defun fc-backward ()
  "Buffer backward."
  (interactive)

  (pcase major-mode
    ('Info-mode
     (Info-history-back))

    ('org-mode
     (org-mark-ring-goto))

    ('eww-mode
     (eww-back-url))

    ((guard (derived-mode-p 'prog-mode))
     (pop-tag-mark))

    (_
     (scroll-down-command))))

;; mark
(defun fc-mark-func ()
  "Mark semanticly."
  (interactive)

  (pcase major-mode
    ((guard (derived-mode-p 'prog-mode))
     (mark-defun))

    ('org-mode
     (org-mark-subtree))

    (_
     (backward-sentence)
     (mark-end-of-sentence 1))))

(defun fc-mark-quote ()
  "Mark quoteation."
  (interactive)

  (let ((start (line-beginning-position))
        (end (line-end-position)))
    (save-excursion
      (when (re-search-backward "[“”\"'‘’（）()]" start t)
        (setf start (point))))

    (save-excursion
      (when (re-search-forward "[“”\"'‘’（）()]" end t)
        (setf end (point))))

    (set-mark start)
    (goto-char end)
    (exchange-point-and-mark)))

;; dictionary
(defun fc-translate-word ()
  "Translate word."
  (interactive)

  (let ((words (if *fc-ergo-prefix*
                   (fc-current-thing :confirm t :ext nil)
                 (fc-current-thing :ext nil))))
    (when (fc-not-void-p words)
      (fc-dict-lookup words))))

(defun fc-mark-point-to-beginning-of-line (&optional arg)
  "Mark from current point to the line beginning.
ARG: lines."
  (interactive "P")

  (set-mark (point))
  (beginning-of-line arg))

(defun fc-mark-point-up-to-char (arg char)
  "Mark from current point to char.
ARG: direction.
CHAR: target character."
  (interactive "p\ncMark up to char: ")

  (set-mark (point))
  (let ((direction (if (>= arg 0) 1 -1)))
    (forward-char direction)
    (unwind-protect
        (search-forward (char-to-string char) nil nil arg)
      (backward-char direction))))

(defun fc-mark-point-to-end-of-line (&optional arg)
  "Mark from current point to the end of line.
ARG: lines."
  (interactive "P")

  (set-mark (point))
  (end-of-line arg)
  (exchange-point-and-mark))

(defun fc-mark-line (&optional arg)
  "Mark line.
ARG: lines."
  (interactive "P")

  (set-mark (line-beginning-position))
  (end-of-line arg)
  (when (looking-at "\n")
    (forward-char))
  (exchange-point-and-mark))

(defun fc-mark-word (&optional arg)
  "Makr word.
ARG: words."
  (interactive "P")

  (unless (looking-at "\\b")
    (backward-word))
  (mark-word arg))

(defun fc-mark-point-to-beginning-of-buffer ()
  "Mark from current point to the beginning of buffer."
  (interactive)

  (set-mark (point))
  (goto-char (point-min)))

(defun fc-mark-point-to-end-of-buffer ()
  "Mark from current point to the end of buffer."
  (interactive)

  (set-mark (point))
  (goto-char (point-max))
  (exchange-point-and-mark))

(defun fc-show-time ()
  "Show current time."
  (interactive)

  (fc-popup-tip (current-time-string) :timeout 3))

(defmacro fc-delete-key (&optional mark-func not-save)
  "Delete region.
MARK-FUNC: mark the region to be deleted.
NOT-SAVE: save to KILL-RING or not."
  `(lambda ()
     (interactive)

     (fc-funcall ,mark-func)
     (fc-funcall (if ,not-save
                     'delete-region
                   'kill-region))))

(defmacro fc-change-key (&optional mark-func not-save)
  "Change region.
MARK-FUNC: mark the region to be changed.
NOT-SAVE: save to KILL-RING or nor."
  `(lambda ()
     (interactive)

     (fc-funcall ,mark-func)
     (fc-funcall (if ,not-save
                     'delete-region
                   'kill-region))
     (save-excursion
       (indent-for-tab-command))
     (fc-modal-global-mode -1)))

(defun fc-narrow-widen (arg)
  "Narrow/widen automatically.
ARG: paragraphs"
  (interactive "P")

  (cond
   ((use-region-p)
    (narrow-to-region (region-beginning) (region-end))
    (deactivate-mark))

   ((buffer-narrowed-p)
    (widen))

   ((derived-mode-p 'prog-mode)
    (narrow-to-defun))

   ((eq major-mode 'diff-mode)
    (diff-restrict-view t))

   (t
    (mark-paragraph arg)
    (narrow-to-region (region-beginning) (region-end))
    (deactivate-mark))))

(defun fc-delete-char (n)
  "Delete char.
N: number."
  (interactive "p")

  (if (eolp)
      (backward-delete-char n)
    (delete-char n)))

(defun fc-sys-info ()
  "Create sys info."
  (let ((user (format "%s@%s" user-login-name (system-name)))
        (loc (cond
              (*fc-location-work* "work")
              (*fc-location-home* "home")
              (t "unknown"))))
    (if *is-gui*
        `(
          ("Emacs" ,(format "%s, DPI %d, fringe %d"
                            emacs-version
                            (fc-display-ppi) *fc-fringe-width*))
          ("User" ,user)
          ("Loc" ,loc)
          ("Font" ,(format "%s, %s, %d"
                           *fc-default-font*
                           *fc-font-weight-of-default*
                           *fc-font-height*))
          ("Theme" ,(format "%s, %s"
                            *fc-current-theme*
                            (if (fboundp 'fc-modeline-mode)
                                "fc-modeline"
                              (symbol-name powerline-default-separator)))))
      `(
        ("Emacs" ,(format "%s, colorful %S" emacs-version *is-colorful*))
        ("User" ,user)
        ("Loc" ,loc)
        ("Theme" ,*fc-current-theme*)))))

(defun fc-process-info ()
  "Return list of process info."
  (cl-loop for i in (and (fboundp 'process-list)
                         (process-list))
           for j from 1
           collect (list (format "Process %d" j)
                         (process-name i))))

(defun fc-convert-info (info)
  "Convert info to string.
INFO: info obj."
  (--reduce-from (concat acc
                         "│"
                         (fc-text (format "%11s" (cl-first it))
                                  :face '(:foreground "tomato"))
                         (format " : %s\n" (fc-string (cl-second it))))
                 ""
                 info))

(defun fc-show-info (&rest args)
  "Show info.
ARGS: list of infos."
  (--reduce-from (concat acc (fc-convert-info it))
                 ""
                 args))

(defun fc-open-my-index-org ()
  "Open index org file."
  (interactive)

  (find-file (concat *fc-cloud-home* "index.org")))

(cl-defun fc-recover-revert-buffer ()
  "Recover current buffer."
  (if (file-exists-p (make-auto-save-file-name))
      (recover-file buffer-file-name)
    (revert-buffer t t)))

(cl-defun fc-fast-switch-window ()
  "Fast switch window."
  (interactive)

  (cond
   ((one-window-p)
    (fc-switch-to-recent-buffer))

   ;; two or three windows
   ((< (length (window-list)) 4)
    (other-window 1))

   ;; more than three windows
   (t
    (ace-window 0)))

  (fc-highlight-cursor))

(cl-defun fc-toggle-hex-mode ()
  "Toggle hex mode."
  (interactive)

  (if (eq major-mode 'hexl-mode)
      (hexl-mode-exit)
    (when (or (not (fc-big-buffer-p))
              (fc-user-confirm "Hexlify" nil))
      (hexl-mode))))

(defun fc-rename-buffer-and-file ()
  "Rename current file and buffer name."
  (let ((name buffer-file-name)
        (new-name (read-file-name "Move to : ")))
    (if (vc-backend name)
        (vc-rename-file name new-name)
      (rename-file name new-name)
      (set-visited-file-name new-name))))

(defun fc-goto-last-change ()
  (--first
   (when it
     (cond
      ((and (consp it) (integerp (car it)) (integerp (cdr it)))
       (goto-char (cdr it)))

      ((and (consp it) (stringp (car it)))
       (goto-char (abs (cdr it))))

      ((integerp it)
       (goto-char it))))
   buffer-undo-list)
  "")

;; global mode
(fc-unbind-keys '("C-x C-c"
                  "C-M-i"))

(fc-bind-keys `(("RET" newline-and-indent)
                ("<escape>" fc-escape-key)
                ("TAB" fc-tab-key)
                ("M-x" fc-M-x)
                ("C-<return>" fc-ctrl-enter-key)
                ("C-x b" ivy-switch-buffer)
                ("C-@" fc-escape-key)
                ("C-<SPC>" fc-escape-key)
                ("C-." ,(fc-manual (fc-find-definitions :apropos t)))
                ("C-_" fc-fast-switch-window) ;; it is C-/ in the console
                ("C-;" fc-fast-switch-window)))

(defmacro fc-head-key (prompt keymap)
  "Run head key.
PROMPT: user prompt.
KEYMAP: keymap to run."
  `(lambda ()
     (interactive)

     (fc-modal-head-key ,prompt ,keymap :timeout *ergo-head-key-timeout*)))

(defmacro fc-head-key-repeat (prompt keymap)
  "Run head key.
PROMPT: user prompt.
KEYMAP: keymap to run."
  `(lambda ()
     (interactive)

     (fc-modal-head-key ,prompt ,keymap :timeout *ergo-head-key-timeout* :repeat t)))

(defconst *fc-common-function-keys*
  `(("<f1>"  neotree-toggle)
    ("C-<f1>" menu-bar-mode)
    ("<f2>"  fc-open-my-index-org)
    ("<f3>"  fc-show-hide-eshell)
    ("<f4>"  fc-show-hide-note)
    ("<f5>"  fc-spotlight)
    ("<f6>")
    ("<f7>"  ,(fc-manual (fc-player--previous *fc-player*)))
    ("<f8>"  ,(fc-manual (fc-player--play-pause *fc-player*)))
    ("<f9>"  ,(fc-manual (fc-player--next *fc-player*)))
    ("<f10>" ,*fc-mute-volume*)
    ("<f11>" ,*fc-decrease-volume*)
    ("<f12>" ,*fc-increase-volume*))
  "Common functions.")

(defconst *fc-mac-function-keys*
  `(("<f1>"  ,*fc-decrease-display-brightness*)
    ("<f2>"  ,*fc-increase-display-brightness*)
    ("<f3>"  fc-buffers-list)
    ("<f4>"  fc-program)
    ("<f5>"  fc-spotlight)
    ("<f6>")
    ("<f7>"  ,(fc-manual (fc-player--previous *fc-player*)))
    ("<f8>"  ,(fc-manual (fc-player--play-pause *fc-player*)))
    ("<f9>"  ,(fc-manual (fc-player--next *fc-player*)))
    ("<f10>" ,*fc-mute-volume*)
    ("<f11>" ,*fc-decrease-volume*)
    ("<f12>" ,*fc-increase-volume*))
  "Mac functions.")

(let ((function-keys '(*fc-common-function-keys* *fc-mac-function-keys*)))
  (fc-bind-keys (symbol-value (cl-first function-keys)))

  (defun fc-switch-function-keys ()
    (interactive)

    (fc-bind-keys (symbol-value (cl-first function-keys)))
    (message "Set functions to %s" (s-trim (get (cl-first function-keys) 'variable-documentation)))

    (setf function-keys (-rotate 1 function-keys))))

(fc-bind-keys `(("M-1" fc-split-unsplit-window)
                ("M-2" fc-split-window)
                ("M-3" fc-show-hide-eshell)
                ("M-4" ivy-switch-buffer)
                ("M-5" toggle-frame-fullscreen)

                ("M-6" fc-flycheck)
                ("M-7" compile)
                ("M-8" fc-proj-find-file)
                ("M-9" fc-previous-bookmark)
                ("M-0" fc-next-bookmark)))

;; movement
(fc-bind-keys '(("M-i" previous-line)
                ("M-k" next-line)
                ("M-j" backward-char)
                ("M-l" forward-char)

                ("C-M-j" backward-word)
                ("M-u" backward-word)
                ("C-M-l" forward-word)
                ("M-o" forward-word)

                ("C-M-i" scroll-down-command)
                ("C-M-k" scroll-up-command)

                ("<home>" fc-beginning-of-line)
                ("C-a" fc-beginning-of-line)
                ("<end>" end-of-line)
                ("C-e" end-of-line)

                ("M-g" goto-line)
                ("M-p" fc-match-paren)))

;; misc
(fc-bind-keys `(("C-M-s" fc-toggle-hide-show-all)
                ("M-s" fc-toggle-hide-show)

                ("M-b" pop-tag-mark)
                ("C-<tab>" fc-fast-switch-window)
                ("<backtab>" fc-buffers-list)
                ("<S-C-iso-lefttab>" previous-buffer)
                ("C-z" undo)

                ;; ("M-[" fc-navi-prev)
                ;; ("M-]" fc-navi-next)

                ("M-h" backward-delete-char)
                ("C-|" fc-next-input-method)

                ("M-." fc-find-definitions)
                ("M-," fc-find-references)))

(defconst *ergo-player-map*
  (fc-make-keymap
   `(
     ("SPC" fc-player-user-select)
     ("i" ,(fc-manual (fc-player--volume-up *fc-player*)))
     ("k" ,(fc-manual (fc-player--volume-down *fc-player*)))
     ("j" ,(fc-manual (fc-player--previous *fc-player*)))
     ("l" ,(fc-manual (fc-player--next *fc-player*)))
     ("o" ,(fc-manual (fc-player--show-metadata *fc-player*)))
     ("p" ,(fc-manual (fc-player--play-pause *fc-player*)))
     ("s" ,(fc-manual (fc-player--app *fc-player*)))
     )
   "ergo-player-map")
  "KEYS i: vol up  k: vol down  j: prev  l: next  o: meta  p: pp  SPC: select player.")

(defconst *fc-common-key-doc*
  "KEYS a: to beginning  b: whole buffer  c: to char  e: to end  f: func  l: line  m: symbil  p: paragraph  q: quote  w: word  ^: to beginnning of buf  $: to end of buf.")

(defconst *ergo-change-map*
  (fc-make-keymap
   `(("a" ,(fc-change-key 'fc-mark-point-to-beginning-of-line))
     ("b" ,(fc-delete-key 'mark-whole-buffer))
     ("c" ,(fc-change-key 'fc-mark-point-up-to-char))
     ("e" ,(fc-change-key 'fc-mark-point-to-end-of-line))
     ("f" ,(fc-change-key 'fc-mark-func))
     ("l" ,(fc-change-key 'fc-mark-line))
     ("p" ,(fc-change-key 'mark-paragraph))
     ("q" ,(fc-change-key 'fc-mark-quote))
     ("w" ,(fc-change-key 'fc-mark-word))

     ("A" ,(fc-change-key 'fc-mark-point-to-beginning-of-line t))
     ("B" ,(fc-delete-key 'mark-whole-buffer t))
     ("C" ,(fc-change-key 'fc-mark-point-up-to-char t))
     ("E" ,(fc-change-key 'fc-mark-point-to-end-of-line t))
     ("F" ,(fc-change-key 'fc-mark-func t))
     ("L" ,(fc-change-key 'fc-mark-line t))
     ("P" ,(fc-change-key 'mark-paragraph t))
     ("Q" ,(fc-change-key 'fc-mark-quote t))
     ("W" ,(fc-change-key 'fc-mark-word t))

     ("^" ,(fc-change-key 'fc-mark-point-to-beginning-of-buffer))
     ("$" ,(fc-change-key 'fc-mark-point-to-end-of-buffer))
     )
   "ergo-change-map")
  *fc-common-key-doc*)

(defconst *ergo-delete-map*
  (fc-make-keymap
   `(("a" ,(fc-delete-key 'fc-mark-point-to-beginning-of-line))
     ("b" ,(fc-delete-key 'mark-whole-buffer))
     ("c" ,(fc-delete-key 'fc-mark-point-up-to-char))
     ("d" ,(fc-delete-key 'fc-mark-line))
     ("e" kill-line)
     ("f" ,(fc-delete-key 'fc-mark-func))
     ("l" ,(fc-delete-key 'fc-mark-line))
     ("p" ,(fc-delete-key 'mark-paragraph))
     ("q" ,(fc-delete-key 'fc-mark-quote))
     ("w" ,(fc-delete-key 'fc-mark-word))

     ("A" ,(fc-delete-key 'fc-mark-point-to-beginning-of-line t))
     ("B" ,(fc-delete-key 'mark-whole-buffer t))
     ("C" ,(fc-delete-key 'fc-mark-point-up-to-char t))
     ("D" ,(fc-delete-key 'fc-mark-line t))
     ("E" ,(fc-delete-key 'fc-mark-point-to-end-of-line t))
     ("F" ,(fc-delete-key 'fc-mark-func t))
     ("L" ,(fc-delete-key 'fc-mark-line t))
     ("P" ,(fc-delete-key 'mark-paragraph t))
     ("Q" ,(fc-delete-key 'fc-mark-quote t))
     ("W" ,(fc-delete-key 'fc-mark-word t))

     ("^" ,(fc-delete-key 'fc-mark-point-to-beginning-of-buffer))
     ("$" ,(fc-delete-key 'fc-mark-point-to-end-of-buffer))
     )
   "ergo-delete-map")
  *fc-common-key-doc*)

(defconst *ergo-basic-map*
  (fc-make-keymap
   `(("c" ,(fc-cond-key :normal 'capitalize-word
                        :region 'capitalize-region))
     ("l" ,(fc-cond-key :normal 'downcase-word
                        :region 'downcase-region))
     ("m" fc-merge-short-line)
     ("p" ,(fc-manual
            (when buffer-file-name
              (kill-new buffer-file-name))))
     ("u" ,(fc-cond-key :normal 'upcase-word
                        :region 'upcase-region))
     ("D" unix2dos)
     ("M" fc-merge-all-line)
     ("U" dos2unix)
     )
   "ergo-basic-map")
  "KEYS c: capitalize  l: downcase  m: merge  p: copy path  u: upcase  D: to-dos  U: to-unix.")

(defconst *ergo-goto-map*
  (fc-make-keymap
   `(("<SPC>" counsel-imenu)

     ("7" ,(fc-manual (fc-show-hide-buffer "*compilation*")))

     ("a" ,(fc-mode-key
            `((image-mode . image-bob)
              (_ . beginning-of-buffer))))
     ("b" ,(fc-cond-key :normal 'fc-goto-favorite-buffer
                        :prefix 'fc-add-remove-favorite-buffer))
     ("c" ,(fc-manual (recenter (/ (window-height) 2))))
     ("e" ,(fc-mode-key
            `((image-mode . image-eob)
              (_ . end-of-buffer))))
     ("g" ,(fc-manual
            (bury-buffer)
            (fc-switch-to-buffer-re "\\.c$\\|\\.el$\\|\\.go$\\|\\.py$")))

     ("h" fc-goto-last-change)

     ("i" windmove-up)
     ("j" windmove-left)
     ("k" windmove-down)
     ("l" windmove-right)

     ("m" fc-switch-layout)
     ("n" ,(fc-cond-key :normal 'fc-switch-next-error-buffer))

     ("o" next-buffer)
     ("p" ,(fc-manual (goto-char (read-number "Point : "))))
     ("q" ,(fc-manual (fc-switch-to-buffer
                       "Modified buffers"
                       (fc-list-buffer :modified t))))
     ("r" fc-recentf)
     ("s" ace-swap-window)
     ("t" ,(fc-manual (fc-tag-list)))
     ("u" previous-buffer)
     ("x" ,(fc-manual (fc-switch-to-buffer "Select view"
                                           (fc-rm-current-buf (fc-viewer-list-buffer)))))
     ("w" fc-buffers-list)

     ("[" ,(fc-manual (recenter 1)))
     ("]" ,(fc-manual (recenter -1)))

     ("," flycheck-previous-error)
     ("." flycheck-next-error)

     (";" goto-line))
   "ergo-goto-map")
  "KEYS SPC: imenu  a: to begin of buffer  b: favorite buffer  c: recenter  e: to end of buffer  g: recent code  h: last change  i: above window  k:  beneath window  j: left window  l: right window  m: switch layout  n: show last navi buf  q: modified buffer  r: all recent file  t: tag  w: all buffer  ;: goto line.")

(defconst *ergo-goto-region-map*
  (fc-make-keymap
   `(("a" beginning-of-buffer)
     ("e" end-of-buffer))
   "ergo-goto-region-map")
  "KEYS a: to begin of buffer  e: to end of buffer.")

(defconst *ergo-gtd-map*
  (fc-make-keymap
   `(
     ("3" org-agenda-list)
     ("a" org-agenda)
     ("c" org-capture)
     ("l" org-store-link)
     ("m" org-tags-view)
     ("t" org-todo-list))
   "ergo-gtd-map")
  "KEYS a: agenda  c: capture  l: store link  m: view tag  t: todo list.")

(cl-defun fc-ergo-which-function ()
  "Which function wrapper."
  (interactive)

  (message "Current: %s"
           (which-function)))

(cl-defun fc-describe-function ()
  "Describe function."
  (cond
   (lsp-mode
    (lsp-ui-doc-show))

   (t
    (describe-function))))

(defconst *ergo-help-map*
  (fc-make-keymap
   `(("SPC" fc-help-portal)
     ("`" ,(fc-manual (fc-play-sound 'cheerup)))
     ("a" apropos)
     ("b" ,(fc-manual (fc-show-info (fc-file-info)
                                    (fc-buffer-info))))
     ("c" describe-char)
     ("f" ,(fc-mode-key
            `(
              (emacs-lisp-mode . describe-function)
              (go-mode . godef-describe)
              (python-mode . python-eldoc-function)
              ((latex-mode markdown-mode org-mode) . fc-ergo-which-function)
              (_ . fc-describe-function))))
     ("h" fc-modal-input)
     ("i" info)
     ("k" describe-key)
     ("m" man)
     ("o" org-info)
     ("r" ,(fc-manual (fc-pop-buf "*Help*")))
     ("s" ,(fc-manual (fc-show-info (fc-sys-info)
                                    (fc-process-info))))
     ("v" describe-variable)
     ("y" yas-describe-tables)
     ("F" describe-face)
     ("M" describe-mode))
   "ergo-help-map")
  "KEYS SPC: portal  a: apropros  b: buffer info  c: char  f: func  i: info  k: key  m: manul  o: org info  r: show *Help*  s: sys info  v: var  y: yas  F: face  M: mode.")

(defconst *ergo-mark-map*
  (fc-make-keymap
   `(("a" fc-mark-point-to-beginning-of-line)
     ("b" mark-whole-buffer)
     ("c" fc-mark-point-up-to-char)
     ("e" fc-mark-point-to-end-of-line)
     ("f" fc-mark-func)
     ("l" fc-mark-line)
     ("m" er/mark-symbol)
     ("p" mark-paragraph)
     ("q" fc-mark-quote)
     ("w" fc-mark-word)

     ("^" fc-mark-point-to-beginning-of-buffer)
     ("$" fc-mark-point-to-end-of-buffer))
   "ergo-mark-map")
  *fc-common-key-doc*)

(defconst *ergo-mode-map*
  (fc-make-keymap
   `(
     ("b" toggle-scroll-bar)
     ("g" golden-ratio-mode)
     ("h" highlight-changes-mode)
     ("l" display-line-numbers-mode)
     ("n" nyan-mode)
     ("r" rainbow-delimiters-mode)
     ("t" fc-toggle-transparency)
     ("v" visual-line-mode)
     ("w" whitespace-mode)
     ("z" zoom-mode)

     ("A" global-anzu-mode)
     ("H" global-highlight-changes-mode)
     ("L" global-display-line-numbers-mode)
     ("S" ,(fc-manual (fc-toggle-var '*fc-enable-sound*)
                      (message (if *fc-enable-sound*
                                   "Sound enabled"
                                 "Sound disabled"))))
     ("T" tabbar-mode)
     ("V" global-visual-line-mode)
     )
   "ergo-mode-map")
  "KEYS  b: scroll bar  g: golden ratio  h: highlight changs  l: linum  n: nyan  r: rainbow-delimiter  t: transparency  v: visual line  A: anzu  H: highlight changs  L: linum  S: sound  T: tabbar  V: global visual line.")

(defconst *ergo-quick-map*
  (fc-make-keymap
   `(("SPC" fc-app-portal)
     ("a" align)
     ("b" ,*fc-undef-key*)
     ("c" ,(fc-cond-key :normal 'quick-calc
                        :region (fc-manual (calc-eval (fc-current-thing)))))
     ("d" fc-dev-mode-toggle)
     ("f" ,(fc-cond-key :normal 'fc-find-files
                        :proj (fc-manual (fc-proj-find-file default-directory))))
     ("h" fc-toggle-hex-mode)

     ("i" insert-file)
     ("j" deft)
     ("k" fc-flycheck)
     ("l" imenu-list-smart-toggle)

     ("m" fc-select-multi-buffer-func)
     ("n" ,(fc-manual (switch-to-buffer (generate-new-buffer "Untitled"))
                      (text-mode)))
     ("o" fc-occur-dwim)
     ("p" fc-switch-function-keys)
     ("q" ,(fc-cond-key :normal 'fc-proj-open
                        :region 'fc-proj-query-rename
                        :proj 'fc-select-proj-func))
     ("r" fc-recover-revert-buffer)
     ("s" save-buffer)
     ("t" ,(fc-cond-key :normal 'fc-show-time
                        :region (fc-manual
                                 (fc-speak (buffer-substring
                                            (region-beginning)
                                            (region-end))))))
     ("u" fc-toggle-window-maximize)
     ("v" fc-tomato)
     ("w" write-file)
     ("x" fc-viewer-toggle)
     ("z" flush-lines)

     ("B" )
     ("C" calendar)
     ("D" dired-jump)
     ("F" fc-fmt-buffer)
     ("I" fc-insert-signature)
     ("J" deft-find-file)
     ("L" fc-screen-saver)
     ("M" fc-rename-buffer-and-file)
     ("R" read-only-mode)
     ("S" save-some-buffers)
     ("T" ,(fc-manual (untabify (point-min) (point-max))))
     ("W" fc-forecast)
     ("X" fc-reading-toggle)

     (";" fc-open-in-system)
     (":" isearch-forward)
     ("'" ,(fc-cond-key :normal 'fc-show-hide-note
                        :region (fc-manual
                                 (fc-insert-note
                                  (fc-current-thing)))))
     )
   "ergo-quick-map")
  "KEYS a: align  c: calc  d: dev mode  f: find file  h: hex mode  i: insert file  j: deft  k: flycheck  l: imenu list  m: multiple  n: new buffer  o: occur  r: recover buffer  s: save  t: time  u: (un)maximize  v: tomato  w: save as  x: reading  z: flush lines  B: none  C: calendar  D: open dir  F: format  I: insert signature  L: screen saver  M: rename file  R: readonly  S: save buffers  W: forecast  X: reading.")

(cl-defmacro fc--adjust-window-size (horizontally step)
  "Adjust window size.
HORIZONTALLY: horizaonta or vertical.
STEP: pixels."
  `(lambda ()
     (interactive)

     (if ,horizontally
         (enlarge-window-horizontally ,step))
     (enlarge-window ,step)))

(defconst fc--fast-enlarge-v (fc--adjust-window-size nil 5))
(defconst fc--fast-reduce-v (fc--adjust-window-size nil -5))
(defconst fc--enlarge-v (fc--adjust-window-size nil 1))
(defconst fc--reduce-v (fc--adjust-window-size nil -1))

(defconst fc--fast-enlarge-h (fc--adjust-window-size t 5))
(defconst fc--fast-reduce-h (fc--adjust-window-size t -5))
(defconst fc--enlarge-h (fc--adjust-window-size t 1))
(defconst fc--reduce-h (fc--adjust-window-size t -1))

(fc-modal-mark-repeat
 'fc--fast-enlarge-v
 'fc--fast-reduce-v
 'fc--fast-enlarge-h
 'fc--fast-reduce-h
 'fc--enlarge-v
 'fc--reduce-v
 'fc--enlarge-h
 'fc--reduce-h)

(defconst *ergo-prefix-quick-map*
  (fc-make-keymap
   `(("c" rpn-calc)

     ("d" ,(fc-manual (fc-load-desktop)))
     ("e" fc-new-buffer-with-template)

     ("i" fc--fast-enlarge-v)
     ("j" fc--fast-reduce-h)
     ("k" fc--fast-reduce-v)
     ("l" fc--fast-enlarge-h)

     ("q" ,(fc-manual (when (fc-user-confirm "Quit Emacs")
                        (fc-save-desktop)
                        (save-buffers-kill-emacs))))
     ("r" ,(fc-manual (when (fc-user-confirm "Restart Emacs")
                        (fc-save-desktop)
                        (restart-emacs))))
     ("t" fc-dark-theme)
     ("w" ,(fc-manual (fc-theme-auto-select *fc-work-themes*)))
     ("z" suspend-emacs)

     ("I" fc--enlarge-v)
     ("J" fc--reduce-h)
     ("K" fc--reduce-v)
     ("L" fc--enlarge-h)

     ("T" fc-deep-dark-theme)
     ("W" ,(fc-manual (fc-theme-auto-select *fc-work-dark-themes*)))
     )
   "ergo-prefix-quick-map")
  "KEYS c: rpn calc  d: load desktop  e: new buf with tmpl  i: vertically enlarge  j: horizontally enlarge  k: vertically reduce  l: horizontally reduce  t: dark theme  w: zenburn  z: suspend  T: deep dark theme  W: dark theme.")

(defconst *ergo-vc-map*
  (fc-make-keymap
   `(("a" fc-git-amend)
     ("c" vc-next-action)
     ("h" git-messenger:popup-message)
     ("j" ,(fc-manual (shell-command-to-string "fit -s")))
     ("k" fc-git-commit)
     ("l" vc-print-log)
     ("p" fc-git-pull)
     ("q" fc-git-push)
     ("r" fc-vc-rename-file)
     ("s" fc-git-add)
     ("u" vc-revert)
     ("v" vc-diff)
     ("x" magit-status)
     ("H" magit-blame)
     ("V" fc-git-diff-repo)
     )
   "ergo-vc-map")
  "KEYS a: amend  c: commit  h: popup history  j: repo status  k: commit  l: log  p: pull  q: push  r: rename  s: stage  u: revert  v: diff  x: magit  H: magit blame  V: diff repo.")

(defconst *ergo-layout-map*
  (fc-make-keymap
   `(("l" ,(fc-manual (let ((name (read-char "Load layout : " nil *ergo-head-key-timeout*)))
                        (if name (fc-layout-load name) (message "")))))
     ("q" fc-layout-push)
     ("p" fc-layout-pop)
     ("s" ,(fc-manual (let ((name (read-char "Save layout : " nil *ergo-head-key-timeout*)))
                        (if name (fc-layout-save name) (message ""))))))
   "ergo-layout-map")
  "KEYS l: load  p: pop  q: push  s: save.")

(defun fc-mode-func-key ()
  "Run function base on mode."
  (interactive)
  (let ((f (intern (format "fc-%s-func"
                           (symbol-name major-mode)))))
    (fc-funcall f :default 'fc-common-mode-func)))

(defun fc-jump-word-in-line ()
  "Jump to a word in current line."
  (interactive)
  (avy-goto-word-0 nil
                   (line-beginning-position)
                   (line-end-position)))

(defconst fc--player-func (fc-head-key-repeat
                           (concat "Player::"
                                   (fc-string *fc-player*))
                           '*ergo-player-map*))

(defun fc-player-func ()
  "Run player commands."
  (when (and (not *fc-player*)
             (fboundp #'fc-player-auto-select))
    (fc-player-auto-select))

  (if *fc-player*
      (fc-funcall fc--player-func)
    (message "Player not found")))

(cl-defun fc-hi-lock-p (regex)
  "Test if REGEX is highlighting.
REGEX: target regular expression."
  (assoc regex hi-lock-interactive-lighters))

(cl-defun fc-hi-lock-toggle (regex &key (auto t))
  "Toggle REGEX highlight state.
REGEX: target regular expression.
AUTO: auto select face."
  (if (fc-hi-lock-p regex)
      (unhighlight-regexp regex)

    (let ((hi-lock-auto-select-face auto))
      (highlight-regexp regex
                        (hi-lock-read-face-name)))))

;; normal mode
(fc-modal-keys
 `(
   ("1" ,(fc-cond-key :normal 'delete-other-windows
                      :prefix 'ace-delete-other-windows
                      :one 'fc-split-window))
   ("2" ,(fc-cond-key :normal (fc-manuals
                               #'fc-split-window
                               #'other-window
                               #'fc-switch-to-recent-buffer)
                      :prefix 'split-window-horizontally))
   ("3" ,(fc-cond-key :normal (fc-head-key "GTD" '*ergo-gtd-map*)
                      :prefix 'split-window-vertically))
   ("4" ,(fc-cond-key :normal 'ivy-switch-buffer
                      :region 'comment-dwim))
   ("5" toggle-frame-fullscreen)
   ("6" fc-toggle-window-maximize)
   ("7" ,(fc-cond-key :normal 'compile
                      :work 'fc-proj-build
                      :prefix 'compile))
   ("8" ,(fc-cond-key :normal 'fc-proj-open
                      :work 'fc-proj-find-file))
   ("9" ,(fc-manual (set-mark-command 0)))
   ("0" fc-ergo-prefix-on)

   ("a" ,(fc-mode-key
          `((image-mode . image-bol)
            (_ . fc-beginning-of-line))))
   ("b" ,(fc-cond-key :normal 'fc-backward
                      :region 'comment-dwim))
   ("C-b" scroll-down-command)

   ;; c := Change
   ("c" ,(fc-cond-key :normal (fc-head-key "Change" '*ergo-change-map*)
                      :region (fc-change-key)))

   ;; d := Delete
   ("d" ,(fc-cond-key :normal (fc-head-key "Delete" '*ergo-delete-map*)
                      :region 'kill-region))

   ("e" ,(fc-mode-key
          `((image-mode . image-eol)
            (_ . end-of-line))))
   ("f" ,(fc-cond-key :normal 'scroll-down-command
                      :region (fc-manual
                               (fc-modal-head-key
                                "Basic" '*ergo-basic-map*
                                :around
                                (lambda (func)
                                  (fc-region (region-beginning) (region-end)
                                    (when (> (point) (mark))
                                      (exchange-point-and-mark))
                                    (fc-funcall func)))))
                      :prefix (fc-manual
                               (fc-modal-head-key
                                "Basic prefix" '*ergo-basic-map*))))

   ;; g := Go/Global
   ("g" ,(fc-cond-key :normal (fc-head-key "Goto" '*ergo-goto-map*)
                      :region (fc-head-key "Goto" '*ergo-goto-region-map*)))

   ;; h := Help
   ("h" ,(fc-cond-key :normal (fc-head-key "Help" '*ergo-help-map*)
                      :region (fc-manual
                               (fc-hi-lock-toggle (fc-current-thing)))))

   ("i" previous-line)
   ("j" backward-char)
   ("k" next-line)
   ("l" forward-char)

   ;; m := Mark
   ("m" ,(fc-cond-key :normal (fc-head-key "Mark" '*ergo-mark-map*)
                      :region 'deactivate-mark
                      :prefix (fc-head-key "Mode" '*ergo-mode-map*)))

   ("n" fc-escape-key)
   ("o" ,(fc-cond-key :normal (fc-manual
                               (end-of-line)
                               (newline-and-indent)
                               (fc-modal-global-mode -1))
                      :region 'fc-occur-dwim))

   ;; p := Player
   ("p" ,(fc-cond-key :normal 'fc-match-paren
                      :prefix 'fc-player-func))

   ;; q := Quick/Quest
   ("q" ,(fc-cond-key :normal (fc-head-key "Quick"
                                           '*ergo-quick-map*)
                      :prefix (fc-head-key "Prefix Quick"
                                           '*ergo-prefix-quick-map*)))

   ("r" ,(fc-cond-key :normal 'fc-proj-recentf
                      :region (fc-manual (fc--query-replace))
                      :preregion (fc-manual (fc--query-replace
                                             :from-beginning t))))

   ("s" ,(fc-cond-key :normal 'fc-hs-toggle
                      :region 'fc-isearch-dwim
                      :prefix 'fc-toggle-hide-show-all))
   ("t" fc-translate-word)
   ("u" ,(fc-cond-key :normal 'fc-mode-func-key))
   ("v" ,(fc-cond-key :normal 'set-mark-command
                      :region (fc-manual (er/expand-region 1))))
   ("C-v" ,(fc-cond-key :normal 'er/mark-symbol
                        :region (fc-manual (er/expand-region -1))))
   ("w" ,(fc-cond-key :normal 'fc-buffers-list
                      :region 'delete-region
                      :proj 'fc-switch-within-project))
   ("x" ,(fc-cond-key :normal 'fc-delete-char
                      :region 'exchange-point-and-mark))
   ("y" ,(fc-cond-key :normal 'yank
                      :prefix 'counsel-yank-pop
                      :region (fc-manuals #'delete-region
                                          #'yank)
                      :preregion (fc-manuals #'delete-region
                                             #'counsel-yank-pop)))
   ("z" ,(fc-manual (push-mark (point))
                    (undo)))

   ("A" fc-begin-of-semantic)
   ("B" ,(fc-cond-key :normal 'fc-bookmark
                      :prefix (fc-manual
                               (let ((bm (fc-user-select
                                          "Select bookmark"
                                          (-map #'car bookmark-alist))))
                                 (when (and bm
                                            (fc-user-confirm
                                             (format
                                              "Delete bootmark '%s'"
                                              bm)))
                                   (bookmark-delete bm))))))

   ;; C := VC
   ("C" ,(fc-head-key "VC" '*ergo-vc-map*))

   ("D" ,(fc-cond-key :normal (fc-manual (kill-buffer (current-buffer)))
                      :region 'kill-rectangle))
   ("E" fc-end-of-semantic)
   ("F" ,(fc-cond-key :normal 'fc-find-files
                      :region (fc-manual (call-interactively 'iedit-mode)
                                         (fc-modal-global-mode -1))))
   ("G" ,(fc-cond-key :normal (fc-manual (fc-text-retrieve default-directory :ignore-files '("compile_commands.json")))
                      :proj (fc-manual (fc-text-retrieve (fc-proj-root) :ignore-files '("compile_commands.json")))
                      :prefix (fc-manual (fc-text-retrieve default-directory) :ignore-files '("compile_commands.json"))))
   ("H" ,(fc-cond-key :normal 'swiper
                      :region (fc-manual
                               (swiper (fc-current-thing :ask nil)))))
   ("I" fc-begin-of-func)
   ("J" backward-word)
   ("K" fc-end-of-func)
   ("L" forward-word)
   ("M" ,(fc-cond-key :normal (fc-head-key "Layout" '*ergo-layout-map*)
                      :region (fc-manual
                               (fc-insert-note
                                (fc-current-thing :ask nil)))))
   ("N" fc-narrow-widen)
   ("O" ,(fc-manual (beginning-of-line)
                    (newline-and-indent)
                    (forward-line -1)
                    (indent-for-tab-command)
                    (fc-modal-global-mode -1)))
   ("P" ,*fc-undef-key*)
   ("Q" ,(fc-cond-key :normal 'delete-window
                      :prefix 'ace-delete-window
                      :one 'bury-buffer))
   ("R" ,(fc-cond-key :normal 'fc-recentf
                      :region (fc-manual (fc--query-replace :backward t))))
   ("S" fc-hs-toggle-all)
   ("T" ,*fc-undef-key*)
   ("U" ,(fc-cond-key :normal (fc-manual
                               (fc-search-next nil t))
                      :prefix (fc-manual
                               (fc-search-next (read-string "Search : ")))))

   ("V" fc-select-files-to-show)
   ("W" ,(fc-cond-key :normal 'fc-buffers-list
                      :region 'delete-rectangle))
   ("X" zap-to-char)
   ("Y" ,(fc-cond-key :normal 'yank-rectangle
                      :region (fc-manual
                               (let ((text (fc-user-select
                                            "Kill ring" kill-ring)))
                                 (call-interactively #'delete-region)
                                 (insert text)))))
   ("Z" neotree-toggle)

   ("!" ,(fc-cond-key :normal 'shell-command
                      :region 'shell-command-on-region))
   ("@" ,(fc-manual (push-mark (point))))
   ("#" comment-dwim)
   ("$" fc-app-portal)
   ("%" fc-program)
   ("^" ,(fc-manual (join-line 1)))
   ("&" ,(fc-manual (fc-set-window-width 0.66)))
   ("*" google-this-search)
   ("(" ,(fc-cond-key :normal 'fc-previous-bookmark
                      :region (fc-decorate-region "(" ")")))
   (")" fc-next-bookmark)
   ("`" ,(fc-cond-key :normal 'fc-ergo-restore
                      :region (fc-decorate-region "`" "`")))
   ("C-`" ,(fc-manual (fc-layout-pop)))
   ("~" ,(fc-manual (fc-player--app *fc-player*)))
   (":" fc-M-x)
   ("=" ,(fc-cond-key :normal (fc-mode-key
                               `((image-mode . image-increase-size)
                                 (_ . text-scale-increase)))
                      :prefix *fc-increase-volume*))
   ("-" ,(fc-cond-key :normal  (fc-mode-key
                                `((image-mode . image-decrease-size)
                                  (_ . text-scale-decrease)))
                      :prefix *fc-decrease-volume*))
   ("+" ,(fc-cond-key :normal  (fc-mode-key
                                `((image-mode . ,(fc-manual
                                                  (image-transform-set-scale 1)))
                                  (_ . ,(fc-manual (text-scale-set 0)))))))
   ("_" ,(fc-cond-key :normal 'fc-list-bookmark
                      :prefix 'fc-edit-bookmark-annotation))
   ("." ,(fc-cond-key :normal 'fc-find-definitions
                      :region 'move-text-up))
   ("," ,(fc-cond-key :normal 'fc-find-references
                      :region 'move-text-down))
   (";" ,(fc-cond-key :normal 'fc-fast-switch-window
                      :region 'comment-dwim))
   ("'" ,(fc-cond-key :normal 'avy-goto-char-timer
                      :region (fc-mode-key
                               `(((latex-mode markdown-mode) . ,*fc-book-zh-single-quote*)
                                 (_ . ,(fc-decorate-region "'" "'"))))))
   ("\"" ,(fc-cond-key :normal nil
                       :region (fc-mode-key
                                `(((latex-mode markdown-mode) . ,*fc-book-zh-quote*)
                                  (_ . ,(fc-decorate-region "\"" "\""))))))
   ("[" ,(fc-cond-key :normal 'fc-navi-prev
                      :prefix *fc-decrease-display-brightness*
                      :region (fc-decorate-region "[" "]")))
   ("]" ,(fc-cond-key :normal 'fc-navi-next
                      :prefix *fc-increase-display-brightness*
                      :region *fc-undef-key*))
   ("{" ,(fc-cond-key :normal 'previous-error
                      :region (fc-decorate-region "{" "}")))
   ("}" next-error)
   ("<" ,(fc-cond-key :normal 'previous-buffer
                      :region 'python-indent-shift-left))
   (">" ,(fc-cond-key :normal 'next-buffer
                      :region 'python-indent-shift-right))
   ("?" ,(fc-cond-key :normal 'fc-run-key-seq
                      :prefix 'fc-set-key-seq))
   ("/" ,(fc-cond-key :normal 'isearch-forward-regexp
                      :prefix 'fc-isearch-dwim))
   ("\\" ,(fc-cond-key :normal 'query-replace-regexp
                       :region 'fc-query-replace
                       :prefix 'query-replace))
   ("|" ,(fc-cond-key :normal 'make-frame
                      :prefix 'delete-frame))
   ("S-<SPC>" ,(fc-cond-key :normal (fc-manual
                                     (fc-modal-head-key
                                      "Basic" '*ergo-basic-map*))
                            :prefix *fc-undef-key*
                            :region 'copy-rectangle-as-kill))
   ("M-<SPC>" ,(fc-cond-key :normal 'scroll-down-command
                            :prefix *fc-undef-key*
                            :region 'copy-rectangle-as-kill))
   ("SPC" ,(fc-cond-key :normal 'scroll-up-command
                        :region 'kill-ring-save
                        :prefix 'just-one-space))
   ("<escape>" fc-escape-key)
   ("<mouse-1>" fc-mouse-func)))

(cl-defun fc-ergo-search-next ()
  "Ergo search next."
  (interactive)

  (when *fc-ergo-prefix*
    (fc-search-stop))

  (fc-search-next))

(fc-bind-keys `(("C-o" fc-modal-run)
                ("C-S-o" fc-modal-run)
                ("C-f" fc-ergo-search-next)))

(defun fc-ergo-repeat-func (func)
  "Set ergo repeat func.
FUNC: new repeat func."
  (fc-bind-keys `((,*fc-repeat-key* ,func)) *fc-modal-keymap*))

(cl-defun fc-modal-global-mode-off (&rest _rest)
  "Turn off global model."
  (fc-modal-global-mode -1))

(--each '(rpn-calc)
  (advice-add it :before #'fc-modal-global-mode-off))

(--each '(
          fc-add-favorite-buffer
          fc-edit-bookmark-annotation
          fc-modal-head-key
          fc-search-next
          fc-set-key-seq
          fc-toggle-bookmark
          fc-toggle-hide-show-all
          just-one-space
          )
  (advice-add it :after #'fc-ergo-prefix-off))

(--each '(
          ace-delete-window
          compile
          delete-other-window
          delete-window
          fc-ergo-simple-grep
          fc-ergo-grep
          fc-lisp-find-tag
          fc-proj-build
          fc-split-window
          ggtags-find-tag-dwim
          split-window-vertically
          split-window-horizontally
          )
  (advice-add it :before #'fc-layout-push))

(--each '(
          fc-find-definitions
          fc-find-references
          )
  (advice-add it :after #'fc-layout-push))

(fc-add-to-hook '*fc-ergo-restore-hook*
                #'fc-ergo-prefix-off
                #'(lambda ()
                    (fc-ergo-repeat-func *fc-repeat-orignal-func*))
                #'fc-search-stop)

(provide 'fc-ergo)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-ergo.el ends here
