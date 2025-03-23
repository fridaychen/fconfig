;;; fc-ergo.el --- setup ergo -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)
(require 'hi-lock)
(require 'delsel)

(defconst *fc--undef-key* '(lambda () (interactive)))
(defconst *ergo-prefix-timeout* 1)
(defconst *ergo--head-key-timeout* 3)

(defvar *fc-ergo-prefix* nil)
(defvar *fc-ergo-prefix-timer* nil)
(defvar *fc-ergo-pre-cursor-color* "#cc0000")
(defvar *fc-ergo-restore-hook* nil)

(defconst *fc--repeat-key* "t")
(defconst *fc--repeat-orignal-func* #'fc-translate-word)

(defvar *fc--work-themes* '((dark leuven-dark)
                            (light leuven)
                            (deep-dark modus-vivendi-deuteranopia)))

(defvar *fc--ignore-files* '("compile_commands.json"))

(defvar-local *fc-favorite-buffer* nil)

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
     "‘")
   (lambda ()
     (when (seq-contains-p "“”‘’'\"" (char-before))
       (delete-char -1))
     "’")))

(defconst *fc-book-zh-quote*
  (fc-decorate-region
   (lambda ()
     (when (seq-contains-p "“”‘’'\"" (char-after))
       (delete-char 1))
     "“")
   (lambda ()
     (when (seq-contains-p "“”‘’'\"" (char-before))
       (delete-char -1))
     "”")))

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
                                      from-str)
                              from-str)))
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

(fc-install 'swiper)

(fc-load 'imenu-list
  :after (progn
           (setf imenu-list-buffer-name "📚")))

(fc-load 'move-text
  :autoload t
  :after (move-text-default-bindings))

(fc-load 'avy
  :autoload t
  :after (progn
           (setf avy-background t)))

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

  (fc-toggle-var *fc-ergo-prefix*)
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

   ((bound-and-true-p iedit-mode)
    (iedit-mode -1)
    (fc-modal-enable))

   ((bound-and-true-p iedit-rectangle-mode)
    (iedit-rectangle-mode -1)
    (fc-modal-enable))

   ((region-active-p)
    (kill-region (region-beginning) (region-end)))

   (fc-modal-global-mode
    (fc-modal-disable))

   ((not fc-modal-global-mode)
    (fc-modal-enable))))

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

   ((and (not (zerop (current-column)))
         (looking-at-p "[\"'\\$)}]\\|\\]"))
    (forward-char))

   ((or (looking-at-p "[[:space:]]+$")
        (and (> (point) 6)
             (looking-back "[[:space:]]+$" 6)))
    (delete-blank-lines))

   ((or fc-modal-global-mode
        (not *fc-dev-mode*)
        (not (looking-at "$")))
    (fc-funcall indent-func))

   ;; only do company when <tab> at the end of the line
   ((looking-at ".?$")
    (if *fc-enable-company*
        (company-complete)
      (fc-funcall indent-func)))))

(cl-defun fc-basic-key ()
  "Basic key function."
  (interactive)

  (fc-modal-head-key
   "Basic" '*ergo-basic-map*
   :around (lambda (func)
             (when (> (point) (mark))
               (exchange-point-and-mark))
             (fc-funcall func))))

(cl-defun fc-ctrl-enter-key ()
  "Ctrl-Enter key function."
  (interactive)

  (end-of-line)
  (newline-and-indent)

  (when fc-modal-global-mode
    (fc-modal-disable)))

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

   ((fc-prev-error))

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

   ((fc-next-error))

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
     (if (and (boundp 'lsp-bridge-mode) lsp-bridge-mode)
         (lsp-bridge-find-def-return)
       (xref-go-back)))

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

(defun fc-mark-regex (regex)
  (when (thing-at-point-looking-at regex 1024)
    (set-mark (match-beginning 0))
    (goto-char (match-end 0))))

(defun fc-mark-quote ()
  "Mark quoteation."
  (interactive)

  (fc-mark-regex "[“”\"'‘’（）()][^“”\"'‘’（）()]*[“”\"'‘’（）()]"))

(defun fc-mark-number ()
  "Mark number."
  (interactive)

  (fc-mark-regex "0x[[:xdigit:]]+\\|[[:xdigit:]]+"))

(defun fc-mark-symbol()
  (when (thing-at-point 'symbol)
    (set-mark (match-beginning 0))
    (goto-char (match-end 0))))

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

  (fc-popup-info (current-time-string) :title "Time" :timeout 3))

(cl-defmacro fc-delete-key (&optional mark-func not-save)
  "Delete region.
MARK-FUNC: mark the region to be deleted.
NOT-SAVE: save to KILL-RING or not."
  `(lambda ()
     (interactive)

     (fc-funcall ,mark-func)
     (fc-funcall (if ,not-save
                     'delete-region
                   'kill-region))))

(cl-defmacro fc-change-key (&optional mark-func not-save)
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
     (fc-modal-disable)))

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

   ((eq major-mode 'org-mode)
    (org-narrow-to-subtree))

   (t
    (mark-paragraph arg)
    (narrow-to-region (region-beginning) (region-end))
    (deactivate-mark))))

(defun fc-delete-char (n)
  "Delete char.
N: number."
  (interactive "p")

  (if (eolp)
      (delete-char (* -1 n))
    (delete-char n)))

(defun fc-open-my-index-org ()
  "Open index org file."
  (interactive)

  (find-file (concat *fc-cloud-home* "index.org")))

(cl-defun fc-recover-revert-buffer ()
  "Recover current buffer."
  (if (file-exists-p (make-auto-save-file-name))
      (recover-file buffer-file-name)
    (revert-buffer t t)))

(cl-defun fc-switch-to-recent-buffer ()
  (fc-select-buffer
   ""
   (fc--buffer-pred
    :no-current t
    :filter #'(lambda ()
                (and (buffer-file-name)
                     (null (get-buffer-window)))))
   :one t))

(cl-defun fc-fast-switch-window ()
  "Fast switch window."
  (interactive)

  (cond
   ((one-window-p)
    (fc-switch-to-recent-buffer))

   ;; two or three windows
   ((< (length
        (fc-filter (window-list)
          (not (fc-side-window-p it))))
       4)
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

(defun fc-copy-buffer-and-file ()
  "Copy current file and buffer name."
  (let ((name buffer-file-name)
        (new-name (read-file-name "Copy to : ")))

    (when (fc-dir-exists-p new-name)
      (setq new-name (concat new-name "/" (file-name-nondirectory name))))

    (copy-file name new-name)
    (set-visited-file-name new-name)))

(defun fc-rename-buffer-and-file ()
  "Rename current file and buffer name."
  (let ((name buffer-file-name)
        (new-name (read-file-name "Move to : ")))

    (when (fc-dir-exists-p new-name)
      (setq new-name (concat new-name "/" (file-name-nondirectory name))))

    (if (vc-backend name)
        (vc-rename-file name new-name)
      (rename-file name new-name)
      (set-visited-file-name new-name))))

(defun fc-goto-last-change ()
  (fc-first buffer-undo-list
    (when it
      (cond
       ((and (consp it) (integerp (car it)) (integerp (cdr it)))
        (goto-char (cdr it)))

       ((and (consp it) (stringp (car it)))
        (goto-char (abs (cdr it))))

       ((integerp it)
        (goto-char it)))))
  "")

;; head key
(cl-defmacro fc-head-key (prompt keymap)
  "Run head key.
PROMPT: user prompt.
KEYMAP: keymap to run."
  `(lambda ()
     (interactive)

     (fc-modal-head-key ,prompt ,keymap :timeout *ergo--head-key-timeout*)))

(cl-defmacro fc-head-key-repeat (prompt keymap)
  "Run head key.
PROMPT: user prompt.
KEYMAP: keymap to run."
  `(lambda ()
     (interactive)

     (fc-modal-head-key ,prompt ,keymap :timeout *ergo--head-key-timeout* :repeat t)))

;; global mode
(fc-unbind-keys '("C-x C-c"
                  "C-M-i"))

(fc-bind-keys `(("RET" newline-and-indent)
                ("<escape>" fc-escape-key)
                ("TAB" fc-tab-key)
                ("s-c" ,(fc-head-key "VC" '*ergo-vc-map*))
                ("s-f" fc-basic-key)
                ("s-p" ,(fc-manual (fc-player-func)))
                ("s-q" ,(fc-head-key "Prefix Quick" '*ergo-prefix-quick-map*))
                ("M-x" fc-M-x)
                ("C-<return>" fc-ctrl-enter-key)
                ("C-@" fc-escape-key)
                ("C-<SPC>" fc-escape-key)
                ("C-." ,(fc-manual (fc-find-definitions :apropos t)))
                ("C-_" fc-fast-switch-window) ;; it is C-/ in the console
                ("C-;" fc-fast-switch-window)))

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
    (message "Set functions to %s" (string-trim (get (cl-first function-keys) 'variable-documentation)))

    (setf function-keys (-rotate 1 function-keys))))

(fc-bind-keys `(("M-1" fc-split-unsplit-window)
                ("M-2" fc-split-window)
                ("M-3" fc-show-hide-eshell)
                ("M-4" fc-switch-to-buffer)
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

                ("M-g" goto-line)))

;; misc
(fc-bind-keys `(("C-M-s" fc-toggle-hide-show-all)
                ("M-s" fc-toggle-hide-show)

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
     ("n" ,(fc-change-key 'fc-mark-number))
     ("p" ,(fc-change-key 'mark-paragraph))
     ("q" ,(fc-change-key 'fc-mark-quote))
     ("s" ,(fc-change-key 'fc-mark-symbol))
     ("w" ,(fc-change-key 'fc-mark-word))

     ("A" ,(fc-change-key 'fc-mark-point-to-beginning-of-line t))
     ("B" ,(fc-delete-key 'mark-whole-buffer t))
     ("C" ,(fc-change-key 'fc-mark-point-up-to-char t))
     ("E" ,(fc-change-key 'fc-mark-point-to-end-of-line t))
     ("F" ,(fc-change-key 'fc-mark-func t))
     ("L" ,(fc-change-key 'fc-mark-line t))
     ("N" ,(fc-change-key 'fc-mark-number t))
     ("P" ,(fc-change-key 'mark-paragraph t))
     ("Q" ,(fc-change-key 'fc-mark-quote t))
     ("S" ,(fc-change-key 'fc-mark-symbol t))
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
     ("n" ,(fc-delete-key 'fc-mark-number))
     ("p" ,(fc-delete-key 'mark-paragraph))
     ("q" ,(fc-delete-key 'fc-mark-quote))
     ("s" ,(fc-delete-key 'fc-mark-symbol))
     ("w" ,(fc-delete-key 'fc-mark-word))

     ("A" ,(fc-delete-key 'fc-mark-point-to-beginning-of-line t))
     ("B" ,(fc-delete-key 'mark-whole-buffer t))
     ("C" ,(fc-delete-key 'fc-mark-point-up-to-char t))
     ("D" ,(fc-delete-key 'fc-mark-line t))
     ("E" ,(fc-delete-key 'fc-mark-point-to-end-of-line t))
     ("F" ,(fc-delete-key 'fc-mark-func t))
     ("L" ,(fc-delete-key 'fc-mark-line t))
     ("N" ,(fc-delete-key 'fc-mark-number t))
     ("P" ,(fc-delete-key 'mark-paragraph t))
     ("Q" ,(fc-delete-key 'fc-mark-quote t))
     ("S" ,(fc-delete-key 'fc-mark-symbol t))
     ("W" ,(fc-delete-key 'fc-mark-word t))

     ("^" ,(fc-delete-key 'fc-mark-point-to-beginning-of-buffer))
     ("$" ,(fc-delete-key 'fc-mark-point-to-end-of-buffer))
     )
   "ergo-delete-map")
  *fc-common-key-doc*)

(defun fc-basic-portal ()
  "Run basic portal."
  (interactive)

  (fc-select-func
   "Basic"
   `(
     ("Delete matching line" ,(fc-manual
                               (when-let* ((regex (read-string "Delete matching"))
                                           (empty (not (zerop (length regex)))))
                                 (flush-lines regex))))
     ("Delete non-matching line" ,(fc-manual
                                   (when-let* ((regex (read-string "Delete non-matching"))
                                               (empty (not (zerop (length regex)))))
                                     (keep-lines regex))))
     ("Dos2unix" dos2unix)
     ("Merge all lines" fc-merge-all-line)
     ("Unix2dos" unix2dos)
     )))

(defconst *ergo-basic-map*
  (fc-make-keymap
   `(("SPC" fc-basic-portal)
     ("c" ,(fc-cond-key :normal 'capitalize-word
                        :region 'capitalize-region))
     ("f" ,(fc-cond-key :region 'fc-fill-region))
     ("l" ,(fc-cond-key :normal 'downcase-word
                        :region 'downcase-region))
     ("m" fc-merge-short-line)
     ("p" ,(fc-manual
            (when buffer-file-name
              (kill-new buffer-file-name))))
     ("q" ,(fc-manual
            (insert "./" (f-relative
                          (read-file-name "Insert filename")
                          default-directory))))
     ("t" text-mode)
     ("u" ,(fc-cond-key :normal 'upcase-word
                        :region 'upcase-region))

     ("T" toggle-truncate-lines)
     )
   "ergo-basic-map")
  "KEYS c: capitalize  l: downcase  m: merge  p: copy path  t: text mode  u: upcase  T: trunc.")

(defconst *ergo-goto-map*
  (fc-make-keymap
   `(("<SPC>" fc-hs-goto)

     ("7" ,(fc-manual (fc-show-hide-buffer "*compilation*")))

     ("a" ,(fc-mode-key
            `((image-mode . image-bob)
              (_ . beginning-of-buffer))))
     ("b" ,(fc-cond-key :normal (fc-head-key "Bookmark" '*ergo-bookmark-map*)))
     ("c" ,(fc-manual (recenter (/ (window-height) 2))))
     ("e" ,(fc-mode-key
            `((image-mode . image-eob)
              (_ . end-of-buffer))))
     ("f" ,(fc-cond-key :normal (fc-manual (fc-select-buffer
                                            "Favorite buffer"
                                            (fc--buffer-pred :no-current t
                                                             :var *fc-favorite-buffer*)))
                        :prefix (fc-manual
                                 (fc-toggle-var
                                  *fc-favorite-buffer*
                                  :entry (message "Add to favorite buffer list")
                                  :quit (message "Remove from favorite buffer list")))))
     ("g" ,(fc-manual
            (bury-buffer)
            (fc-select-buffer ""
                              (fc--buffer-pred
                               :mode '(c-mode python-mode go-mode emacs-lisp-mode))
                              :one t)))

     ("h" fc-goto-last-change)

     ("i" windmove-up)
     ("j" windmove-left)
     ("k" windmove-down)
     ("l" windmove-right)

     ("m" fc-switch-layout)
     ("n" ,(fc-cond-key :normal 'fc-switch-next-error-buffer
                        :prefix 'fc-close-all-next-error-buffer))

     ("o" next-buffer)
     ("p" ,(fc-manual (goto-char (read-number "Point : "))))
     ("q" ,(fc-manual (fc-select-buffer
                       "Modified buffers"
                       (fc--buffer-pred :modified t :no-current t))))
     ("r" fc-recentf)
     ("s" ace-swap-window)
     ("t" ,(fc-manual (fc-tag-list)))
     ("u" previous-buffer)
     ("x" ,(fc-manual (fc-select-buffer
                       "Select view"
                       (fc--buffer-pred :no-current t :var 'fc-viewer-minor-mode)
                       :error-msg "No viewer buffer found.")))
     ("w" fc-buffers-list)

     ("[" ,(fc-manual (recenter 1)))
     ("]" ,(fc-manual (recenter -1)))

     (";" goto-line))
   "ergo-goto-map")
  "KEYS SPC: imenu  a: to begin of buffer  b: favorite buffer  c: recenter  e: to end of buffer  g: recent code  h: last change  i: above window  k:  beneath window  j: left window  l: right window  m: switch layout  n: show last navi buf  q: modified buffer  r: all recent file  t: tag  w: all buffer  ;: goto line.")

(defconst *ergo-goto-region-map*
  (fc-make-keymap
   `(("a" beginning-of-buffer)
     ("e" end-of-buffer))
   "ergo-goto-region-map")
  "KEYS a: to begin of buffer  e: to end of buffer.")

(defun fc-gtd-portal ()
  "Run gtd portal."
  (interactive)

  (fc-select-func
   "GTD"
   `(
     ("Org-roam sync" . org-roam-db-sync)
     )))

(defconst *ergo-gtd-map*
  (fc-make-keymap
   `(
     ("SPC" fc-gtd-portal)
     ("3" fc-org-agenda)
     ("a" org-agenda)
     ("c" org-capture)
     ("f" org-roam-node-find)
     ("j" org-roam-capture)
     ("l" org-store-link)
     ("m" org-tags-view)
     ("t" org-todo-list)
     ("w" ,(fc-manual (fc-select-buffer "Org roam" (fc--buffer-pred :filter #'org-roam-buffer-p))))
     ("x" fc-org-study-agenda)
     )
   "ergo-gtd-map")
  "KEYS SPC: portal  a: agenda  c: capture  f: find node  j: capture node  l: store link  m: view tag  t: todo list.")

(cl-defun fc-ergo-which-function ()
  "Which function wrapper."
  (interactive)

  (message "Current: %s"
           (which-function)))

(cl-defun fc-describe-function ()
  "Describe function."
  (cond
   ((fc--lsp-descripbe-function))

   (t
    (describe-function))))

(defvar *fc-quick-attention* nil)

(cl-defun fc-quick-attention ()
  (interactive)

  (fc-toggle-var *fc-quick-attention*
                 :entry (fc-assist-cmd "--mute")
                 :quit (fc-assist-cmd "--unmute")))

(cl-defun fc-info-dwim ()
  (interactive)

  (fc-show-hide-buffer "*info*"
                       #'info))

(cl-defun fc-ergo-load-theme ()
  (fc-theme-auto-select (alist-get *fc-theme-mode* *fc--work-themes*)))

(defconst *ergo-help-map*
  (fc-make-keymap
   `(("SPC" fc-help-portal)
     ("`" ,(fc-manual (fc-play-sound 'cheerup)))
     ("a" apropos)
     ("b" ,(fc-manual (fc-info-show *fc-info-buffer*)))
     ("c" describe-char)
     ("e" describe-function)
     ("f" ,(fc-mode-key
            `(
              (emacs-lisp-mode . describe-function)
              ((latex-mode markdown-mode org-mode) . fc-ergo-which-function)
              (_ . fc-describe-function))))
     ("h" fc-modal-input)
     ("i" fc-info-dwim)
     ("k" describe-key)
     ("m" man)
     ("o" ,(fc-manual
            (if-let* ((buf (get-buffer "*info*")))
                (fc-pop-buf buf :select t)
              (org-info))))
     ("r" ,(fc-manual (fc-pop-buf "*Help*")))
     ("s" ,(fc-manual (fc-info-show *fc-info-system*)))
     ("v" describe-variable)
     ("y" yas-describe-tables)
     ("F" describe-face)
     ("M" describe-mode))
   "ergo-help-map")
  "KEYS SPC: portal  a: apropros  b: buffer info  c: char  e: elisp func  f: func  i: info  k: key  m: manul  o: org info  r: show *Help*  s: sys info  v: var  y: yas  F: face  M: mode.")

(defconst *ergo-mark-map*
  (fc-make-keymap
   `(("a" fc-mark-point-to-beginning-of-line)
     ("b" mark-whole-buffer)
     ("c" fc-mark-point-up-to-char)
     ("e" fc-mark-point-to-end-of-line)
     ("f" fc-mark-func)
     ("l" fc-mark-line)
     ("m" fc-mark-symbol)
     ("n" fc-mark-number)
     ("p" mark-paragraph)
     ("q" fc-mark-quote)
     ("s" fc-mark-symbol)
     ("w" fc-mark-word)

     ("^" fc-mark-point-to-beginning-of-buffer)
     ("$" fc-mark-point-to-end-of-buffer))
   "ergo-mark-map")
  *fc-common-key-doc*)

(defconst *ergo-mode-map*
  (fc-make-keymap
   `(
     ("b" toggle-scroll-bar)
     ("h" highlight-changes-mode)
     ("l" display-line-numbers-mode)
     ("r" rainbow-delimiters-mode)
     ("t" fc-toggle-transparency)
     ("v" visual-line-mode)
     ("w" whitespace-mode)

     ("H" global-highlight-changes-mode)
     ("L" global-display-line-numbers-mode)
     ("S" ,(fc-manual (fc-toggle-var *fc-enable-sound*)
                      (message (if *fc-enable-sound*
                                   "Sound enabled"
                                 "Sound disabled"))))
     ("T" tabbar-mode)
     ("V" global-visual-line-mode)
     )
   "ergo-mode-map")
  "KEYS  b: scroll bar  g: golden ratio  h: highlight changs  l: linum  r: rainbow-delimiter  t: transparency  v: visual line  H: highlight changs  L: linum  S: sound  T: tabbar  V: global visual line.")

(defconst *ergo-quick-map*
  (fc-make-keymap
   `(("SPC" fc-app-portal)
     ("a" align)
     ("b" fc-quick-attention)
     ("c" ,(fc-cond-key :normal 'quick-calc
                        :region (fc-manual (calc-eval (fc-current-thing)))))
     ("d" fc-dev-mode-toggle)
     ("f" ,(fc-cond-key :normal 'fc-find-files
                        :proj (fc-manual (fc-proj-find-file default-directory))))
     ("h" fc-toggle-hex-mode)

     ("i" insert-file)
     ("j" insert-char)
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
     ("y" fc-close-all-side-window)
     ("z" flush-lines)

     ("B" )
     ("C" fc-copy-buffer-and-file)
     ("D" dired-jump)
     ("F" fc-fmt-buffer)
     ("I" fc-insert-signature)
     ("J" )
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
  "KEYS a: align  c: calc  d: dev mode  f: find file  h: hex mode  i: insert file  j: insert char  k: flycheck  l: imenu list  m: multiple  n: new buffer  o: occur  r: recover buffer  s: save  t: time  u: (un)maximize  v: tomato  w: save as  x: reading  z: flush lines  B: none  C: copy  D: open dir  F: format  I: insert signature  L: screen saver  M: rename file  R: readonly  S: save buffers  W: forecast  X: reading.")

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

     ("f" fc-find-files)
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
     ("t" ,(fc-manual (fc-select-func
                       "Select theme style"
                       `(("dark"            . (lambda ()
                                                (fc-theme-auto-select *fc-dark-theme*)))
                         ("deep dark"       . (lambda ()
                                                (fc-theme-auto-select *fc-deep-dark-theme*)))
                         ("light"           . (lambda ()
                                                (fc-theme-auto-select *fc-light-theme*)))
                         ("very light"      . (lambda ()
                                                (fc-theme-auto-select *fc-very-light-theme*)))
                         ("work dark"       . (lambda ()
                                                (fc-theme-auto-select
                                                 (alist-get 'dark *fc--work-themes*))))
                         ("work deep dark"  . (lambda ()
                                                (fc-theme-auto-select
                                                 (alist-get 'deep-dark *fc--work-themes*))))
                         ("work light"      . (lambda ()
                                                (fc-theme-auto-select
                                                 (alist-get 'light *fc--work-themes*))))
                         ("work very light" . (lambda ()
                                                (fc-theme-auto-select
                                                 (alist-get 'light *fc--work-themes*))))))))
     ("v" fc-tomato-customize)
     ("w" ,(fc-manual (fc-theme-auto-select
                       (alist-get *fc-theme-mode* *fc--work-themes*))))
     ("z" suspend-emacs)

     ("I" fc--enlarge-v)
     ("J" fc--reduce-h)
     ("K" fc--reduce-v)
     ("L" fc--enlarge-h)

     ("S" ,(fc-manual (fc-assist-cmd "--pmsleep")))
     )
   "ergo-prefix-quick-map")
  "KEYS c: rpn calc  d: load desktop  e: new buf with tmpl  i: vertically enlarge  j: horizontally enlarge  k: vertically reduce  l: horizontally reduce  t: select theme  w: work theme  z: suspend  S: sleep  W: deep work theme.")

(defun fc-vc-portal ()
  "Run vc portal."
  (interactive)

  (fc-select-func
   "VC"
   `(
     ("Branch: create"                      . ,(fc-manual
                                                (when-let* ((name (read-string "New branch")))
                                                  (vc-create-branch (fc-proj-root) name))))
     ("Branch: delete"                      . fc-git-delete-branch)
     ("Branch: rename"                      . fc-git-rename-branch)
     ("Branch: switch"                      . ,(fc-manual (fc-vc-switch-branch)
                                                          (fc-vc-revert-repo)))
     ("Diff with other branch"              . fc-vc-diff-with-other-branch)
     ("Diff current file with other branch" . fc-vc-diff-file-with-other-branch)
     ("Gitk with current file"              . ,(fc-manual (start-process "gitk" nil "gitk" buffer-file-name)))
     ("Gitk"                                . ,(fc-manual (start-process "gitk" nil "gitk")))
     ("Rebase"                              . magit-rebase-branch)
     )))

(defconst *ergo-vc-map*
  (fc-make-keymap
   `(("SPC" fc-vc-portal)
     ("a" fc-git-amend)
     ("c" vc-next-action)
     ("h" ,(fc-cond-key :normal (fc-manual
                                 (save-excursion
                                   (fc-mark-line)
                                   (fc-funcall #'vc-region-history)
                                   (deactivate-mark)))
                        :prefix #'fc-vc-show-commit-blame-with-line))
     ("j" ,(fc-manual (shell-command-to-string "fit -s")))
     ("k" fc-git-commit)
     ("l" magit-log-buffer-file)
     ("p" fc-git-pull)
     ("q" fc-git-push)
     ("r" fc-vc-rename-file)
     ("s" fc-git-add)
     ("u" vc-revert)
     ("v" magit-diff-buffer-file)
     ("x" magit-status)
     ("H" magit-blame-addition)
     ("L" magit-log-all)
     ("V" magit-diff-working-tree)
     ("X" ,(fc-manual (start-process "gitk" nil "gitk")))
     )
   "ergo-vc-map")
  "KEYS a: amend  c: commit  h: popup history  j: repo status  k: commit  l: log of current  p: pull  q: push  r: rename  s: stage  u: revert  v: diff  x: magit  H: magit blame  L: log of HEAD  V: diff repo.")

(defconst *ergo-layout-map*
  (fc-make-keymap
   `(("l" ,(fc-manual (let ((name (read-char "Load layout : " nil *ergo--head-key-timeout*)))
                        (if name (fc-layout-load name) (message "")))))
     ("q" fc-layout-push)
     ("p" fc-layout-pop)
     ("s" ,(fc-manual (let ((name (read-char "Save layout : " nil *ergo--head-key-timeout*)))
                        (if name (fc-layout-save name) (message ""))))))
   "ergo-layout-map")
  "KEYS l: load  p: pop  q: push  s: save.")

(cl-defun fc--select-bookmark (&key (prompt "Select bookmark"))
  (fc-select
      prompt
      (mapcar #'car (bookmark-maybe-sort-alist))))

(cl-defun fc--add-bookmark ()
  (interactive)

  (when-let* ((meta (fc--book-p)))
    (bookmark-set (read-string "Add bookmark "
                               (format "Book: <<%s>> %s"
                                       (plist-get meta :title)
                                       (which-function))))
    (cl-return-from fc--add-bookmark))

  (when-let* ((root (fc-proj-root))
              (has-name (boundp 'fc-proj-name)))
    (bookmark-set (read-string "Add bookmark "
                               (format "Proj %s: %s %s"
                                       fc-proj-name
                                       (file-relative-name buffer-file-name root)
                                       (which-function))))
    (cl-return-from fc--add-bookmark))

  (when-let* ((name (read-string "Add bookmark name"
                                 (format "Doc : %s -- %s"
                                         (buffer-name)
                                         (which-function))))
              (not-empty (not (seq-empty-p name))))
    (bookmark-set name)
    (cl-return-from fc--add-bookmark)))

(cl-defun fc--auto-bookmark ()
  (interactive)

  (when-let* ((meta (fc--book-p)))
    (bookmark-set (format "Book: <<%s>> furthest read position" (plist-get meta :title)))
    (cl-return-from fc--auto-bookmark))

  (when-let* ((root (fc-proj-root))
              (has-name (boundp 'fc-proj-name)))
    (bookmark-set (format "Proj %s: %s -- %s"
                          fc-proj-name
                          (file-relative-name buffer-file-name root)
                          (or (which-function) "Unknown")))
    (cl-return-from fc--auto-bookmark))

  (message "Do nothing!"))

(defconst *ergo-bookmark-map*
  (fc-make-keymap
   `(
     ("<SPC>" fc-bookmark)
     ("d" ,(fc-manual
            (when-let* ((bm (fc--select-bookmark :prompt "Select bookmark to remove"))
                        (confirm (fc-user-confirm
                                  (format "Remove bookmark '%s'" bm))))
              (bookmark-delete bm))))
     ("l" fc--auto-bookmark)
     ("j" fc--add-bookmark)
     ("r" bookmark-rename)
     ("u" ,(fc-manual
            (when-let* ((bm (fc--select-bookmark :prompt "Select bookmark to create or update"))
                        (confirm (fc-user-confirm
                                  (format "Update bookmark '%s'" bm))))
              (bookmark-set bm))))))
  "KEYS d: delete  l: auto  j: add  r: rename  u: create or update.")

(defun fc-mode-func-key ()
  "Run function base on mode."
  (interactive)
  (fc-call-mode-func "mode-func" #'fc-common-mode-func))

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
   ("1" ,(fc-cond-key :normal #'delete-other-windows
                      :prefix #'ace-delete-other-windows
                      :one #'fc-split-window
                      :main #'fc-close-all-side-window
                      :side #'fc-close-all-side-window))
   ("2" ,(fc-cond-key :normal (fc-manuals
                               #'fc-split-window
                               #'other-window
                               #'fc-switch-to-recent-buffer)
                      :prefix #'split-window-horizontally))
   ("3" ,(fc-cond-key :normal (fc-head-key "ORG" '*ergo-gtd-map*)
                      :prefix #'split-window-vertically))
   ("4" ,(fc-cond-key :normal #'fc-switch-to-buffer))
   ("5" toggle-frame-fullscreen)
   ("6" ,(fc-cond-key :normal #'fc-toggle-window-maximize
                      :prefix #'balance-windows))
   ("7" ,(fc-cond-key :normal #'compile
                      :work #'fc-proj-build
                      :prefix #'compile))
   ("8" ,(fc-cond-key :normal #'fc-proj-open
                      :work #'fc-proj-find-file))
   ("9" ,(fc-manual (set-mark-command 0)))
   ("0" fc-ergo-prefix-on)

   ("a" ,(fc-mode-key
          `((image-mode . image-bol)
            (_ . fc-beginning-of-line))))
   ("b" ,(fc-cond-key :normal #'fc-backward
                      :region #'comment-dwim))
   ("C-b" scroll-down-command)

   ;; c := Change
   ("c" ,(fc-cond-key :normal (fc-head-key "Change" '*ergo-change-map*)
                      :region (fc-change-key)))

   ;; d := Delete
   ("d" ,(fc-cond-key :normal (fc-head-key "Delete" '*ergo-delete-map*)
                      :region #'kill-region
                      :preregion #'kill-rectangle))

   ("e" ,(fc-mode-key
          `((image-mode . image-eol)
            (_ . end-of-line))))
   ("f" ,(fc-cond-key :normal #'scroll-down-command
                      :region #'fc-basic-key
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
                      :region #'deactivate-mark
                      :prefix (fc-head-key "Mode" '*ergo-mode-map*)))

   ("n" fc-escape-key)
   ("o" ,(fc-cond-key :normal #'fc-ctrl-enter-key
                      :region #'fc-occur-dwim))

   ;; p := Player
   ("p" ,(fc-cond-key :normal #'fc-match-paren
                      :prefix #'fc-player-func))

   ;; q := Quick/Quest
   ("q" ,(fc-cond-key :normal (fc-head-key "Quick"
                                           '*ergo-quick-map*)
                      :prefix (fc-head-key "Prefix Quick"
                                           '*ergo-prefix-quick-map*)))

   ("r" ,(fc-cond-key :normal #'fc-proj-recentf
                      :region (fc-manual (fc--query-replace))
                      :preregion (fc-manual (fc--query-replace
                                             :from-beginning t))))

   ("s" ,(fc-cond-key :normal #'fc-hs-toggle
                      :region #'fc-isearch-dwim
                      :prefix #'fc-toggle-hide-show-all))
   ("t" fc-translate-word)
   ("u" fc-mode-func-key)
   ("v" ,(fc-cond-key :normal #'set-mark-command
                      :region (fc-manual (er/expand-region 1))))
   ("C-v" ,(fc-cond-key :normal #'fc-mark-symbol
                        :region (fc-manual (er/expand-region -1))))
   ("w" ,(fc-cond-key :normal #'fc-buffers-list
                      :region #'delete-region
                      :proj (fc-manual
                             (when-let* ((root (fc-proj-root)))
                               (fc-select-buffer
                                "Switch within project"
                                (fc--buffer-pred :dir root :no-current t)
                                :relative root)))))
   ("x" ,(fc-cond-key :normal #'fc-delete-char
                      :region #'exchange-point-and-mark))
   ("y" ,(fc-cond-key :normal #'yank
                      :prefix #'fc-yank-pop
                      :region (fc-manuals #'delete-region
                                          #'yank)
                      :preregion (fc-manuals #'delete-region
                                             #'fc-yank-pop)))
   ("z" ,(fc-manual (push-mark (point))
                    (undo)))

   ("A" fc-begin-of-semantic)
   ("B" ,(fc-cond-key :normal (fc-head-key "Bookmark" '*ergo-bookmark-map*)))
   ("s-b" fc-bookmark)

   ;; C := VC
   ("C" ,(fc-head-key "VC" '*ergo-vc-map*))

   ("D" ,(fc-cond-key :normal #'fc-kill-current-buffer
                      :region #'delete-region
                      :preregion #'delete-rectangle))
   ("E" fc-end-of-semantic)
   ("F" ,(fc-cond-key :normal #'fc-find-files
                      :preregion (fc-manual (call-interactively #'iedit-rectangle-mode)
                                            (fc-modal-disable))
                      :region (fc-manual (call-interactively #'iedit-mode)
                                         (fc-modal-disable))))
   ("G" ,(fc-cond-key :normal (fc-manual (fc-text-retrieve :ignore-files *fc--ignore-files*))
                      :proj (fc-manual (fc-text-retrieve :dir (fc-proj-root) :ignore-files *fc--ignore-files*))
                      :prefix (fc-manual (fc-text-retrieve :ignore-files *fc--ignore-files*))))
   ("H" ,(fc-cond-key :normal #'swiper
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
                    (fc-modal-disable)))
   ("P" ,*fc--undef-key*)
   ("Q" ,(fc-cond-key :normal #'delete-window
                      :prefix #'ace-delete-window
                      :one #'bury-buffer))
   ("R" ,(fc-cond-key :normal #'fc-recentf
                      :region (fc-manual (fc--query-replace :backward t))))
   ("S" fc-hs-toggle-all)
   ("T" ,(fc-manual (fc-translate (fc-current-thing :confirm t :ask t))))
   ("U" ,(fc-cond-key :normal (fc-manual
                               (fc-search-next nil t))
                      :prefix (fc-manual
                               (fc-search-next (read-string "Search : ")))))

   ("V" fc-select-files-to-show)
   ("W" ,(fc-cond-key :normal #'fc-buffers-list
                      :region #'delete-rectangle))
   ("X" zap-to-char)
   ("Y" ,(fc-cond-key :normal #'yank-rectangle
                      :region (fc-manual
                               (let ((text (fc-select "Kill ring" kill-ring)))
                                 (call-interactively #'delete-region)
                                 (insert text)))))
   ("Z" neotree-toggle)

   ("!" ,(fc-cond-key :normal #'shell-command
                      :region #'shell-command-on-region))
   ("@" ,(fc-manual (push-mark (point))))
   ("#" comment-dwim)
   ("$" ,(fc-cond-key :normal #'fc-app-portal
                      :region (fc-decorate-region "$" "$")))
   ("%" fc-program)
   ("^" ,(fc-manual (join-line 1)))
   ("&" ,(fc-cond-key :normal (fc-manual (fc-set-window-width 0.66))
                      :prefix (fc-manual (fc-set-window-width 0.33))))
   ("*" google-this-search)
   ("(" ,(fc-cond-key :normal #'fc-previous-bookmark
                      :region (fc-decorate-region "(" ")")))
   (")" fc-next-bookmark)
   ("`" ,(fc-cond-key :normal #'fc-ergo-restore
                      :region (fc-decorate-region "`" "`")))
   ("C-`" ,(fc-manual (fc-layout-pop)))
   ("C-'" ,(fc-manual (fc-layout-pop)))
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
   ("_" ,(fc-cond-key :normal #'fc-list-bookmark
                      :prefix #'fc-edit-bookmark-annotation))
   ("." ,(fc-cond-key :normal #'fc-find-definitions
                      :region #'move-text-up))
   ("," ,(fc-cond-key :normal #'fc-find-references
                      :region #'move-text-down))
   (";" ,(fc-cond-key :normal #'fc-fast-switch-window
                      :region #'comment-dwim))
   ("'" ,(fc-cond-key :normal #'avy-goto-char-timer
                      :region (fc-mode-key
                               `(((latex-mode markdown-mode org-mode) . ,*fc-book-zh-single-quote*)
                                 (_ . ,(fc-decorate-region "'" "'"))))))
   ("\"" ,(fc-cond-key :normal nil
                       :region (fc-mode-key
                                `(((latex-mode markdown-mode org-mode) . ,*fc-book-zh-quote*)
                                  (_ . ,(fc-decorate-region "\"" "\""))))))
   ("[" ,(fc-cond-key :normal #'fc-navi-prev
                      :prefix *fc-decrease-display-brightness*
                      :region (fc-decorate-region "[" "]")))
   ("]" ,(fc-cond-key :normal #'fc-navi-next
                      :prefix *fc-increase-display-brightness*
                      :region *fc--undef-key*))
   ("{" ,(fc-cond-key :normal #'previous-error
                      :region (fc-decorate-region "{" "}")))
   ("}" next-error)
   ("<" ,(fc-cond-key :normal #'previous-buffer
                      :region #'python-indent-shift-left))
   (">" ,(fc-cond-key :normal #'next-buffer
                      :region #'python-indent-shift-right))
   ("?" ,(fc-cond-key :normal #'fc-run-key-seq
                      :prefix #'fc-set-key-seq))
   ("/" ,(fc-cond-key :normal #'isearch-forward-regexp
                      :prefix #'fc-isearch-dwim))
   ("\\" ,(fc-cond-key :normal #'query-replace-regexp
                       :region #'fc--query-replace
                       :preregion (fc-manual (fc--query-replace
                                              :from-beginning t))
                       :prefix #'query-replace))
   ("|" ,(fc-cond-key :normal #'make-frame
                      :prefix #'delete-frame))
   ("S-<SPC>" ,(fc-cond-key :normal (fc-manual
                                     (fc-modal-head-key
                                      "Basic" '*ergo-basic-map*))
                            :prefix *fc--undef-key*
                            :region #'copy-rectangle-as-kill))
   ("M-<SPC>" ,(fc-cond-key :normal #'scroll-down-command
                            :prefix *fc--undef-key*
                            :region #'copy-rectangle-as-kill))
   ("SPC" ,(fc-cond-key :normal #'scroll-up-command
                        :region #'kill-ring-save
                        :prefix #'just-one-space))
   ("<escape>" fc-escape-key)
   ("<mouse-1>" fc-mouse-func)
   ("<mouse-8>" fc-backward)))

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
  (fc-bind-keys `((,*fc--repeat-key* ,func)) *fc-modal-keymap*))

(cl-defun fc-modal-global-mode-off (&rest _rest)
  "Turn off global model."
  (fc-modal-disable))

(defun fc-kill-current-buffer ()
  (interactive)

  (kill-buffer (current-buffer)))

(cl-defun fc-switch-to-buffer ()
  (interactive)

  (when-let* ((buf-name (read-buffer-to-switch "Switch to buffer: "))
              (buf (get-buffer buf-name))
              (win (progn
                     (when (fc-side-window-p)
                       (select-window (or (window-child (window-main-window))
                                          (window-main-window))))
                     (display-buffer buf
                                     '(display-buffer-same-window
                                       display-buffer-use-some-window
                                       display-buffer-reuse-window)))))
    (select-window win)))

(defvar *fc-fill-region-fmt* "%d")

(cl-defun fc-fill-region ()
  "Fill region."
  (when *fc-ergo-prefix*
    (setf *fc-fill-region-fmt* (read-string "New fill region format" *fc-fill-region-fmt*))
    (cl-return-from fc-fill-region))

  (let* ((start (region-beginning))
         (start-line (fc-line-num (region-beginning)))
         (end-line (fc-line-num (region-end)))
         (col (current-column))
         (line-num 1))
    (deactivate-mark)
    (goto-char start)

    (cl-loop for i from start-line to end-line
             do
             (insert (format *fc-fill-region-fmt* line-num))
             (forward-line 1)
             (cl-incf line-num)
             (move-to-column col t))))

(fc-each '(rpn-calc)
  (advice-add it :before #'fc-modal-global-mode-off))

(fc-each '(
           fc-add-favorite-buffer
           fc-edit-bookmark-annotation
           fc-modal-head-key
           fc-search-next
           fc-set-key-seq
           fc-toggle-bookmark
           fc-toggle-hide-show-all
           )
  (advice-add it :after #'fc-ergo-prefix-off))

(fc-add-to-hook '*fc-ergo-restore-hook*
                #'fc-ergo-prefix-off
                #'(lambda ()
                    (fc-ergo-repeat-func *fc--repeat-orignal-func*))
                #'fc-search-stop)

(fc-layout-spotlight
 #'compile
 #'describe-char
 #'describe-face
 #'describe-function
 #'describe-variable
 #'fc-info-dwim
 #'fc-text-retrieve
 #'fc-find-definitions
 #'fc-find-references
 #'fc-flycheck
 #'fc-occur-dwim
 #'fc-switch-next-error-buffer
 #'imenu-list-smart-toggle
 #'magit-diff-buffer-file
 #'magit-diff-working-tree
 #'magit-log-all
 #'man)

(provide 'fc-ergo)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-ergo.el ends here
