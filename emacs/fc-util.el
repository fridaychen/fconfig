;;; fc-util.el --- elisp utility -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)
(ignore-errors
  (require 'battery))

(cl-defun fc-file-first-exists (files)
  "Find first existing file.
FILES: file list to be tested."
  (fc-first files (file-exists-p it)))

(cl-defun fc-dir-exists-p (dir)
  "Test dir exists or not.
DIR: target dir."
  (and (file-exists-p dir)
       (eq (car (file-attributes dir)) t)))

(cl-defun fc-create-file-if-not-exists (filename
                                        &rest rest)
  "Create file if it dose not exist.
FILENAME: file name.
REST: contents."
  (unless (file-exists-p filename)
    (with-temp-file filename
      (fc-each rest
        (cond
         ((or (stringp it) (symbolp it))
          (insert (fc-string it)))

         (t
          (insert (fc-funcall it))))))))

;; remove empty
(defun fc-whitespace-clean ()
  "Cleanup whitespace."
  (interactive)

  (save-excursion
    (fc-replace-regexp "[ \t]+$"
                       ""
                       :from-start t)))

(defun fc-exec-command (command &rest args)
  "Exec command.
COMMAND: command to run.
ARGS: arguments for the command."
  (save-window-excursion
    (apply #'call-process (if (string-prefix-p "~" command)
                              (expand-file-name command)
                            command)
           nil 0 nil (-flatten args))))

(defun fc-exec-command-in-term (command)
  "Exec shell command.
COMMAND: command to be executed."
  (eshell-command command))

(defun fc-exec-command-to-string (command &rest args)
  "Run specific command and return the output.
COMMAND: command to run.
ARGS: arguments for command."
  (declare (indent 1))
  (save-window-excursion
    (with-output-to-string
      (with-current-buffer
          standard-output
        (apply #'call-process command nil t nil (flatten-list args))))))

(cl-defun fc-exec-command-to-buffer (bufname command &rest args)
  "Run specific command and save output to specified buffer.
BUFNAME: buffer.
COMMAND: command to run.
ARGS: arguments for command."
  (let ((buf (get-buffer-create bufname)))
    (save-window-excursion
      (switch-to-buffer buf)
      (read-only-mode -1)
      (erase-buffer)

      (apply #'call-process command nil buf nil (flatten-list args))

      buf)))

(cl-defmacro fc-toggle-var (symbol &key entry quit)
  "Toggle symbol.
SYMBOL: symbol name to be toggled."
  `(if (symbol-value ,symbol)
       (progn
         (setq ,symbol nil)
         ,quit)
     (setq ,symbol t)
     ,entry))

(cl-defun fc-current-thing (&key (ask t) (ext t) regq confirm (prompt "Thing") (deactivate t))
  "Fetch current thing at the point.
EXT: extensional way.
ASK: ask user to confirm.
DEACTIVATE: deativeate region.
REGQ: regex quote.
CONFIRM: ask use to confirm.
PROMPT: prompt for user input."
  (when confirm
    (setf ask nil))

  (let* ((orig (if (use-region-p)
                   (buffer-substring (region-beginning)
                                     (region-end))
                 (thing-at-point (if ext 'symbol 'word))))
         (result (if (and regq orig)
                     (regexp-quote orig)
                   orig)))

    (when (and deactivate (use-region-p))
      (deactivate-mark))

    (if (or confirm
            (and ask (null result)))
        (read-string prompt result)
      result)))

(defun fc-not-void-p (s)
  "Empty or not.
S: obj."
  (not (fc-void-p s)))

(defun fc-void-p (s)
  "Empty or not.
S: obj."
  (cond
   ((null s) t)
   ((and (sequencep s) (zerop (length s))) t)
   (t nil)))

(defun fc-read-symbol (prompt)
  "Read symbol.
PROMPT: user prompt."
  (intern (read-string prompt)))

(defun fc-display-width ()
  "Current monitor width."
  (let ((width (display-mm-width))
        (attrs (frame-monitor-attributes)))
    (if (> width 0)
        width
      (cl-second (assoc 'mm-size attrs)))))

(defun fc-display-height ()
  "Current monitor height."
  (let ((height (display-mm-height))
        (attrs (frame-monitor-attributes)))
    (if (> height 0)
        height
      (cl-third (assoc 'mm-size attrs)))))

(defun fc-display-ppi ()
  "PPI of current monitor."
  (let* ((w (* (fc-display-width) 0.039))
         (h (* (fc-display-height) 0.039))
         (di (sqrt (+ (* h h)
                      (* w w))))
         (dp (sqrt (+ (expt (x-display-pixel-width) 2)
                      (expt (x-display-pixel-height) 2)))))
    (truncate (+ 0.5 (/ dp di)))))

(defun fc-has-battery ()
  "Return the system has a battery or not."
  (let ((alist (ignore-errors
                 (fc-funcall battery-status-function))))
    (cond
     ((null alist)
      nil)

     ((or *is-cygwin* *is-windows*)
      (not (equal (cdr (assoc ?B alist)) "N/A")))

     (*is-linux*
      (and (boundp 'battery-status-function)
           (not (null battery-status-function))
           (not (string-equal
                 (alist-get ?p (funcall battery-status-function))
                 "N/A"))))

     (*is-mac*
      (not (equal (cdr (assoc ?B alist)) "N/A"))))))

(defun fc-show-hide-buffer (buffer-name &optional create-func)
  "Toggle buffer show/hide state.
BUFFER-NAME: buffer name.
CREATE-FUNC: function to create buffer when it not exists."
  (if (equal (buffer-name (current-buffer)) buffer-name)
      (bury-buffer)
    (let ((buffer (get-buffer buffer-name)))
      (if buffer
          (fc-pop-buf buffer :select t)
        (when create-func
          (funcall create-func))))))

(cl-defmacro fc-manual (&rest body)
  "Create a command.
BODY: body."
  `(lambda ()
     (interactive)
     (let ((_ret (progn ,@body)))
       (message (if (and _ret (stringp _ret))
                    (string-trim _ret)
                  "")))))

(cl-defmacro fc-manuals (&rest rest)
  "Create a command.
REST: list of commands."
  `(lambda ()
     (interactive)
     (cl-loop for x in (list ,@rest)
              do
              (fc-funcall x))))

(cl-defmacro fc-region (start end &rest body)
  "Region wrapper.
START: region start pos.
END: region end pos.
BODY: form body."
  (declare (indent 2))
  `(save-excursion
     (save-restriction
       (narrow-to-region ,start ,end)
       ,@body)))

(cl-defmacro fc-whole-buffer (&rest body)
  "Buffer wrapper.
BODY: form body."
  (declare (indent defun))
  `(save-restriction
     (widen)
     (save-excursion
       ,@body)))

(cl-defmacro fc-decorate-region (prefix suffix &key (mark #'fc-mark-symbol))
  "Decorate region.
PREFIX: region prefix.
SUFFIX: region suffix.
MARK: call this func when region is not active."
  `(lambda (start end)
     (interactive "r")

     (unless (region-active-p)
       (if (symbolp ',mark)
           (fc-funcall ',mark)
         (fc-funcall ,mark))

       (unless (region-active-p)
         (cl-return-from fc-decorate-region))

       (setf start (region-beginning)
             end (region-end)))

     (fc-region start end
       (when ,suffix
         (goto-char (point-max))
         (insert (fc-funcall ,suffix)))

       (when ,prefix
         (goto-char (point-min))
         (insert (fc-funcall ,prefix))))))

(defun fc-file-to-string (filename)
  "Read file contents into string.
FILENAME: file to be read."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defun fc-unserialize (filename &optional default)
  "Read file into objection.
FILENAME: file to be read.
DEFAULT: default value."
  (if (file-exists-p filename)
      (with-temp-buffer
        (save-excursion
          (insert-file-contents filename))
        (read (current-buffer)))
    default))

(defun fc-serialize (filename obj)
  "Write object into file.
FILENAME: target file.
OBJ: object to be written."
  (with-temp-buffer
    (pp obj (current-buffer))
    (write-file filename)))

(defun launch-separate-emacs-in-terminal ()
  "Launch Emacs in terminal."
  (suspend-emacs "fg ; emacs -nw --eval '(fc-after-restart)'"))

(defun launch-separate-emacs-under-x ()
  "Launch Emacs in GUI."
  (call-process "sh" nil nil nil "-c" "emacs --eval '(fc-after-restart)'&"))

(defun restart-emacs ()
  "Restart Emacs."
  (interactive)
  ;; We need the new emacs to be spawned after all kill-emacs-hooks
  ;; have been processed and there is nothing interesting left
  (let ((kill-emacs-hook (append kill-emacs-hook
                                 (list (if (display-graphic-p)
                                           #'launch-separate-emacs-under-x
                                         #'launch-separate-emacs-in-terminal)))))
    (save-buffers-kill-emacs)))

(defvar *fc-sound-player* (executable-find "mpg123"))

(cl-defun fc-play-sound-file (file &optional (_volume 25))
  "Play sound file.
FILE: sound file path."
  (when (and *fc-enable-sound*
             *fc-sound-player*)
    (start-process *fc-sound-player* nil *fc-sound-player*
                   "-q"
                   (expand-file-name file))))

(cl-defun fc-play-sound (sound &optional (volume 25))
  "Play sound.
SOUND: sound name.
VOLUME: volume."
  (when *fc-enable-sound*
    (let ((filename (format "%s/sound/%s"
                            *fc-home*
                            (seq-random-elt (alist-get sound *fc-sounds*)))))
      (fc-play-sound-file filename volume))))

(cl-defun fc-job-done (&key (voice "job done") msg)
  "Notify user the job is done.
VOICE: voice message
MSG: text message."
  (cond
   ((or *is-linux* *is-mac*)
    (fc-speak voice))
   (t
    (fc-play-sound 'sweep 50)))

  (when msg
    (fc-popup-info msg :title "Job")))

;; assocate list tree
(cl-defun atree-get (alist &rest keys)
  "Get value from ALIST.
KEYS: path."
  (let ((al alist))
    (fc-each keys
      (let ((l (assoc it al)))
        (if l
            (setf al l)
          (cl-return-from atree-get nil))))
    al))

(cl-defun atree-set (alist value &rest keys)
  "Set value to ALIST.
VALUE: new value.
KEYS: path."
  (let ((al alist))
    (fc-each keys
      (let ((l (assoc it al)))
        (if l
            (setf al l)
          (nconc al `((,it)))
          (setf al (assoc it al)))))
    (setcdr al value)))

;; file utilities
(cl-defun fc-exists-file-in-path (filename
                                  &optional
                                  (dir default-directory))
  "Test file exists or not under specific dir.
FILENAME: file name.
DIR: dir."
  (cond
   ((or (not dir) (string= dir "/"))
    nil)

   ((file-exists-p (concat dir "/" filename))
    (if (string-suffix-p "/" dir)
        (concat dir filename)
      (concat dir "/" filename)))

   (t
    (fc-exists-file-in-path filename
                            (file-name-directory (string-remove-suffix "/"
                                                                       dir))))))

(cl-defun fc-root-window-p (&optional (window (get-buffer-window)))
  "Test if the window is a root window.
WINDOW: target window."
  (eq (frame-root-window window) window))

(cl-defun fc-get-window-width (&optional (window (get-buffer-window)))
  "Get the width of a window.
WINDOW: target window."
  (if (window-live-p window)
      (window-width window)
    (window-total-width window)))

(cl-defun fc-set-window-width (width &optional (window (get-buffer-window)))
  "Set the selected window's width.
WIDTH: width or percent.
WINDOW: target window."
  (when (fc-root-window-p window)
    (cl-return-from fc-set-window-width))

  (let* ((real-width (if (floatp width)
                         (round (* (frame-width) width))
                       width))
         (delta (- real-width (fc-get-window-width window))))
    (when (and (/= delta 0)
               (= (window-resizable window delta t)
                  delta))
      (window-resize window
                     delta
                     t))))

(cl-defun fc-set-window-height (height &optional (window (get-buffer-window)))
  "Set the selected window's height.
HEIGHT: height or percent.
WINDOW: target window."
  (when (fc-root-window-p window)
    (cl-return-from fc-set-window-height))

  (let* ((real-height (if (floatp height)
                          (round (* (frame-height) percent))
                        height))
         (delta (- real-height (window-height window))))
    (when (and (/= delta 0)
               (= (window-resizable window delta)
                  delta))
      (window-resize window
                     delta
                     :ignore t))))

(defvar *fc-big-buffer-threshold* 1048576)

(cl-defun fc-big-buffer-p ()
  "Test if the current buffer is big size buffer."
  (> (buffer-size) *fc-big-buffer-threshold*))

;; environment variables
(cl-defun fc-add-env-path (path &optional to-front (name "PATH"))
  "Add componet to path style environment variable.
PATH: new component.
TO-FRONT: add to front or tail.
NAME: name of environment."
  (unless (fc-dir-exists-p path)
    (cl-return-from fc-add-env-path))

  (let* ((current-path (getenv name))
         (seperator (if *is-windows* ";" ":"))
         (offset (cl-search (concat seperator path seperator)
                            (concat seperator current-path seperator))))
    (when offset
      (cl-return-from fc-add-env-path))

    (setenv name
            (if to-front
                (concat path seperator current-path)
              (concat current-path seperator path)))))

(cl-defun fc--full-prompt-p (prompt)
  "Test PROMPT is full PROMPT.
PROMT: user prompt."
  (fc-first '(": " "? " ") " "-x ")
    (string-suffix-p it prompt)))

(cl-defun fc-add-env-paths (paths)
  "Add multiple compoments to path style environment variable.
PATHS: components."
  (fc-each paths
    (apply #'fc-add-env-path it)))

;; make a prompt string
(cl-defun fc-prompt (prompt)
  "Produce a prompt string.
PROMPT: user prompt string."
  (propertize
   (if (fc--full-prompt-p prompt)
       prompt
     (concat (fc-string prompt) " : "))
   'face 'minibuffer-prompt))

(cl-defun fc--before-read-obj (&rest rest)
  "Wrapper function.
REST: args."
  (when (null (car rest))
    (cl-return))

  (let* ((args (car rest))
         (lines (split-string (car args) "\n")))
    (setf (car (last lines)) (fc-prompt (car (last lines))))
    (setf (car args)
          (string-join lines "\n"))
    args))

(fc-each '(read-directory-name
           read-from-minibuffer
           read-number
           read-string)
  (advice-add it :filter-args #'fc--before-read-obj))

(cl-defun fc-ask (ask)
  "Ask user to input a string.
ASK: prompt or (list prompt init-val)."
  (cond
   ((null ask)
    nil)

   ((listp ask)
    (read-string (cl-first ask) (cl-second ask)))

   (t
    (read-string ask))))

;; unicode utility
(cl-defun fc-unicode-square-string (str)
  "Return the square latin version of str.
STR: origin str."
  (concat (mapcar
           (lambda (c)
             (cond
              ((and (>= c ?a) (<= c ?z))
               (+ c (- ?🄰 ?a)))

              ((and (>= c ?A) (<= c ?Z))
               (+ c (- ?🄰 ?A)))

              ((= ?0 c)
               ?⓪)

              ((and (>= c ?1) (<= c ?9))
               (+ c (- ?① ?1)))

              (t c)))
           str)))

;; propertizer wrapper
(cl-defun fc--text (separator &rest rest)
  "Concat strings.
SEPARATOR: separator between elements.
REST: strings."
  (--reduce-from (let ((part (if (listp it)
                                 (apply #'fc--text separator it)
                               (fc-string it))))
                   (if (and part (not (string-empty-p part)))
                       (if (string-empty-p acc)
                           part
                         (concat acc separator part))
                     acc))
                 ""
                 rest))

(cl-defun fc-remove-properties (text)
  "Remove all properties of text.
TEXT: target."
  (set-text-properties 0 (length text) nil text))

(cl-defun fc-text (obj &key face tip keys pointer (separator " ") limit display)
  "Format text.
OBJ: text source.
FACE: font face.
TIP: tip message.
KEYS: key bindings.
POINTER: mouse pointer.
SEPARATOR: sepatator string.
LIMIT: max text length.
DISPLAY: display property."
  (let ((obj (fc--text separator obj))
        (args ()))
    (when face
      (push (cons 'face face) args))

    (when tip
      (push (cons 'help-echo tip) args))

    (when keys
      (push (cons 'local-map keys) args))

    (when pointer
      (push (cons 'pointer pointer) args))

    (when (and limit
               (< limit (string-width obj)))
      (setf obj (format "%s>"
                        (truncate-string-to-width obj limit))))

    (when display
      (push (cons 'display display) args))

    (apply #'propertize obj
           (--mapcat (list (car it) (cdr it))
                     args))))

(cl-defun fc-text-propertize (text props &key (start 0) (end (length text)) copy)
  (let ((obj (if copy (concat text) text)))
    (apply #'font-lock-append-text-property
           `(
             ,start ,end
             ,@props
             ,obj))
    obj))

;; insert text
(cl-defun fc-insert-text (after-fun &rest rest)
  "Insert text then run AFTER-FUN on the region.
AFTER-FUN: fun to call.
REST: text to insert."
  (let ((start (point)))
    (fc-each rest
      (insert it))

    (when after-fun
      (funcall after-fun start (point)))))

(cl-defun fc-insert-space ()
  "Insert space."
  (when (/= (current-column) 0)
    (end-of-line)
    (insert "\n")))

(cl-defun fc-insert-space-text (after-fun &rest rest)
  "Insert text then run AFTER-FUN on the region.
AFTER-FUN: fun to call.
REST: text to insert."
  (fc-insert-space)
  (apply #'fc-insert-text (cons after-fun rest)))

(cl-defun fc-multi-line-comment-region (start end)
  "Comment multi-line region.
START: start of region.
END: end of region."
  (let ((comment-style 'extra-line))
    (comment-region start end)))

;; find file in upper path
(cl-defun fc-locate-file-in-path (filenames &optional (dir default-directory))
  "Locate file in path.
FILENAMES: filename list.
DIR: target dir."
  (if (fc-first filenames
        (file-exists-p (concat dir it)))
      dir
    (let ((parent-dir (file-name-directory (directory-file-name dir))))
      (if (= (length parent-dir) 1)
          nil
        (fc-locate-file-in-path filenames parent-dir)))))

(defun fc-speak (&rest rest)
  "TTS.
REST: text to be speak."
  (cond
   ((and *is-linux*
         (fc-network-connected-p))
    (apply #'google-speak rest))

   (*is-linux*
    (let ((proc (start-process "espeak-ng" nil
                               "espeak-ng"
                               "-s" "140"
                               "-a" "40"
                               "-v" "us-mbrola-2"
                               "--stdin")))
      (fc-each rest
        (process-send-string proc it)
        (process-send-string proc "\n"))
      (process-send-eof proc)))

   (*is-mac*
    (apply #'osx-lib-say rest))))

(defun fc-line-num (&optional point)
  "Get current line number.
POINT: point of buffer."
  (save-excursion
    (when point
      (goto-char point))

    (string-to-number (format-mode-line "%l"))))

(cl-defun fc-buffer-lines (&optional (buffer (current-buffer)))
  "Get line counts."
  (fc-point-to-line (point-max)))

(defun fc-point-to-line (point)
  "Calautle the line number of POINT.
POINT: target point."
  (fc-whole-buffer
    (goto-char point)
    (fc-line-num)))

(cl-defmacro fc-first-window (form)
  "Find first window which form return non-nil.
FORM: test form."
  `(fc-first (mapcar (lambda (x) (cons x (window-buffer x)))
                     (window-list))
     ,form))

;; looking-at utilities
(cl-defmacro fc-do-looking-at (regex &rest body &key line &allow-other-keys)
  (declare (indent 1))
  `(save-excursion
     (when ,line
       (forward-line 0))

     (when (looking-at ,regex)
       (let* ((start (match-beginning 0))
              (end (match-end 0)))
         ,@body))))

(cl-defmacro fc-replace-looking-text (regex &rest body)
  (declare (indent 1))
  `(fc-do-looking-at ,regex
     (let ((new-text (progn ,@body)))
       (delete-region start end)
       (insert new-text)
       new-text)))

(cl-defun -fc-visible-char (c &key font)
  (when font
    (cl-return-from -fc-visible-char (font-has-char-p font c)))

  (let ((ret (char-displayable-p c)))
    (and ret (or *is-colorful*
                 (not (eq ret 'unicode))))))

(cl-defun -fc-visible (o)
  (pcase (type-of o)
    ('integer
     (when (char-displayable-p o)
       o))

    ('string
     (let* ((font-spec (get-text-property 0 'face o))
            (font (when (and *is-gui* font-spec (fc-font-exists-p font-spec))
                    (find-font (apply #'font-spec font-spec)))))

       (when (and font-spec (cl-member :family font-spec) (not *is-gui*))
         (cl-return-from -fc-visible))

       (dotimes (i (length o))
         (unless (-fc-visible-char (aref o i) :font font)
           (cl-return-from -fc-visible)))
       o))))

(cl-defun fc-visible (&rest rest)
  "Return the elm of rest if every char of it is displayable."
  (catch 'found
    (mapc #'(lambda (x)
              (when (-fc-visible x)
                (throw 'found x)))
          rest)))

(cl-defun fc-call-mode-func (suffix default &rest args)
  (let* ((sym (intern (format "fc--%s-%s"
                              (string-remove-suffix "-mode"
                                                    (fc-string major-mode))
                              suffix))))
    (cond
     ((fboundp sym)
      (apply sym args))

     ((boundp sym)
      (symbol-value sym))

     ((and default (fboundp default))
      (apply default args))

     ((and default (boundp default))
      (symbol-value default)))))

(cl-defun fc-get-mode-var (suffix &optional default)
  (let* ((fsym (intern (format "*fc--%s-%s*"
                               (string-remove-suffix "-mode"
                                                     (fc-string major-mode))
                               suffix))))
    (if (boundp fsym)
        (symbol-value fsym)
      default)))

(cl-defun fc-color-complement (o)
  "Get complement color.
O: color or face."
  (when-let* ((color (if (facep o)
                         (fc-get-face
                          o :background :default nil)
                       o)))
    (apply #'color-rgb-to-hex
           (color-complement color))))

(cl-defun fc-yes-no (prompt &optional default)
  (let ((ans (read-char (fc-prompt (concat prompt
                                           " "
                                           (if default "[Y|n]" "[y|N]"))))))
    (pcase ans
      ((or ?y ?Y ?t ?T) t)
      ((or ?n ?N ?f ?F) nil)
      (_ default))))

(provide 'fc-util)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-util.el ends here
