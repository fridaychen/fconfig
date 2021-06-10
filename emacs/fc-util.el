;;; fc-util.el --- elisp utility -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)
(ignore-errors
  (require 'battery))
(require 'notifications)

(cl-defun fc-file-first-exists (files)
  "Find first existing file.
FILES: file list to be tested."
  (-first #'file-exists-p files))

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
      (--each rest
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

(defun fc-remove-empty-line ()
  "Remove empty lines."
  (interactive)

  (save-excursion
    (fc-replace-regexp "^\n\\{2,\\}"
                       "\n"
                       :from-start t)))

(defun fc-indent-all ()
  "Indent the whole buffer."
  (interactive)

  (indent-region 0 (point-max)))

(defun auto-setup-charset()
  "Auto setup buffer encoding."
  (let ((coding nil)
        (revert-without-query '(".")))
    (setf coding (make-symbol "t_auto_char"))
    (setf coding (intern (downcase (shell-command-to-string
                                    (concat "detect_charset.py '" buffer-file-name "'")))))

    (unless (string-equal "ascii" coding)
      (revert-buffer-with-coding-system coding))))

(defun fc-exec-command (command &rest args)
  "Exec command.
COMMAND: command to run.
ARGS: arguments for the command."
  (save-window-excursion
    (apply #'call-process (if (string-prefix-p "~" command)
                              (expand-file-name command)
                            command)
           nil 0 nil args)))

(defun fc-exec-command-in-term (command)
  "Exec shell command.
COMMAND: command to be executed."
  (eshell-command command))

(defun fc-exec-command-to-string (command &rest args)
  "Run specific command and return the output.
COMMAND: command to run.
ARGS: arguments for command."
  (save-window-excursion
    (with-output-to-string
      (with-current-buffer
          standard-output
        (apply #'call-process command nil t nil args)))))

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

      (apply #'call-process command nil buf nil args)

      buf)))

(defun fc-toggle-var (symbol)
  "Toggle symbol.
SYMBOL: symbol name to be togllged."
  (interactive)

  (if (symbol-value symbol)
      (set symbol nil)
    (set symbol t)))

(cl-defun fc-current-thing (&key (ask t) (ext t) regq confirm (prompt "Thing") (deactivate t))
  "Fetch current thing at the point.
EXTENSION: extensional way.
ASK: ask user to confirm.
DEACTIVATE: deativeate region.
REGQ: regex quote.
CONFIRM: ask use to confirm.
PROMPT: prompt for user input."
  (let ((result (if (use-region-p)
                    (buffer-substring (region-beginning)
                                      (region-end))
                  (thing-at-point (if ext 'symbol 'word)))))

    (when (and deactivate (use-region-p))
      (deactivate-mark))

    (if (or confirm
            (and ask (null result)))
        (setf result (read-string prompt
                                  (if (and regq result)
                                      (regexp-quote result)
                                    result)))
      (when (and regq result)
        (setf result (regexp-quote result))))

    result))

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
          (switch-to-buffer buffer)
        (when create-func
          (funcall create-func))))))

(defmacro fc-manual (&rest body)
  "Create a command.
BODY: body."
  `(lambda ()
     (interactive)
     (let ((_ret (progn ,@body)))
       (message (if (and _ret (stringp _ret))
                    (string-trim _ret)
                  "")))))

(defmacro fc-manuals (&rest rest)
  "Create a command.
REST: list of commands."
  `(lambda ()
     (interactive)
     (cl-loop for x in (list ,@rest)
              do
              (fc-funcall x))))

(defmacro fc-region (start end &rest body)
  "Region wrapper.
START: region start pos.
END: region end pos.
BODY: form body."
  (declare (indent 2))
  `(save-excursion
     (save-restriction
       (narrow-to-region ,start ,end)
       ,@body)))

(defmacro fc-buffer (&rest body)
  "Buffer wrapper.
BODY: form body."
  (declare (indent defun))
  `(save-excursion
     (save-restriction
       ,@body)))

(cl-defmacro fc-decorate-region (prefix suffix &key (mark-func #'er/mark-symbol))
  "Decorate region.
PREFIX: region prefix.
SUFFIX: region suffix.
MARK-FUNC: call this func when region is not active."
  `(lambda (start end)
     (interactive "r")

     (unless (region-active-p)
       (if (symbolp ',mark-func)
           (fc-funcall ',mark-func)
         (fc-funcall ,mark-func))

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

(defun fc-buffer-visible-p (bufname)
  "Test if a buffer is visible.
BUFNAME: to be tested."
  (let ((buf (get-buffer bufname)))
    (and buf
         (get-buffer-window buf))))

(cl-defun fc--user-select (prompt collection &key fullscreen)
  "Select a item from the collection.
PROMPT: user prompt.
COLLECTION: cadidates collection.
FULLSCREEN: fullscreen ui mode."
  (defvar helm-full-frame)

  (cond
   ((and fullscreen (fboundp 'helm))
    (let ((helm-full-frame t))
      (helm :sources
            (helm-build-sync-source prompt
                                    :candidates collection))))

   ((let ((total_len (apply #'+
                            (--map
                             (length (fc-string it))
                             collection))))
      (and (> 9 (length collection))
           (> (- (frame-width) 20 (length prompt))
              total_len)))
    (ido-completing-read (fc-prompt prompt) collection))

   (t
    (ivy-read (fc-prompt prompt) collection))))

(cl-defun fc-user-select (prompt collection &key always fullscreen)
  "Select a item from the collection.
PROMPT: user prompt.
COLLECTION: cadidates collection.
ALWAYS: always ask use to select.
FULLSCREEN: fullscreen ui mode."
  (when (not collection)
    (cl-return-from fc-user-select nil))

  (when (and (not always)
             (= (length collection) 1))
    (let* ((proj (car collection)))
      (cl-return-from fc-user-select
        (if (cl-typep proj 'cons)
            (cdr proj)
          proj))))

  (if (cl-typep (cl-first collection) 'cons)
      (let* ((names (-map 'car collection))
             (name (fc--user-select prompt
                                    names
                                    :fullscreen fullscreen)))
        (cdr (--first (equal (car it) name)
                      collection)))
    (fc--user-select prompt
                     collection
                     :fullscreen fullscreen)))

(cl-defun fc-user-select-func (prompt collection &key fullscreen default)
  "Select a function to run from collection.
PROMPT: user prompt.
COLLECTION: cadidates collection.
FULLSCREEN: fullscreen ui mode.
DEFAULT: default function."
  (fc-funcall (fc-user-select prompt
                              collection
                              :fullscreen fullscreen
                              :always t)
              :default default))

(defun fc--popup-tip-local (title content timeout)
  "Popup top application level.
CONTENT: buffer content.
TITLE: buffer title.
TIMEOUT: buffer show timeout in seconds."
  (if (and (<= 26 emacs-major-version)
           *is-gui*)
      (let ((frame (posframe-show "*fc-tip*"
                                  :string (concat "[" title "]\n\n" content)
                                  :poshandler #'posframe-poshandler-frame-center
                                  :background-color "DarkRed"
                                  :foreground-color "White")))
        (if (eq 0 timeout)
            (read-event)
          (sit-for timeout))
        (posframe-hide frame))

    (let ((lines (s-count-matches "\n" content)))
      (unless (s-suffix? "\n" content)
        (cl-incf lines))

      (popup-tip content :height lines))))

(cl-defun fc-popup-tip (content &key (title "*fc-tip*") (timeout 0) os)
  "Popup top.
CONTENT: buffer content.
TITLE: buffer title.
TIMEOUT: buffer show timeout in seconds.
OS: os level or app level."
  (if os
      (notifications-notify :title title :body content :urgency "critical" :sound-name )
    (fc--popup-tip-local title content timeout)))

(defun fc-popup-hide-tip ()
  "Hide popup tip buffer."
  (posframe-hide "*fc-tip*"))

(add-hook '*fc-ergo-restore-hook* #'fc-popup-hide-tip)

(defun fc-get-string-from-file (filename)
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
      (read-from-string (fc-get-string-from-file filename))
    default))

(defun fc-serialize (filename obj)
  "Write object into file.
FILENAME: target file.
OBJ: object to be written."
  (with-temp-buffer
    (insert (prin1-to-string obj))

    (write-file filename)))

(cl-defun fc-user-confirm (prompt &optional (default-ans t))
  "Ask user 'y or n' question.
PROMPT: user prompt.
DEFAULT-ANS: default answer.
y -> t
n -> nil
Enter -> default-ans
Escape -> nil"
  (cl-loop
   with s = (format "%s ? (%s or %s)"
                    prompt
                    (if default-ans "Y" "y")
                    (if default-ans "n" "N"))
   do
   (pcase (read-char s)
     (?y (cl-return t))
     (?n (cl-return nil))
     (13 (cl-return default-ans))
     (27 (keyboard-quit)))))

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

(defun fc-zh-to-number (str)
  "Convert chinese number string to number.
STR: chinese number string."
  (let ((al '((?零 . 0)
              (?一 . 1)
              (?二 . 2)
              (?两 . 2)
              (?三 . 3)
              (?四 . 4)
              (?五 . 5)
              (?六 . 6)
              (?七 . 7)
              (?八 . 8)
              (?九 . 9)
              (?十 . 10)
              (?百 . 100)
              (?千 . 1000)
              (?万 . 10000)))
        (n 0)
        (ret 0))
    (--each (append str nil)
      (let ((v (cdr (assoc it al))))
        (if (< v 10)
            (setf n (+ (* n 10) v))
          (when (and (= n 0) (= v 10))
            (setf n 1))
          (cl-incf ret (* n v))
          (setf n 0))))
    (+ ret n)))

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

(defun fc-job-done (&rest rest)
  "Notify user the job is done.
REST: message."
  (cond
   ((or *is-linux* *is-mac*)
    (apply #'fc-speak rest))
   (t
    (fc-play-sound 'sweep 50))))

;; assocate list tree
(cl-defun atree-get (alist &rest keys)
  "Get value from ALIST.
KEYS: path."
  (let ((al alist))
    (--each keys
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
    (--each keys
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
                            (file-name-directory (s-chop-suffix "/"
                                                                dir))))))

(cl-defun fc-root-window-p (&optional (window (get-buffer-window)))
  "Test if the window is a root window.
WINDOW: target window."
  (eq (frame-root-window window) window))

(cl-defun fc-set-window-width (&key (window (get-buffer-window))
                                    width
                                    percent)
  "Set the selected window's width.
WINDOW: target window.
WIDTH: width in pixel.
PERCENT: width in percentage."
  (when (fc-root-window-p window)
    (cl-return-from fc-set-window-width))

  (let* ((real-width (if percent
                         (/ (* (frame-width) percent) 100)
                       width))
         (delta (- real-width (window-width window))))
    (when (and (/= delta 0)
               (= (window-resizable window delta t)
                  delta))
      (window-resize window
                     delta
                     t))))

(cl-defun fc-set-window-height (&key (window (get-buffer-window))
                                     height
                                     percent)
  "Set the selected window's height.
WINDOW: target window.
HEIGHT: height in pixel.
PERCENT: height in percentage."
  (when (fc-root-window-p window)
    (cl-return-from fc-set-window-height))

  (let* ((real-height (if percent
                          (/ (* (frameheighth) percent) 100)
                        height))
         (delta (- real-height (window-height window))))
    (when (and (/= delta 0)
               (= (window-resizable window delta)
                  delta))
      (window-resize window
                     delta))))

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
  (--first (string-suffix-p it prompt)
           '(": " "? " ") " "-x ")))

(cl-defun fc-add-env-paths (paths)
  "Add multiple compoments to path style environment variable.
PATHS: components."
  (--each paths
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

(--each '(read-directory-name
          read-from-minibuffer
          read-number
          read-string)
  (advice-add it :filter-args #'fc--before-read-obj))

;; unicode utility
(cl-defun fc-unicode-square-string (str)
  "Return the square latin version of str.
STR: origin str."
  (concat (-map
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
(cl-defun fc-text (obj &key face tip keys pointer (separator " ") limit)
  "Format text.
OBJ: text source.
FACE: font face.
TIP: tip message.
KEYS: key bindings.
POINTER: mouse pointer.
SEPARATOR: sepatator string.
LIMIT: max text length."
  (let ((obj (if (listp obj)
                 (s-join separator
                         (--filter (not (null it))
                                   obj))
               (fc-string obj)))
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

    (apply #'propertize obj
           (--mapcat (list (car it) (cdr it))
                     args))))

;; popup-menu
(cl-defun fc-create-pop-menu (title items)
  "Create pop menu.
TITLE: menu title.
ITEMS: menu items."
  `(,title
    ,(cons "PANE"
           items)))

(cl-defun fc-pop-menu (menu)
  "Popup menu.
MENU: menu."
  (x-popup-menu t menu))

;; insert text
(cl-defun fc-insert-text (after-fun &rest rest)
  "Insert text then run AFTER-FUN on the region.
AFTER-FUN: fun to call.
REST: text to insert."
  (let ((start (point)))
    (--each rest
      (insert it))

    (when after-fun
      (funcall after-fun start (point)))))

(cl-defun fc-insert-space ()
  "Insert space."
  (when (/= (current-column) 0)
    (end-of-line)
    (newline))

  (unless (looking-at "\n")
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
  (if (--first (file-exists-p (concat dir it)) filenames)
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
      (--each rest
        (process-send-string proc it)
        (process-send-string proc "\n"))
      (process-send-eof proc)))

   (*is-mac*
    (osx-lib-say rest))))

;; face
(defun fc-get-face-attribute (face attr)
  "Get attribute of specific face.
FACE: target face
ATTR: attribute."
  (let* ((face-bg (face-attribute face attr)))
    (cond
     ((stringp face-bg)
      face-bg)
     (t (face-attribute 'default attr)))))

(defun fc-set-face-attribute (face &rest rest)
  "Safely set face attribute.
FACE: target face.
REST: all arguments."
  (when (facep face)
    (apply 'set-face-attribute face rest)))

(defun fc-color-difference (colora colorb)
  "Calculate color difference between two colors.
COLORA: the one color.
COLORB: another color."
  (cl-loop for i in (color-values colora)
           for j in (color-values colorb)
           sum (+ (* i i) (* j j)) into x
           finally return (sqrt x)))

(provide 'fc-util)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-util.el ends here
