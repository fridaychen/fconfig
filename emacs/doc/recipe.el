;; list
(setq foo '(2 3 4 5 6))
(add-to-list 'foo 1)

(append foo '(4))

(butlast foo 2)

;; sequence
_____________________________________________
|                                             |
|          Sequence                           |
|  ______   ________________________________  |
| |      | |                                | |
| | List | |             Array              | |
| |      | |    ________       ________     | |
| |______| |   |        |     |        |    | |
|          |   | Vector |     | String |    | |
|          |   |________|     |________|    | |
|          |  ____________   _____________  | |
|          | |            | |             | | |
|          | | Char-table | | Bool-vector | | |
|          | |____________| |_____________| | |
|          |________________________________| |
|_____________________________________________|

;; array

;; vector
(length [1 2 3])
(elt [?a ?b ?c] ?a)

;; hashtable
(setf foo (make-hash-table))
(puthash 1 "saf" foo)
(gethash 1 foo)

(remhash 1 foo)

;; alist association-list
(assoc 1 '((1 "a") (2 "b") (3 "c")))
(alist-get 1 '((1 "a" "a") (2 "b") (3 "c")))

;; plist
(plist-get '(pine cones numbers (1 2 3) color "blue") 'numbers)

;; eval string
(eval (car (read-from-string "(+ 1 1)")))

;; list files
(directory-files "~/.emacs.d/fconfig/")

;; slice list
(nthcdr 2 '(1 2 3 4))
(last '(1 2 3 4) 3)

(cl-first '(1 2 3))
(cl-second '(1 2 3))
(cl-third '(1 2 3 4))
(cl-fourth '(1 2 3 4))

;; x-pop-menu
(defconst prog-menu '("PANE" ("1" 1 "aa") ("2" 2) ("3" 3)))

(setq foo (x-popup-menu t (list "Programs" prog-menu)))

;; tiny-menu
(defconst *fc-main-menu* '(("main menu" ("Programs"
                                         ((?u "Upgrade" package-utils-upgrade-all)
                                          (?i "Install" (lambda () (fc-read-symbol "Package name: ")))
                                          (?d "Delete" (lambda () (fc-read-symbol "Package name: ")))))
                            )))

(defun fc-program ()
  (interactive)

  (let ((tiny-menu-items *fc-main-menu*))
    (tiny-menu)))

;; advice

(defun his-tracing-function (orig-fun &rest args)
  (message "display-buffer called with args %S" args)
  (let ((res (apply orig-fun args)))
    (message "display-buffer returned %S" res)
    res))

(advice-add 'display-buffer :around #'his-tracing-function)
(advice-remvoe 'display-buffer #'his-tracing-function)

(defun fc-ergo-save-window (&rest rest)
  (fc-save-window))

(advice-add 'fc-split-window :before 'fc-ergo-save-window)
(advice-add 'delete-other-windows :before 'fc-ergo-save-window)
(advice-add 'ace-delete-other-windows :before 'fc-ergo-save-window)
(advice-add 'delete-window :before 'fc-ergo-save-window)
(advice-add 'ace-delete-window :before 'fc-ergo-save-window)

(advice-add 'read-from-minibuffer :around #'return-my-name)
(defun return-my-name (orig-fun &rest args)
  (let ((orig-val (apply orig-fun args)))
    <return-the-new-value>))
Or you can do it with

(advice-add 'read-from-minibuffer :filter-return #'return-my-name)
(defun return-my-name (orig-val)
  <return-the-new-value>)

;; font scale
(setq face-font-rescale-alist '(("^-apple-hiragino.*" . 1.2)
                                (".*STFangSong.*" . 1.0)
                                (".*方正宋刻本秀楷简补全.*" . 1.0)
                                (".*osaka-medium.*" . 1.2)
                                (".*courier-bold-.*-mac-roman" . 1.0)
                                (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
                                (".*monaco-bold-.*-mac-roman" . 0.9)
                                ("-cdac$" . 1.3)))

;; display
(x-display-monitor-attributes-list)

(x-display-pixel-width)
(x-display-pixel-height)

(display-mm-width)
(display-mm-height)

(defun fc-display-ppi ()
  (let* ((w (* (display-mm-width) 0.039))
         (h (* (display-mm-height) 0.039))
         (di (sqrt (+ (* h h)
                      (* w w))))
         (dp (sqrt (+ (* (x-display-pixel-width) (x-display-pixel-width))
                      (* (x-display-pixel-height) (x-display-pixel-height))))))
    (format "~a * ~a" w  h)
    (/ dp di)))

(fc-display-ppi)

font-height
iMac 180
Yoga 140

;; keymap
(current-active-maps)

;; display
(selected-frame)
(selected-window)

(current-buffer)

default-frame-alist

;; font

(insert (format "%s" (font-family-list)))

(list-fonts (font-spec "fontset-read"))

(set-face-attribute 'default nil :font "DejaVu Sans Mono-14")
(set-face-attribute 'mode-line nil :family "Monaco" :family "Mircosoft YaHei" :height 140)

(create-fontset-from-fontset-spec
 "-*-foo-*-*-*-*-*-*-*-*-*-*-fontset-read,
    ascii:-*-monaco-*-*-*-*-*-*-*-*-*-*-iso8859-1,
    japanese-jisx0208:-*-microsoft yahei-*-*-*-*-*-*-*-*-*-*-*-*,
    chinese-gbk:-*-HYQuanTangShiJ-*-*-*-*-*-*-*-*-*-*-*-*")

(create-fontset-from-fontset-spec
 "-*-foo-*-*-*-*-*-*-*-*-*-*-fontset-foo,
    ascii:-*-monaco-*-*-*-*-*-*-*-*-*-*-iso8859-1,
    japanese-jisx0208:-*-microsoft yahei-*-*-*-*-*-*-*-*-*-*-*-*,
    chinese-gbk:-*-microsoft yahei-*-*-*-*-*-*-*-*-*-*-*-*")
(set-face-attribute 'mode-line nil :fontset "fontset-foo" :height 120 :weight 'light)

(setq buffer-face-mode-face '(:family "Monaco" :height 190))

;; buffer window

;; my-buffer is supposed to be the buffer you are looking for
(cond ((eq my-buffer (window-buffer (selected-window)))
       (message "Visible and focused"))
      ((get-buffer-window my-buffer)
       (message "Visible and unfocused"))
      (t
       (message "Not visible")))

(window-buffer (selected-window))

(get-buffer-window (get-buffer "*ggtags-global*"))

(get-buffer-window)
(get-buffer-window-list)

;; projectile

(let ((projectile-completion-system 'helm))
  (call-interactively 'projectile-find-file))

;; mouse fringe bookmark

(global-set-key (kbd "<left-fringe> <mouse-1>") #'(lambda(event)
                                                    (interactive "e")
                                                    (save-excursion
                                                      (mouse-set-point event)
                                                      (bm-toggle))))

;; font setup

(set-frame-font "-unknown-Monaco-normal-normal-normal-*-*-*-*-*-*-0-iso10646-1")

(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
                    charset (font-spec :family "Microsoft Yahei Light" :size 18)))

(find-font (font-spec :family "Microsoft Yahei Light1"))

(fc-font-exists-p "Microsoft YaHei Light")

(member "Microsoft YaHei" (font-family-list))

(defun font-candidate (&rest fonts)
  "Return existing font which first match."
  (find-if (lambda (f) (find-font (font-spec :name f))) fonts))
(insert (format "%s" (font-family-list)))

;; shell command
(let* ((s (shell-command-to-string "amixer get Master"))
       (start (string-match "\[[0-9]+%\]" s)))
  (when (match-end 0)
    2))

;; simulate key
(defun fc-test ()
  (interactive)

  (execute-kbd-macro (kbd "ex M-k M-k a")))

(fc-modal-keys `(("$" . fc-test)))

;; pattern matching
(cl-destructuring-bind (a b c) (list 1 2 3)
  (message "%d %d %d" a b c)
  (+ a b c))

(-let [(a b c) '(1 2 3)]
  (+ a b c))

;; dash
(--each (list 1 2 3)
  (message "%d" it))

(-filter (lambda (x) (> x 10)) (list 8 9 10 11 12))
(--filter (> it 10) (list 1 2 3 11 20))

-map
-reduce
-count
-slice
-group-by

;; s
(string-trim " hello ")
(s-join "|" '("1" "2" "3"))
(s-replace "[0-9]" "world" " -- h 1 h 2= =tom") ;; non-regexp
(s-match "[0-9]+" "helo1293world12")
(s-reverse "hello")
(s-join ":" (list "1" "2" "3"))

;; capslock to control
setxkbmap -option ctrl:nocaps

;; xdotool

xdotool key XF86AudioRaiseVolume
xdotool key XF86AudioLowerVolume

xdotool key XF86MonBrightnessDown
xdotool key XF86MonBrightnessUp

keycode 160 = XF86AudioMute
keycode 174 = XF86AudioLowerVolume
keycode 176 = XF86AudioRaiseVolume
keycode 162 = XF86AudioPlay
keycode 144 = XF86AudioPrev
keycode 145 = XF86AudioNext
keycode 164 = XF86AudioStop
keycode 237 = XF86HomePage

;; revert-all-buffers

(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files"
  (interactive)
  (let* ((list (buffer-list))
         (buffer (car list)))
    (while buffer
      (when (and (buffer-file-name buffer)
                 (not (buffer-modified-p buffer)))
        (set-buffer buffer)
        (revert-buffer t t t))
      (setq list (cdr list))
      (setq buffer (car list))))
  (message "Refreshed open files"))

;; show buffers by pattern

(defun foo (pattern)
  (let* ((bufs (-filter
                #'(lambda (x) (and x
                                   (buffer-file-name x)
                                   (string-match "[ch]$" (buffer-file-name x))))
                (buffer-list)))
         (count (length bufs))
         (first-buf (cl-first bufs))
         (rest-bufs (cl-rest bufs))
         (size (/ (frame-height) count)))

    (when (or (< count 5)
              (yes-or-no-p (--reduce (concat acc " " it)
                                     (-map #'buffer-name bufs))))
      (switch-to-buffer first-buf)
      (delete-other-windows)

      (--each (cl-rest bufs)
        (split-window-vertically size)
        (other-window 1)
        (switch-to-bufer it)))))

(foo "[hc]$")

;; window configuration

(setq foo (current-window-configuration))

(set-window-configuration foo)

;; curent line number
(line-number-at-pos)

;; struct

(defstruct starship
           (name nil)
           (speed 0)
           (condition 'green)
           (shields 'down))

(setf s1 (make-starship))

(typep s1 'starship)

(setf s2 (make-starship :speed 130))

(setf (starship-speed s2) 100)

(describe setf)

(equalp s1 s2)

;; read-char

(setf foo (read-char "Q action : " nil 5))

(lookup-key *fc-modal-keymap* "b")

(defun fc-modal-head-key (prompt keymap timeout)
  (let ((keys ""))
    (let ((key (read-char prompt nil timeout)))
      (setf keys (concat keys (char-to-string key)))
      (setf ret (lookup-key keymap keys))

      (message "--- %s --- %s" (type-of ret) ret)

      (cond
       ((or (typep ret 'symbol)
            (typep ret 'cons))
        (message "%s" ret)
        (call-interactively ret))

       ((numberp ret)
        (message "too long")
        (return))))))

(fc-modal-head-key "action : " *fc-modal-keymap* 5)

(concat "" (char-to-string ?a))

(type-of (lookup-key *fc-modal-keymap* "q"))

;; ivy

(ivy-read "hello" (list "1" "2" "3" "4"))

;; path
(fc-load 'exec-path-from-shell
  :after (exec-path-from-shell-initialize))

;; double-space enter modal mode
(setf *test-foo* 0)

(defun run-modal ()
  (if (eq *test-foo* 2)
      (progn
        (when (buffer-modified-p)
          (backward-delete-char 2))
        (fc-modal-global-mode 1))

    (unless (buffer-modified-p)
      (insert " ")))

  (setf *test-foo* 0))

(defun test-modal ()
  (interactive)

  (when (buffer-modified-p)
    (selfc-insert-command 1))

  (when (eq *test-foo* 0)
    (run-at-time "0.2sec" nil 'run-modal))

  (incf *test-foo*))

(global-set-key (kbd "SPC") 'test-modal)

;; iterate keymap
(defun fc-modal-dump-keymap (keymap)
  (let ((str ""))
    (map-keymap (lambda (evt func)
                  (message "--%S" evt)
                  (setf str (concat str " " (if (characterp evt)
                                                (progn
                                                  (message "convert %S %d" evt (length str))
                                                  (byte-to-string evt))
                                              (message "format %S" evt)
                                              (format "%S" evt)))))
                *fc-modal-keymap*)
    (message "%s" str)))

;; doc string
(defconst foo "hello" "docstring hello")
(defconst bar "world")

(documentation-property 'foo 'variable-documentation)
(documentation-property '*ergo-goto-map* 'variable-documentation)

;; highlight
(highlight-phrase "FIXME")

(font-lock-add-keywords モード
                        '(("正規表現" . face名)))

(font-lock-add-keywords 'org-mode
                        '(("\\<MyCompany\\>" . 'my-keyword-face)
                          ("FIXME:.*" . 'my-keyword-face)
                          ("TODO:.*" . 'my-keyword-face)))

;; relative/absolute path
(file-relative-name (buffer-file-name (current-buffer)))
(file-relative-name (buffer-file-name (current-buffer)) "~")
(expand-file-name (file-relative-name (buffer-file-name (current-buffer))))
(expand-file-name (file-relative-name (buffer-file-name (current-buffer)) "~") "~")

;; flet
(cl-flet ((f (x) (* x x)))
  (f 7))

;; use advice to modify parameter
(defun replace-regexp-in-string-test-empty (args)
  (when (null (nth 2 args))
    (setf (nth 2 args) ""))
  args)

(advice-add 'replace-regexp-in-string :filter-args
            #'replace-regexp-in-string-test-empty)
