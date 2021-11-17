;;; fc-app.el --- setup app -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'cl-lib)

(defvar *fc-rg-cpus* 0)

(fc-install 'package-utils)

(defun -fc-select-template (name)
  "Select template.
NAME: target buffer name."
  (let* ((file-regex "txt$\\|md$\\|org$")
         (template-dir (format "%s/template/" *fc-resource*))
         (templates (when (fc-dir-exists-p template-dir)
                      (directory-files
                       template-dir
                       nil
                       file-regex)))
         (site-template-dir (format "%s/site/template/" *fc-home*))
         (site-templates (when (fc-dir-exists-p site-template-dir)
                           (directory-files
                            site-template-dir
                            nil
                            file-regex)))
         (template (fc-user-select
                    (format "Select template for %s" name)
                    (--map
                     (cons (capitalize
                            (replace-regexp-in-string
                             "_\\|-"
                             " "
                             (file-name-sans-extension it)))
                           it)
                     (cl-concatenate 'list templates site-templates)))))
    (when template
      (format "%s/template/%s"
              *fc-resource* template))))

(cl-defun fc-new-buffer-with-template (bufname template)
  "New buffer with template.
BUFNAME: name of buffer.
TEMPLATE: template file path."
  (interactive (let* ((name (read-string "Buffer name : "))
                      (template (-fc-select-template name)))
                 (list name template)))

  (let* ((buf (get-buffer-create bufname)))
    (save-excursion
      (with-current-buffer buf
        (setf enable-local-variables :all
              enable-dir-local-variables nil)
        (erase-buffer)
        (insert-file-contents template)

        (goto-char (point-min)))

      (fc-pop-buf buf :automode t :select t))))

(cl-defun google-speak (&rest rest)
  "Execute google-speak script.
REST: words."
  (apply #'fc-exec-command
         "google-speak"
         (--map (replace-regexp-in-string " " "+" it)
                rest)))

(fc-add-network-advice 'google-speak)

;; open in OS
(cl-defun fc-open-in-system (&optional (path nil))
  "Open path in desktop system.
PATH: target path."
  (when (and (not path)
             buffer-file-name)
    (setf path buffer-file-name))

  (let* ((dir (shell-quote-argument
               (file-name-directory path))))
    (cond
     (*is-mac* (shell-command
                (format "open %s &" dir)
                nil
                nil))
     (*is-linux* (dbus-call-method
                  :session
                  "org.freedesktop.FileManager1"
                  "/org/freedesktop/FileManager1"
                  "org.freedesktop.FileManager1"
                  "ShowFolders"
                  (list :array
                        (format "file:%s" path))
                  "")))))

;; note buffer
(defvar *fc-note* "*scratch*" "Name of note buffer.")

(defun fc-set-note-buffer (name)
  "Set note buffer.
NAME: name of new note buffer."
  (setf *fc-note* name))

(defalias 'fc-show-hide-note (fc-manual (fc-show-hide-buffer *fc-note*)))

(defun fc--insert-org-note (orig-mode s)
  "Insert note into ORG-MODE note buffer.
ORIG-MODE: original mode,
S: note string."
  (goto-char (point-max))

  (fc-insert-space-text nil
                        "#+BEGIN_SRC " (substring (fc-string orig-mode) 0 -5)
                        "\n"
                        s)
  (fc-insert-space-text nil
                        "#+END_SRC\n"))

(defun fc--insert-node (s)
  "Inset note into note buffer.
S: note string."
  (goto-char (point-max))

  (fc-insert-space-text nil
                        "-------" (current-time-string) "-------\n"
                        s
                        "\n\n"))

(defun fc-insert-note (s)
  "Insett note into note buffer.
S: note string."
  (let ((orig-mode major-mode))
    (with-current-buffer *fc-note*
      (if (eq major-mode 'org-mode)
          (fc--insert-org-note orig-mode s)
        (fc--insert-note s)))))

;; help functions
(defun fc-show-ascii-table ()
  "Show ascii table."
  (interactive)

  (fc-popup-tip
   " <<US-ASCII>>

    2 3 4 5 6 7
  -------------
 0:   0 @ P ` p
 1: ! 1 A Q a q
 2: \" 2 B R b r
 3: # 3 C S c s
 4: $ 4 D T d t
 5: % 5 E U e u
 6: & 6 F V f v
 7: ' 7 G W g w
 8: ( 8 H X h x
 9: ) 9 I Y i y
 A: * : J Z j z
 B: + ; K [ k {
 C: , < L \ l |
 D: - = M ] m }
 E: . > N ^ n ~
 F: / ? O _ o DEL"))

(defun fc-show-common-keys ()
  "Show functions of common keys."
  (interactive)

  (fc-popup-tip
   " << Common Mark Keys >>

  a: to begining of the line
  b: whole buffer
  c: to specific character
  e: to end of the line
  l: whole line
  p: paragraph
  q: quotation
  w: current word
  p: paragraph
  f: semantic function

  ^: to beginning of buffer
  $: to end of buffer

  Upper-case means same region, but without touching kill-ring.
  "))

;; grep
(cl-defun fc-grep (pattern root &key file recursion word regex)
  "Execute grep.
PATTERN: regex pattern.
ROOT: root path.
FILE: file glob.
RECURSION: bool.
WORD: bool.
REGEX: regex."

  (let ((l nil))
    (push (getenv "GREP") l)
    (push "-nH --color -d skip" l)
    (when file
      (push (format "-a --exclude=\"*\" --include=\"%s\"" file) l))
    (when recursion
      (push "-r" l))
    (when (not regex)
      (push "-F" l))
    (when word
      (push "-w" l))

    (push (shell-quote-argument pattern) l)
    (push (if (string-suffix-p "/" root)
              (concat root "*")
            (concat root "/*"))
          l)

    (grep (s-join " " (reverse l)))))

(defun fc--ergo-grep-root (root)
  "Get root directory for grep.
ROOT: directory."
  (if (and buffer-file-name
           (equal (file-name-directory buffer-file-name)
                  (expand-file-name root)))
      "."
    root))

(defun fc--ergo-grep-file ()
  "Get grep file extentions."
  (let* ((filename buffer-file-name)
         (ext (if filename
                  (file-name-extension filename)
                nil)))
    (cond
     ((and ext
           buffer-file-name
           (member (intern ext) '(c h)))
      "*.[hc]")
     (ext (concat "*." ext))
     (t "*"))))

(defun fc-ergo-grep (regex pattern root file recursion word)
  "Execute grep.
REGEX: enable regular exp or not.
PATTERN: string or regexp.
ROOT: directory.
FILE: file name patterns.
RECURSION: recursion or not.
WORD: word boundary or not"
  (interactive (list current-prefix-arg
                     (fc-current-thing :prompt "Grep Pattern")
                     (read-directory-name "Root : " default-directory)
                     (read-string "File : " (fc--ergo-grep-file))
                     (fc-user-confirm "Recusion")
                     (fc-user-confirm "Word")))

  (fc-grep pattern (fc--ergo-grep-root root)
           :file file
           :recursion recursion
           :word word
           :regex (if regex t nil)))

(defun fc-ergo-simple-grep (pattern)
  "Simple grep.
PATTERN: target pattern."
  (interactive (list (fc-current-thing :regq t :prompt "Simple pattern" )))

  (fc-grep pattern (fc--ergo-grep-root default-directory)
           :file (fc--ergo-grep-file)
           :recursion t
           :word nil
           :regex t))

(defun fc-ergo-simple-proj-grep (pattern)
  "Simple project grep.
PATTERN: target pattern."
  (interactive (list (fc-current-thing :regq t :prompt "Project simple pattern")))

  (fc-grep pattern (file-relative-name (fc-proj-root))
           :file (fc--ergo-grep-file)
           :recursion t
           :word nil
           :regex t))

;; transparency
(defvar *fc-alpha-transparency* 75)

(defun fc-toggle-transparency ()
  "Toggle transparency."
  (interactive)

  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond
               ((numberp alpha) alpha)
               ((numberp (cdr alpha)) (cdr alpha))
               ;; Also handle undocumented (<active> <inactive>) form.
               ((numberp (cadr alpha)) (cadr alpha)))
              100)
         `(,*fc-alpha-transparency* . 50) '(100 . 100)))))

;; insert signature
(cl-defun fc-insert-figlet ()
  "Insert figlet."
  (interactive)

  (fc-insert-text (lambda (start end)
                    (fc-region start end
                      (fc-multi-line-comment-region start end)
                      (whitespace-cleanup)))
                  (fc-exec-command-to-string "figlet"
                                             "-w"
                                             (fc-string *fc-column-limit*)
                                             "-f"
                                             (fc-user-select
                                              "Select font"
                                              (--map
                                               (cons (file-name-base it)
                                                     (format "%s/extra/figlet/%s" *fc-home* it))
                                               (fc--list-file (format "%s/extra/figlet" *fc-home*)
                                                              nil
                                                              :sort t)))
                                             (read-string "Text : "))))

(cl-defun fc-insert-signature ()
  "Insert signature."
  (interactive)

  (fc-insert-text #'fc-multi-line-comment-region
                  "
  _____        ____
  |  ___|      / ___|   fridaychen@gmail.com
  | |_        | |
  |  _|    _  | |___
  |_|     (_)  \\____|
  "))

(cl-defun fc-insert-todo-block ()
  "Insert ascii art todo list block."
  (interactive)
  (fc-insert-text #'fc-multi-line-comment-region
                  "
  /)/)   ToDo:
  ( ..\\   |. []-
  /'-._)  |. []-
 /#/      |. []-
/#/       |. []-
"))

;; fc file finder
(cl-defun fc--rg-ignore (ignore-files)
  "Generate rg ignore-args.
IGNORE-FILES: ignore file glob."
  (--reduce-from (cons "-g" (cons (concat "!" it) acc))
                 nil
                 ignore-files))

(cl-defun fc--rg-types (file-types)
  "Generate rg type-args.
FILE-TYPES: fc style file types."
  (apply #'seq-concatenate
         'list
         (--map (pcase it
                  ('code '("-t" "awk"
                           "-t" "c" "-t" "cpp" "-t" "elisp" "-t" "go"
                           "-t" "py" "-t" "ruby" "-t" "rust" "-t" "sh"
                           "-t" "vim"
                           ))
                  ('doc '("-t" "markdown" "-t" "org" "-t" "txt"))
                  ('conf '("-t" "cmake" "-t" "make" "-t" "config"
                           "-t" "json" "-t" "yaml"))
                  ('xml '("-t" "xml")))
                file-types)))

(cl-defun fc--list-file-rg (dir file-types)
  "List files.
DIR: under this dir performing finding file.
FILE-TYPES: target file types to be finded."
  (let ((default-directory dir)
        (arg-type (fc--rg-types file-types))
        (arg-cpu (list "-j" (format "%d" *fc-rg-cpus*))))
    (let* ((result (apply #'fc-exec-command-to-string
                          "rg"
                          "--files"
                          "--no-ignore"
                          (seq-concatenate
                           'list
                           arg-cpu
                           arg-type)))
           (files (split-string (string-trim result) "\n")))
      files)))

(cl-defun fc--list-file-ff (dir file-types)
  "List files.
DIR: under this dir performing finding file.
FILE-TYPES: target file types to be finded."
  (let* ((default-directory dir)
         (arg-type (--map (format "-%s" it) file-types)))
    (let* ((result (apply #'fc-exec-command-to-string
                          "ff"
                          "-nocolor"
                          arg-type))
           (files (split-string (string-trim result) "\n")))
      files)))

(cl-defun fc--list-file (dir file-types &key sort)
  "List files.
DIR: under this dir performing finding file.
FILE-TYPES: target file types to be finded.
SORT: sort or not."
  (let ((files (fc--list-file-rg dir file-types)))
    (if sort
        (sort files #'string<)
      files)))

(cl-defun fc--find-file (dir prompt file-types &key sort)
  "Find file.
DIR: under this dir performing finding file.
PROMPT: prompt string.
FILE-TYPES: target file types to be finded.
SORT: sort files."
  (let ((files (fc--list-file dir file-types :sort sort)))
    (fc-user-select prompt files)))

;; fc text retrieve
(cl-defun fc--internal-ftr-rg (dir pattern file-types &key ignore-files)
  "Ftr with ripgrep.
DIR: under this dir performing search.
PATTERN: target regex pattern.
FILE-TYPES: target file types to be searched."
  (let ((default-directory dir)
        (arg-type (fc--rg-types file-types))
        (arg-cpu (list "-j" (format "%d" *fc-rg-cpus*)))
        (arg-ignore (fc--rg-ignore ignore-files)))
    (apply #'fc-exec-command-to-buffer
           (current-buffer)
           "rg"
           "--vimgrep"
           "--stats"
           "--no-ignore"
           "-S"
           pattern
           (seq-concatenate
            'list
            arg-ignore
            arg-cpu
            arg-type)))

  (goto-char (point-min))
  (setf enable-local-variables :all)
  (insert
   (format
    "-*- mode: grep; enable-dir-local-variables: nil; enable-local-variables: \"all\"; default-directory: \"%s\"; -*-\n"
    dir)))

(cl-defun fc--external-ftr-rg (dir pattern _file-types)
  "Ftr with ftr.
DIR: under this dir performing search.
PATTERN: target regex pattern.
FILE-TYPES: target file types to be searched."
  (fc-exec-command-to-buffer
   (current-buffer)
   "ftr"
   "-code"
   "-conf"
   "-doc"
   "-emacs"
   "-rp"
   dir
   pattern))

(defun fc-text-retrieve (dir &key ignore-files)
  "Text retrieve.
DIR: dir to search."
  (let* ((pattern (fc-current-thing :regq t :confirm t))
         (bufname (format "*fc text retrieve %s*" pattern))
         (buf (get-buffer-create bufname))
         (filename (when buffer-file-name
                     (file-name-nondirectory buffer-file-name))))
    (save-excursion
      (with-current-buffer buf
        (fc--internal-ftr-rg dir pattern '(code conf doc xml) :ignore-files ignore-files)

        (goto-char (point-min))

        (when filename
          (search-forward filename nil t)))

      (fc-pop-buf buf :automode t :read-only t :highlight (list pattern)))))

;; eshell extensions
(setenv "_FASD_FUZZY" "16")

(defun fc-eshell-dirtrim (path n full-prefix short-prefix)
  (let* ((parts (-filter #'fc-not-void-p (split-string path "/"))))
    (if (<= (length parts) n)
        (concat full-prefix path)
      (concat short-prefix
              (string-join (last parts n)
                           "/")))))

(defun fc-eshell-pwd ()
  (let ((pwd (eshell/pwd))
        (home (getenv "HOME"))
        (prompt_dirtrim 5))
    (cond
     ((string-equal home pwd)
      "~")

     ((string-prefix-p home pwd)
      (fc-eshell-dirtrim
       (file-relative-name pwd home)
       prompt_dirtrim
       "~/"
       "~/.../"
       ))

     (t
      (fc-eshell-dirtrim pwd prompt_dirtrim "" ".../")))))

(defun fc-eshell-prompt-function ()
  (let ((branch (magit-get-current-branch)))
    (concat
     "\n"
     (fc-text (user-login-name) :face '(:foreground "SkyBlue"))
     (fc-text ":" :face '(:foreground "red"))
     (fc-eshell-pwd)
     (if branch
         (fc-text
          (format " %s %s%s"
                  (fc-visible "" "^")
                  branch
                  (shell-command-to-string "git status -s | awk -f ${FCHOME}/bin/ps-fit.awk"))
          :face '(:foreground "OrangeRed" :inherit bold)))
     "\n"
     (fc-visible "╍❱ " "-> "))))

(setf eshell-prompt-function #'fc-eshell-prompt-function
      eshell-prompt-regexp (fc-visible "^[^\n]*\n╍❱ " "^[^\n]*\n-> "))

(defun j (&rest args)
  "Fast jump.
ARGS: args for fast jump."
  (if (/= 1 (length args))
      "input one argument only !"
    (cd (string-trim (shell-command-to-string
                      (concat "fasd -d " (car args)))))
    ""))

(defun r ()
  "Jump to project root."
  (cl-loop
   with dir = default-directory
   do
   (when (--first (file-exists-p (concat dir it)) '("TOP" ".TOP"))
     (cd dir)
     (cl-return))

   (setf dir
         (file-name-directory (substring dir 0 -1)))
   while (> (length dir) 1)))

(defun lsd (&rest args)
  "List directory only.
ARGS: ls patterns."
  (interactive)

  (if (not args)
      (shell-command-to-string "ls -d */" )
    (string-join (-map
                  (lambda (x)
                    (shell-command-to-string (format "ls -d \"%s\"/*/" x)))
                  args)
                 "")))

(defun fc--find-one-file (prompt args)
  "Let user select one file for list.
PROMPT: user prompt string.
ARGS: args for ff."
  (let* ((result (apply #'fc-exec-command-to-string
                        "ff"
                        "-nocolor"
                        args))
         (files (if (string-equal "" result)
                    ()
                  (split-string (string-trim result) "\n")))
         (file
          (fc-user-select prompt
                          files)))
    file))

(cl-defmacro ff-run (prompt args &rest rest)
  "FF run.
PROMPT: prompt for user to select.
ARGS: file types.
REST: commands to run."
  (declare (indent 2))
  `(let ((file (fc--find-one-file ,prompt ,args)))
     (if (not file)
         "No file was found !!!"
       ,@rest)))

(defun fn (&rest args)
  "Open file in new buffer.
ARGS: args for ff."
  (ff-run "Select file to open" (or args '("-code" "-conf" "-doc"))
    (find-file file)))

(defun fo (&rest args)
  "Open file in system.
ARGS: args for ff."
  (ff-run "Select file to open in other app" args
    (fc-exec-command "fj" "--open" file)))

(defun fplay ()
  "Play media file in system."
  (ff-run "Select media file to play" '("-media")
    (fc-exec-command "fj" "--play" file)))

(defun fplayv ()
  "Play media file in system, video only."
  (ff-run "Select video file to play" '("-video")
    (fc-exec-command "fj" "--playv" file)))

(defun fplaya ()
  "Play media file in system, audio only."
  (ff-run "Select video file to play"
      '("-media")
    (fc-exec-command "fj" "--playa" file)))

(defun fc-init-eshell ()
  "Init eshell."
  (interactive)

  (eshell/alias "b" "fj-build $*")
  (eshell/alias "c" "cat $*")

  (eshell/alias "cd.." "cd ..")
  (eshell/alias "cd-" "cd -")

  (eshell/alias "gp" "git pull $*")
  (eshell/alias "gq" "git push $*")

  (eshell/alias "la" "ls -A $*")
  (eshell/alias "ll" "ls -Alh $*")
  (eshell/alias "lst" "ls -Alh -rt $*")
  (eshell/alias "lss" "ls -Alh -rS $*")
  )

;; f key sequence
(defvar *fc-key-seq* "")

(defun fc-set-key-seq ()
  "Set key sequences."
  (interactive)

  (let ((keys (read-string "Keys : ")))
    (setf *fc-key-seq* keys)
    (fc-ergo-repeat-func 'fc-run-key-seq)))

(defun fc-run-key-seq ()
  "Run key sequence."
  (interactive)

  (execute-kbd-macro (kbd *fc-key-seq*)))

;; spotlight
(defun fc-spotlight ()
  "Run spotlight."
  (interactive)

  (if *is-linux*
      (fc-exec-command "xdotool" "key" "--delay" "100" "Super+s")))

;; welcome buffer
(defun fc-show-welcome ()
  "Show welcome message."
  (defvar *fc-welcome-scale* 4)
  (setf inhibit-startup-message t)
  (text-mode)

  (insert-file-contents (fc-file-first-exists
                         '("~/.emacs.d/welcome.txt"
                           "~/.emacs.d/fconfig/welcome.txt")))

  (if *is-gui*
      (text-scale-decrease *fc-welcome-scale*)))

;; isearch
(fc-load 'isearch
  :local t
  :autoload t
  :after (progn
           (defun fc-isearch-dwim ()
             "Do search dwim."
             (interactive)

             (let ((target (fc-current-thing)))
               (isearch-resume target nil nil t target nil))))
  :bind '((isearch-mode-map
           ("C-j" isearch-repeat-forward)
           ("C-k" isearch-repeat-backward))))

;; search
(defvar *fc-app-search-regex* nil)

(cl-defun fc--search-set-target (regex)
  "Set search target.
REGEX: target."
  (fc-bind-keys `(("C-r" ,(fc-manual
                           (fc-search-next nil t)))))

  (when *fc-app-search-regex*
    (unhighlight-regexp *fc-app-search-regex*))

  (setf *fc-app-search-regex* regex)

  (when *fc-app-search-regex*
    (highlight-regexp *fc-app-search-regex*)))

(cl-defun fc-search-next (&optional (regex nil) (backward nil))
  "Search next.
REGEX: target regex.
BACKWARD: search direction."
  (interactive)

  (cond (regex
         (fc--search-set-target regex))

        ((region-active-p)
         (fc--search-set-target (fc-current-thing :regq t)))

        ((not *fc-app-search-regex*)
         (fc--search-set-target (fc-current-thing :regq t
                                                  :confirm t
                                                  :prompt "Search regex: "))))

  (if backward
      (re-search-backward *fc-app-search-regex* 0 t)
    (re-search-forward *fc-app-search-regex* (point-max) t)))

(cl-defun fc-search-stop ()
  "Stop search."
  (fc-bind-keys `(("C-r" isearch-backward)))
  (fc--search-set-target nil))

;; search engine
(fc-load 'google-this
  :autoload t
  :bind '((nil
           ("M-*" google-this))
          (*fc-modal-keymap*
           ("*" google-this))))

;; current dir tree
(fc-load 'neotree
  :autoload t
  :after (progn
           (defvar *fc-tree-dir* nil)

           ;; setup neotree
           (setf neo-show-hidden-files t
                 neo-hidden-regexp-list *fc-ignore-file*
                 neo-theme (if *is-gui* 'classic 'ascii)
                 neo-show-hidden-files nil)

           (cl-defun fc-show-tree ()
             (interactive)

             (when (equal (neo-global--get-window)
                          (get-buffer-window))
               (neotree-hide)
               (cl-return-from fc-show-tree))

             (let ((dir (file-name-directory buffer-file-name)))
               (cond
                ((and (neo-global--window-exists-p)
                      (equal *fc-tree-dir* dir))
                 (neotree-hide))

                ((and (equal *fc-tree-dir* dir))
                 (neotree-show))

                (t
                 (setf *fc-tree-dir* dir)
                 (neotree-dir (file-name-directory buffer-file-name)))))))

  :bind '((*fc-modal-keymap*
           ("Z" fc-show-tree))))

;; development mode
(defun fc-dev-mode-toggle ()
  "Toggle dev mode."
  (interactive)

  (fc-toggle-var '*fc-dev-mode*)

  (if *fc-dev-mode*
      (progn
        (setf debug-on-error t
              warning-minimum-level :debug)

        (run-hooks '*fc-enable-dev-hook*))
    (setf debug-on-error nil
          warning-minimum-level :error)
    (run-hooks '*fc-disable-dev-hook*)))

;; multiple buffers functions
(cl-defun fc--run-multi-buffer (dir func)
  "Do run func on multi buffers which under dir.
DIR: root dir.
FUNC: function to be run."
  (setf dir (concat (expand-file-name dir)
                    (if (string-suffix-p "/" dir) "" "/")))

  (--each (fc-list-buffer :dir dir)
    (with-current-buffer it
      (fc-funcall func))))

(cl-defmacro fc-run-multi-buffer (operation &rest rest)
  "Exec operation over multi buffers.
OPERATION: target operation.
REST: commands."
  (declare (indent 1))
  `(lambda () (interactive)
     (let ((dir (read-directory-name (format "Select directory for %s :" ,operation))))
       (when dir
         (setf dir (concat (expand-file-name dir)
                           (if (string-suffix-p "/" dir) "" "/")))

         (when (fc-user-confirm (format "%s files under %s" (capitalize ,operation) dir))
           (--each (fc-list-buffer :dir dir)
             (with-current-buffer it
               ,@rest)))))))

(defun fc-select-multi-buffer-func ()
  "Select multi buffer function."
  (fc-user-select-func
   "Multi buffer"
   `(("close"  . ,(fc-run-multi-buffer "close" (kill-buffer)))
     ("revert" . ,(fc-run-multi-buffer "revert" (fc-recover-revert-buffer)))
     ("refresh" . ,(fc-run-multi-buffer "refresh" (vc-refresh-state))))))

;; git utilities
(defun fc-select-git-func ()
  "Select git function."
  (interactive)

  (fc-user-select-func
   "Git"
   `(("amend"			.	fc-git-amend)
     ("cancel last commit"	.	fc-git-cancel-last-commit)
     ("search"			.	fc-git-search)
     ("pull"			.	,(fc-manual (shell-command "git pull")))
     ("push"			.	,(fc-manual (shell-command "git push"))))))

;; project utilities
(defun fc-select-proj-func ()
  "Select proj function."

  (fc-user-select-func
   "Project"
   `(
     ("auto format"	  . ,(lambda ()
                               (fc-proj--set *fc-project*
                                             (fc-user-confirm "Enable auto format")
                                             :local nil '*fc-format-at-save*)
                               (fc-proj--save *fc-project*)))
     ("clang style"       . ,(lambda ()
                               (fc-proj--set *fc-project*
                                             (fc-user-select "Clang style" '("LLVM"
                                                                             "Google"
                                                                             "Chromium"
                                                                             "Mozilla"
                                                                             "WebKit"
                                                                             "Microsoft"))
                                             :local nil 'fc-proj-clang-style)
                               (fc-proj--save *fc-project*)))
     ("close files"       . ,(lambda ()
                               (fc--run-multi-buffer
                                (fc-proj-root)
                                (lambda () (kill-buffer)))))
     ("load error file"   . fc-proj-load-compilation-error)
     ("open"              . fc-proj-open)
     ("property"          . fc-proj-select-property-to-edit)
     ("rename"            . fc-proj-query-rename)
     ("refresh"           . ,(lambda () (fc--run-multi-buffer (fc-proj-root) #'vc-refresh-state)))
     ("save"              . ,(lambda () (fc-proj--save *fc-project*)))
     ("switch"            . fc-proj-switch)
     ("tab indent mode"   . ,(lambda ()
                               (fc-proj--set *fc-project*
                                             (fc-user-confirm "Enable tab indent mode")
                                             :local 'c-mode 'indent-tabs-mode)
                               (fc-proj--save *fc-project*)))
     ("tab width"         . ,(lambda ()
                               (let ((tabwidth (string-to-number (read-string "Tab width : "))))
                                 (fc-proj--set *fc-project* tabwidth :local 'c-mode 'c-basic-offset)
                                 (fc-proj--set *fc-project* tabwidth :local 'c-mode 'tab-width)
                                 (fc-proj--save *fc-project*))))
     ("update local vars" . ,(lambda ()
                               (fc-proj--update-local-vars *fc-project*)
                               (fc-proj--save *fc-project*)))
     ("work"              . ,(lambda ()
                               (fc-proj--set *fc-project*
                                             (fc-user-confirm "Remote work")
                                             :local nil 'fc-proj-work)
                               (fc-proj--save *fc-project*))))))

;; desktop
(require 'desktop)

(cl-defun fc-get-desktop-path ()
  "Get desktop path."
  (let ((dir (cond
              (*is-gui*
               "~/.emacs.d/desktop-gui")
              (*is-colorful*
               "~/.emacs.d/desktop-colorful")
              (t
               "~/.emacs.d/desktop-linux"))))
    (unless (fc-dir-exists-p dir)
      (make-directory dir))
    dir))

(cl-defun fc-own-desktop-p ()
  "Test if current Emacs owns desktop file."
  (eq (desktop-owner (fc-get-desktop-path))
      (emacs-pid)))

(cl-defun fc-load-desktop ()
  "Load desktop."
  (desktop-read (fc-get-desktop-path)))

(cl-defun fc-save-desktop ()
  "Save to desktop."
  (when (or (fc-own-desktop-p)
            (fc-user-confirm "Save desktop"))
    (unless (fc-own-desktop-p)
      (let ((desktop-dirname (fc-get-desktop-path)))
        (desktop-remove)))

    (desktop-save (fc-get-desktop-path))
    (recentf-save-list)))

;; profiler
(defun fc-profile-startup ()
  "Start profiler."
  (fc-load 'esup)

  (esup))

;; theme
(cl-defun fc-select-theme ()
  "Allow user to select theme."
  (let* ((s (fc-user-select "Themes"
                            (custom-available-themes)))
         (theme (when s (intern s))))
    (when (and theme
               (not (eql theme *fc-current-theme*)))
      (fc-load-theme theme))))

(defun fc-init-dir-locals ()
  "Copy default .dir-locals.el."
  (let ((dir (read-directory-name "Target dir : ")))
    (when dir
      (fc-exec-command
       "cp"
       (expand-file-name
        "~/.emacs.d/fconfig/resource/dot-dir-locals.el")
       (concat dir "/.dir-locals.el")))))

(defun fc-decode-ansi-esc-code ()
  "Decode ansi escape code."
  (let ((start (if (region-active-p)
                   (region-beginning)
                 (point-min)))
        (end (if (region-active-p)
                 (region-end)
               (point-max))))
    (ansi-color-apply-on-region start end)))

(defun dos2unix ()
  "Convert file format from dos to unix."
  (interactive)

  (save-excursion
    (set-buffer-file-coding-system 'unix)

    (fc-replace-string "\r" "" :from-start t)))

(defun unix2dos ()
  "Convert file format from unix to dos."
  (interactive)

  (save-excursion
    (fc-replace-regexp "\\([^\r]\\)\n"
                       "\\1\r\n"
                       :from-start t)
    (fc-replace-regexp "^\n"
                       "\r\n"
                       :from-start t)))

(defun fc-forecast ()
  "Exec forecast."
  (interactive)

  (with-current-buffer (get-buffer-create "*fc-weather*")
    (read-only-mode -1)
    (erase-buffer)
    (insert (shell-command-to-string "curl -s wttr.in"))
    (fc-decode-ansi-esc-code)
    (goto-char (point-min))
    (text-mode)
    (fc-pop-buf (current-buffer) :read-only t)))

(defun fc-config-line-space ()
  "Setup line space for all file buffers."
  (setf *fc-basic-line-spacing* (string-to-number
                                 (read-string
                                  "New line space"
                                  (fc-string *fc-basic-line-spacing*))))
  (--each (fc-list-buffer)
    (with-current-buffer
        (setf line-spacing *fc-basic-line-spacing*))))

(defconst *fc-app-font-size-map*
  (fc-make-keymap
   `(
     ("j" ,(fc-manual (fc-adjust-font -1)
                      (format "%d" *fc-font-height*)))
     ("k" ,(fc-manuals (fc-adjust-font 1)
                       (format "%d" *fc-font-height*)))
     )
   "font size keymap")
  "KEYS  j: down  k: up.")

;; snippets
(defun fc-app-create-snippet ()
  "Create a new snippet."
  (let* ((mode (read-string "Mode" (fc-string major-mode)))
         (name (read-string "Name"))
         (key (read-string "Key"))
         (buf (generate-new-buffer name)))
    (switch-to-buffer buf)
    (setf buffer-file-name (format "%s/emacs/snippets/%s/%s" *fc-home* mode name))
    (snippet-mode)
    (insert (format "# -*- mode: snippet -*-
# name: %s
# key: %s
# --\n
"
                    name key))))

(defun fc-select-other-func ()
  "Select other function."
  (fc-user-select-func
   "Other"
   `(
     ("de-ansi esc color"	. fc-decode-ansi-esc-code)
     ("dos2unix"		. dos2unix)
     ("hex2string"              . fc-c-hex2string)
     ("init dir-locals"		. fc-init-dir-locals)
     ("insert figleted string"  . fc-insert-figlet)
     ("insert signature"        . fc-insert-signature)
     ("load latest desktop"	. ,(fc-manual (fc-load-desktop)))
     ("new snippet"             . fc-app-create-snippet)
     ("string2hex"              . fc-c-string2hex)
     ("tabify"                  . ,(fc-manual (tabify (point-min)
                                                      (point-max))))
     ("toggle visual line move" . ,(fc-manual (fc-toggle-var 'line-move-visual)))
     ("unix2dos"		. unix2dos)
     ("untabify"                . ,(fc-manual (untabify (point-min)
                                                        (point-max))))
     )))

(cl-defun fc-toggle-server ()
  "Toggle server mode."
  (if server-mode
      (server-mode -1)
    (server-mode 1)))

(defun fc-select-sys-func ()
  "Select system function."
  (let ((server-title (if server-mode "server stop" "server start")))
    (fc-user-select-func
     "System"
     `(("location"	  . fc-update-location)
       ("sound sink"      . fc-app-select-sound-sink)
       ("package"	  . list-packages)
       ("profile startup" . fc-profile-startup)
       (,server-title	  . fc-toggle-server)
       ("upgrade"	  . (lambda () (fc-run (package-utils-upgrade-all))))))))

(defun fc-select-ui-func ()
  "Select system function."
  (fc-user-select-func
   "UI"
   `(("font"         . fc-config-font)
     ("font size"    . ,(fc-head-key-repeat "Adjust font size"
                                            '*fc-app-font-size-map*))
     ("line space"   . fc-config-line-space)
     ("theme"        . fc-select-theme)
     ("theme reset"  . fc-reset-theme)
     )))

(advice-add 'package-utils-upgrade-all :after #'fc-job-done)

;; occur
(defun fc-occur-dwim ()
  "Run occru dwim."
  (interactive)

  (let ((s (fc-current-thing :regq t :confirm t))
        (target (format-mode-line "%l:")))
    (occur s)

    (with-selected-window (get-buffer-window "*Occur*")
      (when (search-forward target)
        (search-forward s)))))

;; vi-style f command
(defvar *fc-move-char* nil)
(defvar *fc-move-func* nil)

(cl-defun fc-forward-to-char (c)
  "Move forward to char.
C: target char."
  (interactive (list (progn
                       (fc-ergo-repeat-func 'fc-repeat-to-char)
                       (read-char "Forward to char : " nil *ergo-prefix-timeout*))))

  (unless c
    (cl-return-from fc-forward-to-char))

  (setf *fc-move-char* c
        *fc-move-func* 'fc-forward-to-char)

  (when (= (char-after (point)) c)
    (forward-char))
  (search-forward (char-to-string c))
  (backward-char))

(cl-defun fc-backward-to-char (c)
  "Move backward to char.
C: target char."
  (interactive (list (progn
                       (fc-ergo-repeat-func 'fc-repeat-to-char)
                       (read-char "Backward to char : " nil *ergo-prefix-timeout*))))

  (unless c
    (cl-return-from fc-backward-to-char))

  (setf *fc-move-char* c
        *fc-move-func* 'fc-backward-to-char)

  (search-backward (char-to-string c)))

(defun fc-repeat-to-char ()
  "VI style ;."
  (interactive)

  (if (and *fc-move-func*
           *fc-move-char*)
      (funcall *fc-move-func* *fc-move-char*)
    (message "Empty recent search char")))

;; convert between string and hex
(defun fc--string2hex (prefix suffix)
  "Convert string to hex and insert."
  (--each (append (fc-current-thing :prompt "String") nil)
    (insert (format "%s%02x%s" prefix it suffix))))

(defun fc-string2hex ()
  (interactive)

  (cl-multiple-value-bind (prefix suffix)
      (pcase major-mode
        ((guard (derived-mode-p 'prog-mode))
         (list "0x" ", "))

        (_
         (list "" " ")))
    (fc--string2hex prefix suffix)))

(defun fc-hex2string (start end)
  "Convert hex to string.
START: source of region.
END: end of region."
  (interactive "r")

  (fc-region start end
    (cl-loop
     initially (goto-char 0)
     while (re-search-forward "[0-9a-fA-F]\\{2\\}" nil t)
     collect (string-to-number (match-string 0) 16) into numbers
     finally do
     (if (null numbers)
         (message "Hex number is not found !")
       (message "string is %s"
                (kill-new (apply #'string numbers))))
     (deactivate-mark))))

;; set sound sink
(defun fc-app-select-sound-sink ()
  "Select sound sink."
  (let ((sink (fc-user-select "Select sound sink"
                              (split-string
                               (fc-exec-command-to-string "fc-sound-sink")
                               "\n" t)
                              :mouse t)))
    (when sink
      (fc-exec-command "pactl" "set-default-sink" sink))))

;; app portal
(defun fc-app-portal ()
  "Run app portal."
  (interactive)

  (fc-user-select-func
   "App"
   `(
     ("git"	.	fc-select-git-func)
     ("multi"	.	fc-select-multi-buffer-func)
     ("project" .	fc-select-proj-func)
     ("other"   .	fc-select-other-func)
     ("sys"	.	fc-select-sys-func)
     ("ui"	.	fc-select-ui-func)
     )
   :default #'(lambda () (message "No app is selected !!!"))))

;; help portal
(defun fc-help-portal ()
  "Run help portal."
  (interactive)

  (fc-user-select-func
   "Help"
   `(("ascii"  . fc-show-ascii-table)
     ("common" . fc-show-common-keys)
     )))

(provide 'fc-app)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-app.el ends here
