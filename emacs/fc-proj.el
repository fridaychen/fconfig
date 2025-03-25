;;; fc-project.el --- project management -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(defclass fc-project ()
  ((conf :initarg :conf
         :initarg nil
         :type fc-conf)
   (dir :initarg :dir
        :initform ""
        :type string
        :accessor fc-proj--dir)
   (last-target :initform nil)))

(cl-defmethod cl-print-object ((x fc-project) stream)
  (princ (format "Project: %s"
                 (fc-proj--get x :name))
         stream))

(defvar *fc-proj-repo-path* (fc-file-first-exists
                             '("~/REPOS/"
                               "~/Projects/REPOS")))
(defvar *fc-proj-list* (make-hash-table))

(cl-defun fc--proj-replace-var (proj path)
  "Replace path variables.
PROJ: project object.
PATH: path."
  (cond
   ((string-prefix-p "$REPO" path)
    (expand-file-name (concat *fc-proj-repo-path* (substring path 5))))

   ((string-prefix-p "$PROJ" path)
    (expand-file-name (concat (fc-proj--dir proj) (substring path 5))))

   (t
    path)))

(cl-defmethod fc-proj--get ((x fc-project) &rest keys)
  (let ((ret (apply 'fc-proj--get-raw x keys)))
    (cond
     ((null ret)
      ret)

     ((listp ret)
      (fc-map ret
        (if (stringp it)
            (fc--proj-replace-var x it)
          it)))

     ((stringp ret)
      (fc--proj-replace-var x ret))

     (t ret))))

(cl-defmethod fc-proj--get-raw ((x fc-project) &rest keys)
  (let ((ret (apply 'fc-conf-get (oref x conf) keys)))
    (if ret (cdr ret) nil)))

(cl-defmethod fc-proj--set ((x fc-project) val &rest keys)
  (apply 'fc-conf-put (oref x conf) val keys))

(cl-defmethod fc-proj--save ((x fc-project))
  (when (fc-conf-save (oref x conf))
    (fc-proj--update-local-vars x)))

(cl-defmethod fc-proj--exec ((x fc-project) &rest args)
  "Run command under project.
X: project obj.
ARGS: command arguments."
  (fc-with-dir (fc-proj--get x :path)
    (shell-command
     (string-join args ""))))

(cl-defmethod fc-proj--opentag ((x fc-project) &rest _args)
  (fc-tag-open-project
   (fc-proj--dir x)
   (fc-proj--get x :src)))

(cl-defmethod fc-proj--build ((x fc-project) target)
  "Compile project.
X: project obj.
TARGET: make target."
  (let ((old-path (getenv "PATH")))
    (fc-add-env-paths (fc-proj--get x :path))

    (fc-each (fc-proj--get x :env)
      (if (consp it)
          (setenv (car it) (cadr it))))

    (fc-with-dir (oref x dir)
      (compile (format "fj-build %s" target)))
    (setenv "PATH" old-path)))

(cl-defmethod fc-proj--update-local-vars ((x fc-project))
  "Update local var def file.
X: project obj."
  (let ((include (fc-proj--get x :include))
        (define (fc-proj--get x :define))
        (build-args (fc-proj--get x :build-args))
        (proj-arg (fc-proj--get x :local)))

    ;; .clang_complete
    (let ((var-filename (expand-file-name
                         (concat (fc-proj--dir x)
                                 "/.clang_complete"))))
      (delete-file var-filename)

      (with-current-buffer (get-buffer-create (find-file var-filename))
        (fc-map define (insert "-D" it "\n"))
        (fc-map build-args (insert it "\n"))
        (fc-map include (insert "-I" it "\n"))
        (save-buffer)))

    ;; .dir-locals.el
    (let ((var-filename (expand-file-name (concat (fc-proj--dir x) "/.dir-locals.el"))))
      (delete-file var-filename)

      (with-current-buffer (get-buffer-create (find-file var-filename))
        (add-dir-local-variable nil 'fc-proj-name (fc-proj--get x :name))
        (add-dir-local-variable nil 'fc-proj-main-branch (fc-proj--get x :main-branch))
        (add-dir-local-variable nil 'fc-proj-tag (fc-proj--get x :tag))
        (add-dir-local-variable nil 'fc-capture-tags (fc-proj--get x :capture-tags))

        ;; company-clang
        (let ((clang-args (append (fc-map define (concat "-D" it))
                                  build-args
                                  (fc-map include (concat "-I" it)))))
          (add-dir-local-variable 'c-mode
                                  'company-clang-arguments
                                  clang-args))

        ;; flycheck-clang
        (add-dir-local-variable 'c-mode 'flycheck-clang-include-path include)

        (add-dir-local-variable 'c-mode 'flycheck-clang-definitions define)

        (add-dir-local-variable 'c-mode 'flycheck-clang-args build-args)

        ;; project specific local
        (fc-each proj-arg
          (let ((mode (cl-first it))
                (vars (cl-rest it)))
            (fc-each vars
              (add-dir-local-variable mode (car it) (cdr it)))))
        (save-buffer)))))

(cl-defun fc-proj-wizard (dir)
  "Wizard to create project file.
DIR: project path."
  (interactive "DFProject directory : ")

  (let* ((path (concat dir "/.cricket"))
         (conf (fc-conf-new path)))

    (fc-conf-put conf (read-string "Project name : ") :name)
    (fc-conf-put conf (read-string "Main branch name : " "master") :main-branch)
    (fc-conf-put conf nil :src)
    (fc-conf-put conf nil :src-exclude)
    (fc-conf-put conf nil :include)
    (fc-conf-put conf nil :define)
    (fc-conf-put conf nil :build-args)
    (fc-conf-put conf nil :env)
    (fc-conf-put conf nil :path)
    (fc-conf-put conf nil :local)
    (fc-conf-put conf nil :capture-tags)
    (fc-conf-put conf
                 (make-symbol
                  (fc-select "Select tag system"
                      '("global"
                        "lsp"
                        "xref"
                        "none")))
                 :tag)

    (fc-conf-save conf)

    (fc-project :conf conf :dir dir)))

(cl-defun fc-proj-open (&optional (dir (expand-file-name default-directory)))
  (interactive)

  (let* ((proj-file (fc-exists-file-in-path ".cricket" dir))
         (proj-dir (and proj-file (file-name-directory proj-file))))
    (unless proj-file
      (if (fc-yes-no "Project not exists, run wizard to create one")
          (fc--proj-add (fc-proj-wizard
                         (read-directory-name "Project directory : "))))
      (cl-return-from fc-proj-open))

    (let ((proj (fc-first *fc-projects*
                  (equal (oref it dir) proj-dir))))
      (when (and proj
                 (not (fc-user-confirm "Reopen project" nil)))
        (fc--proj-set proj)
        (cl-return-from fc-proj-open)))

    (let* ((conf (fc-conf-open proj-file))
           (proj (fc-project :conf conf :dir proj-dir)))
      (fc--proj-add proj))))

(cl-defun fc--proj-add (proj)
  (let* ((pdir (oref proj dir))
         (p (-first (lambda (x) (equal (oref x dir) pdir))
                    *fc-projects*)))
    (when p
      (delete p *fc-projects*)))

  (add-to-list '*fc-projects* proj)
  (fc--proj-set proj))

(cl-defun fc--proj-set (proj)
  (unless (eq proj *fc-project*)
    (setf *fc-project* proj)
    (setf *fc-project-name* (fc-proj--get proj :name))
    (fc-proj--opentag proj)

    (let ((note (concat (oref proj dir) "/notes")))
      (fc-set-note-buffer (buffer-name (find-file note))))
    (run-hooks '*fc-project-hook*)))

(cl-defun fc-proj-switch ()
  (fc--proj-set (fc-select "Projects"
                    (fc-map *fc-projects*
                      (cons (fc-string it) it)))))

(cl-defun fc-proj-edit-property (proj prop)
  (let* ((vstr (read-string (format "Edit [%s] : " prop)
                            (format "%S" (fc-proj--get-raw proj prop))))
         (v (read-from-string vstr)))
    (if (and v (car v))
        (progn
          (fc-proj--set proj (car v) prop)
          (fc-proj--save proj)))))

(cl-defmacro fc-edit-property-fn (tag)
  `(lambda ()
     (fc-proj-edit-property *fc-project* ,tag)))

(cl-defun fc-proj-select-property-to-edit ()
  (fc-select-func
   "Project properties"
   `(
     (":build-args"   . ,(fc-edit-property-fn :build-args))
     (":build-dir"    . ,(fc-edit-property-fn :build-dir))
     (":capture-tags" . ,(fc-edit-property-fn :capture-tags))
     (":define"       . ,(fc-edit-property-fn :define))
     (":env"          . ,(fc-edit-property-fn :env))
     (":error-file"   . ,(fc-edit-property-fn :error-file))
     (":include"      . ,(fc-edit-property-fn :include))
     (":local"        . ,(fc-edit-property-fn :local))
     (":name"         . ,(fc-edit-property-fn :name))
     (":main-branch"  . ,(fc-edit-property-fn :main-branch))
     (":path"         . ,(fc-edit-property-fn :path))
     (":src"          . ,(fc-edit-property-fn :src))
     (":src-exclude"  . ,(fc-edit-property-fn :src-exclude))
     (":tag"          . ,(fc-edit-property-fn :tag)))))

(cl-defun fc-proj-build ()
  "Build current project."

  (when-let* ((target (read-string "Build target : " (oref *fc-project* last-target))))
    (oset *fc-project* last-target target)
    (fc-proj--build *fc-project* target)))

(cl-defun fc-proj-load-compilation-error ()
  (when-let* ((proj-dir (and *fc-project* (fc-proj--dir *fc-project*)))
              (error-file (fc-proj--get *fc-project* :error-file))
              (build-dir (or (fc-proj--get *fc-project* :build-dir)
                             "")))
    (fc-pop-buf (find-file-noselect (format "%s/%s" proj-dir error-file))
                :mode 'compilation-mode
                :select t
                :dir (format "%s/%s" proj-dir build-dir))
    (goto-char (point-max))))

(defun fc-user-select-project ()
  "Allow user to select project."
  (let ((proj (fc-select (format "Project <%s>" *fc-project-name*)
                  (fc-map *fc-projects*
                    (cons (fc-proj--get it :name) it)))))
    (when proj
      (fc--proj-set proj))))

(fc-load 'projectile
  :autoload t
  :after (progn
           (setf projectile-completion-system *fc-completion*)

           (defalias 'projectile-project-root 'fc-proj-root)
           (defalias 'fc-projectile-grep 'projectile-grep)))

;; project utils
(cl-defun fc-proj-recentf ()
  (when-let* ((root (fc-proj-root))
              (files (fc-filter recentf-list
                       (string-prefix-p root it))))
    (find-file (fc-select "Project recentf"
                   (fc-map files
                     (cons (file-relative-name it root) it))))
    (cl-return-from fc-proj-recentf))

  (message "No project or recent files !!!"))

(cl-defun fc-proj-root (&optional (dir (expand-file-name default-directory)))
  (unless dir
    (cl-return-from fc-proj-root nil))

  (if (boundp 'fc-proj-name)
      (fc-locate-file-in-path '("TOP" ".TOP" ".cricket") dir)
    (fc-locate-file-in-path '("TOP" ".TOP" ".cricket" ".git" ".svn") dir)))

(cl-defun fc-proj-name (&optional dir)
  "Find the project name under root.
DIR: project path."
  (if-let* ((root (fc-proj-root dir)))
      (if (boundp 'fc-proj-name)
          fc-proj-name
        (file-name-nondirectory (directory-file-name root)))
    ""))

(cl-defun fc-proj-find-file (&optional dir)
  (let* ((root (if dir
                   (fc-proj-root dir)
                 (oref *fc-project* dir)))
         (name (if dir
                   (fc-proj-name dir)
                 (fc-proj--get *fc-project* :name)))
         (file (fc--find-file root
                              (format "Project [%s] files" name)
                              '(conf code doc xml) :sort t)))
    (when file
      (find-file (format "%s/%s" root file)))))

(cl-defun fc-proj-query-replace-with-dired ()
  (let ((files (read-string "Pattern : " "*.[hc]")))
    (find-dired (fc-proj-root) (concat "-name \"" files "\""))

    (while (get-buffer-process (get-buffer "*Find*"))
      (sit-for 1))

    (dired-toggle-marks)
    (fc-funcall 'dired-do-find-regexp-and-replace)))

(cl-defun fc-proj-query-replace-with-ggtags ()
  (let* ((name (fc-proj-name))
         (from-prompt (format "Replace in {%s} from: " name))
         (to-prompt (format "Replace in {%s} to: " name)))
    (ggtags-query-replace
     (read-string from-prompt (fc-current-thing :ask nil))
     (read-string to-prompt))))

(cl-defun fc-proj-query-rename ()
  (interactive)

  (when (fc--lsp-active-p)
    (fc--lsp-rename)
    (cl-return-from fc-proj-query-rename))

  (fc-funcall (pcase fc-proj-tag
                ('global #'fc-proj-query-replace-with-ggtags)
                (_ #'fc-proj-query-replace-with-dired))))

(defvar *fc-project-hook* nil)
(defvar *fc-project-name* "Nul")
(defvar *fc-project* nil)
(defvar *fc-projects* nil)

(run-hooks '*fc-project-hook*)

(defun fc-proj--compilation-done (_ _)
  (fc-proj-load-compilation-error))

(add-to-list 'compilation-finish-functions #'fc-proj--compilation-done)

(fc-load 'project
  :local t
  :after (progn
           (defun fc-proj--proj-root (dir)
             (list 'vc 'Git (fc-proj-root dir)))

           (setf project-find-functions
                 (list #'fc-proj--proj-root))))

(provide 'fc-proj)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-proj.el ends here
