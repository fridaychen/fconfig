;;; fc-package.el --- setup emacs package managerment -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(defconst *fc-basic-packages* '(s dash xr))

(cl-defun fc-require (name &optional run)
  (unless (featurep name)
    (if run
        (fc-run
          (load (fc-home-path (concat "fconfig/" (symbol-name name))) t t))
      (load (fc-home-path (concat "fconfig/" (symbol-name name))) t t))))

(cl-defun fc-install (&rest rest)
  (mapcar
   (lambda (it)
     (and it
          (not (package-installed-p it))
          (package-install it)))
   rest))

(cl-defmacro fc-load (name &rest rest)
  (declare (indent 1))
  "Load package and run setup.
NAME: package name.
REST: include :package :local :enable :bind :before :after :autoload :run"
  (let ((n (make-symbol "n"))
        (l (make-symbol "l"))
        (pkg (make-symbol "pkg"))
        (local (make-symbol "local"))
        (enable (make-symbol "enable"))
        (autoload (make-symbol "autoload"))
        (raw (make-symbol "raw"))
        (run (make-symbol "run"))
        (idle (make-symbol "idle"))
        (bind (make-symbol "bind"))
        (before (make-symbol "before"))
        (after (make-symbol "after")))
    `(let* ((,n (if (stringp ,name) (intern ,name) ,name))
            (,l ',rest)
            (,pkg (plist-get ,l :package))
            (,local (plist-get ,l :local))
            (,enable (plist-get ,l :enable))
            (,autoload (plist-get ,l :autoload))
            (,raw (plist-get ,l :raw))
            (,run (plist-get ,l :run))
            (,idle (plist-get ,l :idle))
            (,bind (plist-get ,l :bind))
            (,before (plist-get ,l :before))
            (,after (plist-get ,l :after)))
       (when (or (null ,enable)
                 (and ,enable (eval ,enable t)))
         (when ,before
           (eval ,before t))

         (when ,raw
           (unless (file-exists-p (format "%s/site/%s"
                                          user-emacs-directory
                                          ,n))
             (shell-command (format "cd %s/site; git clone %s"
                                    user-emacs-directory
                                    ,raw)))

           (add-to-list 'load-path (format "%s/site/%s"
                                           (expand-file-name user-emacs-directory)
                                           ,n)))

         (cond
          ;; load locally
          ((and ,local (eval ,local t))
           (require ,n nil t))

          ;; load package
          (t
           (when (and (not ,raw)
                      (not (package-installed-p (or ,pkg ,n))))
             (package-install (if (null ,pkg)
                                  ,n
                                (eval ,pkg t))))

           (cond
            ((featurep ,n))
            (,autoload)
            (,run
             (fc-run
               (require ,n nil t)))
            (,idle
             (fc-idle-delay
               (require ,n nil t)))
            (t
             (require ,n nil t)))))

         (with-eval-after-load ,name
           (when ,after
             (eval ,after t))

           (when ,bind
             (fc-each (eval ,bind t)
               (fc-bind-keys (cl-rest it)
                             (symbol-value (car it))))))))))

(fc-load 'package
  :local t
  :after (progn
           (add-to-list 'package-archives
                        '("melpa" . "http://melpa.org/packages/"))

           (setf package-enable-at-startup nil)
           (package-initialize)

           (unless package-archive-contents
             (message "@@@ refresh package")
             (package-refresh-contents))

           (dolist (pkg *fc-basic-packages*)
             (fc-install pkg)
             (require pkg))))

(provide 'fc-package)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-package.el ends here
