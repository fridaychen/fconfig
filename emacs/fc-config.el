;;; fc-config.el --- save information center -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(declare-function fc-add-idle-hook "fc-facility")
(declare-function fc-unserialize "fc-util")
(declare-function fc-serialize "fc-util")

(cl-defstruct fc-conf
  (path nil)
  (data nil)
  (changed nil))

(cl-defun fc-conf-new (path)
  (make-fc-conf :path path :data '(("ver" 1))))

(cl-defun fc-conf-open (path &optional (auto-create t))
  (unless (file-exists-p path)
    (if auto-create
        (fc-serialize path nil)
      (cl-return-from fc-conf-open nil)))

  (let ((conf (fc-conf-new path)))
    (setf (fc-conf-data conf)
          (fc-unserialize path))
    conf))

(cl-defun fc-conf-get (conf &rest keys)
  "Get config.
CONF: config data.
KEY: config key."
  (apply #'atree-get (fc-conf-data conf) keys))

(cl-defun fc-conf-put (conf value &rest keys)
  "Put config.
CONF: config data.
KEY: config key.
VALUE: config value."
  (apply #'atree-set (fc-conf-data conf) value keys)
  (setf (fc-conf-changed conf) t))

(defun fc-conf-save (conf)
  "Save config.
CONF: config data."
  (when (fc-conf-changed conf)
    (setf (fc-conf-changed conf) nil)
    (fc-serialize (fc-conf-path conf)
                  (fc-conf-data conf))
    t))

(provide 'fc-config)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-config.el ends here
