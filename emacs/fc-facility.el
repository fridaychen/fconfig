;;; fc-facility.el --- setup editor -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; fc-idle-hook
(require 'cl-lib)
(defvar *fc-enable-dbus* (and *is-linux* (executable-find "dbus-daemon")))

(defvar *fc-idle-delay* 1200)

;; idle hook
(let ((fc-idle-hook nil))
  (defun fc-add-idle-hook (func)
    "Add idle function to hook.
FUNC: new function."
    (add-hook 'fc-idle-hook func))

  (defun fc-del-idle-hook (func)
    "Delete idle function from hook.
FUNC: the one to be deleted."
    (remove-hook 'fc-idle-hook func))

  (run-with-idle-timer *fc-idle-delay*
                       t
                       (lambda () (run-hooks 'fc-idle-hook))))

;; power sleep hook
(let ((fc-power-sleep-hook nil))
  (defun fc-add-sleep-hook (func)
    "Add sleep function to hook.
FUNC: new function."
    (add-hook 'fc-power-sleep-hook func))

  (defun fc-del-sleep-hook (func)
    "Delete sleep function from hook.
FUNC: the one to be deleted."
    (remove-hook 'fc-power-sleep-hook func))

  (defun fc-sleep-signal-handler ()
    (run-hooks '*fc-power-sleep-hook*)))

;; power resume hook
(let ((fc-power-resume-hook nil))
  (defun fc-add-resume-hook (func)
    "Add resume function to hook.
FUNC: new function."
    (add-hook '*fc-power-resume-hook* func))

  (defun fc-del-resume-hook (func)
    "Delete resume function from hook.
FUNC: the one to be deleted."
    (remove-hook '*fc-power-resume-hook* func))

  (defun fc-resume-signal-handler ()
    (run-hooks 'fc-power-resume-hook)))

(let ((fc-network-connected-hook nil))
  (defun fc-add-network-connected-hook (func)
    "Add network connected function to hook.
FUNC: new function."
    (add-hook 'fc-network-connected-hook func))

  (defun fc-del-network-connected-hook (func)
    "Delete network connected function from hook.
FUNC: the one to be deleted."
    (remove-hook 'fc-network-connected-hook func))

  (defun fc-network-connected--handler ()
    (run-hooks 'fc-network-connected-hook)))

(cl-defmacro fc-add-hook (hook &rest rest)
  (declare (indent 1))
  `(add-hook ,hook (lambda () ,@rest)))

(cl-defun fc-add-hook-func (hook &rest rest)
  (declare (indent 1))
  (--each rest
    (when it
      (add-hook hook it))))

(cl-defun fc-run-hook (hook &optional timeout)
  "Run hook.
HOOK: hook var
TIMEOUT: do it later."
  (if timeout
      (fc-delay-task
       (lambda ()
         (run-hooks hook))
       timeout)
    (run-hooks hook)))

(cl-defun fc-idle-delay-task (func &optional (timeout 0.05))
  (run-with-idle-timer timeout nil func))

(cl-defun fc-delay-task (func &optional (timeout 1))
  (run-with-timer timeout nil func))

(cl-defmacro fc-idle-delay (&rest rest)
  "Run at idle.
REST: operations."
  (declare (indent defun))
  `(fc-idle-delay-task (lambda () ,@rest)))

(cl-defmacro fc-delay (&rest rest)
  (declare (indent defun))
  "Run later.
REST: operations."
  `(fc-delay-task (lambda () ,@rest)))

(if (<= 26 emacs-major-version)
    (cl-defmacro fc-run (&rest rest)
      (declare (indent defun))
      `(make-thread (lambda () ,@rest)))
  (defalias 'fc-run 'progn))

(when *fc-enable-dbus*
  (require 'dbus)

  ;; class dbus
  (defclass fc-dbus-intf ()
    ((path :initform ""
           :type string)
     (intf :initform ""
           :type string)
     (prop-intf :initform ""
                :type string)
     (service :initform ""
              :type string)))

  (cl-defmethod fc-dbus--call ((x fc-dbus-intf) method)
    (dbus-call-method :session (oref x service) (oref x path) (oref x intf) method))

  (cl-defmethod fc-dbus--get ((x fc-dbus-intf) prop)
    (dbus-get-property :session (oref x service) (oref x path) (oref x intf) prop))

  (cl-defmethod fc-dbus--set ((x fc-dbus-intf) prop value)
    (dbus-set-property :session (oref x service) (oref x path) (oref x intf) prop value))

  (cl-defmethod fc-dbus--register-signal ((x fc-dbus-intf) prop func)
    (dbus-register-signal :session (oref x service) (oref x path) (oref x prop-intf) prop func))

  ;; suspend/resume singal
  (dbus-register-signal :system "org.freedesktop.login1" "/org/freedesktop/login1"
                        "org.freedesktop.login1.Manager" "PrepareForSleep"
                        (lambda (sleep)
                          (if sleep
                              (fc-sleep-signal-handler)
                            (fc-resume-signal-handler))))

  (defun fc-network-connected-p ()
    (equal 70 (dbus-get-property
               :system "org.freedesktop.NetworkManager" "/org/freedesktop/NetworkManager"
               "org.freedesktop.NetworkManager" "State")))

  (defun fc-network-state-changed (state)
    (pcase state
      ;; unknown state
      (0)
      ;; state alseep
      (10)
      ;; state disconnected
      (20)
      ;; state disconnecting
      (30)
      ;; state connecting
      (40)
      ;; state connected local
      (50)
      ;; state connected site
      (60)
      ;; state connected global
      (70 (fc-network-connected--handler))))

  (dbus-register-signal :system "org.freedesktop.NetworkManager" "/org/freedesktop/NetworkManager"
                        "org.freedesktop.NetworkManager" "StateChanged"
                        #'fc-network-state-changed))

(unless (fboundp #'fc-network-connected-p)
  (defun fc-network-connected-p ()
    (fc-not-void-p (fc-exec-command-to-string *fc-assist-app* "--gateway"))))

(defun fc-network-advice (orig-fun &rest args)
  "Setup network advice.
ORIG-FUN: original function.
ARGS: original arguments."
  (interactive)

  (if (fc-network-connected-p)
      (apply orig-fun args)
    (message "No network connection !")))

(defun fc-add-network-advice (&rest rest)
  "Add network advice to functions."
  (--each rest
    (advice-add it :around 'fc-network-advice)))

;; redisplay hook
(let ((fc-display-hook nil))
  (defun fc-add-display-hook (func)
    "Add display function to hook.
FUNC: new function."
    (add-hook 'fc-display-hook func))

  (cl-flet ((f (&optional ignored)
              (run-hooks 'fc-display-hook)))
    (add-function :after pre-redisplay-function #'f)))

(defun fc-home-path (path)
  (expand-file-name
   (concat user-emacs-directory path)))

;; dwell delay
(defvar *fc-dwell-timeout* 0.6)
(defvar *fc-dwell-timer* nil)
(defvar *fc-dwell-func* nil)

(make-local-variable '*fc-dwell-func*)

(defun fc--dwell-func ()
  (when *fc-dwell-timer*
    (cancel-timer *fc-dwell-timer*)
    (setq *fc-dwell-timer* nil))

  (when *fc-dwell-func*
    (setf *fc-dwell-timer*
          (fc-idle-delay-task *fc-dwell-func* *fc-dwell-timeout*))))

(defun fc-dwell-enable (func)
  (setq-local *fc-dwell-func* func)
  (add-hook 'post-command-hook #'fc--dwell-func nil t))

(defun fc-dwell-disable ()
  (setq-local *fc-dwell-func* nil)
  (remove-hook 'post-command-hook #'fc--dwell-func t))

(cl-defmacro fc-with-dir (dir &rest rest)
  (declare (indent 1))
  `(when-let ((default-directory ,dir))
     (progn ,@rest)))

(cl-defmacro fc-with-buffer (buffer-or-name &rest rest)
  (declare (indent 1))
  `(when-let ((buf (get-buffer-create ,buffer-or-name)))
     (with-current-buffer buf
       (progn ,@rest))))

(cl-defmacro fc-with-existing-buffer (buffer-or-name &rest rest)
  (declare (indent 1))
  `(when-let ((buf (get-buffer ,buffer-or-name)))
     (with-current-buffer buf
       (progn ,@rest))))

(cl-defmacro fc-with-each-buffer (&rest rest &key (buffers '(buffer-list)) &allow-other-keys)
  `(--each ,buffers
     (with-current-buffer it
       ,@rest)))

(cl-defun fc-run-command-chain (chain &rest args)
  (cl-loop for x in chain
           do
           (let ((r (apply x args)))
             (when r
               (cl-return r)))))

(defun fc-main-thread-p ()
  "Return t if current thread is main thread."
  (eq (current-thread) main-thread))

(cl-defmacro fc-safe-wrap (fn &rest clean-up)
  `(unwind-protect
       (let (retval)
         (condition-case ex
             (setq retval (progn ,fn))
           ('error
            (message (format "Caught exception: [%s]" ex))
            (setq retval (cons 'exception (list ex)))))
         retval)
     ,@clean-up))

(cl-defmacro fc-each (obj &rest body)
  (declare (indent 1))
  `(cond
    ((and (not (stringp ,obj)) (seqp ,obj))
     (seq-do (lambda (it)
               ,@body)
             ,obj))

    (t
     (let ((it ,obj))
       (progn ,@body)))))

(defun fc-mapcar (func collection)
  (if func
      (mapcar func collection)
    collection))

(provide 'fc-facility)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-facility.el ends here
