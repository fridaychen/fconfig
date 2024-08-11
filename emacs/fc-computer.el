;;; fc-computers.el --- auto setup by hostname-*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(declare-function fc-manual "fc-util.el")

(defvar *fc-location* nil)

(defconst *fc-cloud-home* "~/Google Drive/Emacs/")
(defconst *fc-computer-path* (fc-home-path "fconfig/computers/"))
(defconst *fc-chinese-fonts* '("Microsoft Yahei"))
(defconst *fc-japanese-fonts* '("Microsoft Yahei"))
(defconst *fc-english-fonts* '("Monaco"))

(defconst *fc-assist-app*
  (cond
   (*is-mac* (fc-home-path "fconfig/mac/assist.sh"))
   (*is-linux* (fc-home-path "fconfig/linux/assist.sh"))
   (*is-cygwin* (fc-home-path "fconfig/win/assist.sh"))))

(cl-defun fc--eth-addr-eql (addr1 addr2)
  (equal
   (fc-map (string-split addr1 ":") (string-to-number it 16))
   (fc-map (string-split addr2 ":") (string-to-number it 16))))

(cl-defun fc-location-p (sym)
  (cond
   ((listp *fc-location*)
    (member sym *fc-location*))

   ((symbolp *fc-location*)
    (eq sym *fc-location*))))

(cl-defun fc-update-location ()
  (defconst *fc-gateway-mac*
    (fc-exec-command-to-string *fc-assist-app* "--gateway"))

  (defvar *fc-location-gateway* nil)

  (setf *fc-location*
        (car (fc-first *fc-location-gateway*
               (fc--eth-addr-eql *fc-gateway-mac* (cdr it))))))

(defun fc-do-auto-config ()
  "Auto config implementation."
  (when *is-gui*
    (let* ((find-font (lambda (font-list)
                        (fc-first font-list (fc-font-exists-p it))))
           (cjk-font (funcall find-font *fc-cjk-fonts*))
           (symbol-font (funcall find-font *fc-symbol-fonts*))
           (english-font (funcall find-font *fc-english-fonts*))
           (cjk-reading-font (funcall find-font *fc-cjk-reading-fonts*))
           (english-reading-font (funcall find-font *fc-english-reading-fonts*)))
      (message "Fonts :\tcjk -> %s\n\tenglish -> %s\n\tsymbol -> %s\n\teng-reading -> %s\n\tcjk-reading -> %s"
               cjk-font english-font symbol-font
               english-reading-font cjk-reading-font)
      (setf *fc-default-font* english-font
            *fc-font* (list
                       (cons '(kana han cjk-misc bopomofo) cjk-font)
                       (cons '(symbol) symbol-font))
            *fc-mode-line-font* (funcall find-font *fc-mode-line-fonts*))

      (make-face 'fc-viewer-face)
      (setf *fc-reading-face* 'fc-viewer-face
            *fc-reading-fontset* (fc-create-fontset
                                  "reading"
                                  (elt english-reading-font 1)))
      (apply #'set-face-attribute 'fc-viewer-face nil
             :fontset *fc-reading-fontset*
             english-reading-font)

      (fc-setup-font-spec
       *fc-reading-fontset*
       `(((kana han cjk-misc bopomofo) ,@cjk-reading-font)))))

  (fc-idle-delay
    (fc-add-network-connected-hook #'fc-update-location)
    (fc-update-location)))

(defun fc-auto-config ()
  "Facade function."
  (interactive)

  (fc-require 'fc-common)

  (let ((filename (concat *fc-computer-path* (system-name) ".el")))
    (if (file-exists-p filename)
        (load-file filename)))

  (when (fboundp 'fc-user-config)
    (fc-user-config))

  (fc-do-auto-config))

(defun fc-assist-cmd (&rest args)
  "Execute assist command.
ARGS: assist command arguments."
  (apply #'fc-exec-command (cons *fc-assist-app* args)))

(defconst *fc-increase-display-brightness*
  (fc-manual (fc-assist-cmd "--brighten")))
(defconst *fc-decrease-display-brightness*
  (fc-manual (fc-assist-cmd "--dim")))
(defconst *fc-increase-volume*
  (fc-manual (fc-assist-cmd "--volup")))
(defconst *fc-decrease-volume*
  (fc-manual (fc-assist-cmd "--voldown")))
(defconst *fc-mute-volume*
  (fc-manual (fc-assist-cmd "--mute")))

(provide 'fc-computer)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-computer.el ends here
