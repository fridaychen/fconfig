;;; fc-ui.el --- UI -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; UI selector
(cl-defun fc-select (prompt collection &key always conv (init ""))
  "Select a item from the collection.
PROMPT: user prompt.
COLLECTION: cadidates collection.
ALWAYS: always ask use to select.
CONV: convert items of collection into strings."
  (declare (indent 2))
  (when (and (not always)
             (= (if (hash-table-p collection)
                    (hash-table-count collection)
                  (length collection))
                1))
    (cl-return-from fc-select (fc-first collection t)))

  (when-let* ((candidates (if conv
                              (mapcar (lambda (x) (cons (funcall conv x) x))
                                      collection)
                            collection))
              (sample (fc-first candidates t))
              (input (completing-read prompt candidates nil nil init)))
    (cond ((listp sample)
           (alist-get input candidates nil nil #'string-equal))

          ((symbolp sample)
           (intern input))

          (t input))))

(cl-defun fc-select-func (prompt collection)
  "Select a function to run from collection.
PROMPT: user prompt.
COLLECTION: cadidates collection."
  (when-let* ((func (fc-select prompt collection :always t)))
    (fc-funcall func)))

(cl-defun fc-select-color (prompt &key always (colors (defined-colors)))
  (when-let* ((color (fc-select prompt colors
                       :always t
                       :conv (lambda (x)
                               (concat
                                x
                                " "
                                (fc-text x :face `(:foreground "black" :background ,x))
                                " "
                                (fc-text x :face `(:foreground ,x :background "black"))))))
              (defined (color-defined-p color)))
    color))

(cl-defun fc-select-buffer (prompt
                            pred
                            &key relative pop (error-msg "Buffer list is empty !!!") one)
  "Select a BUFFER to switch.
PROMPT: prompt string.
PRED: arguments for list-buffer.
RELATIVE: root directory for showing file path.
POP: show the selected buffer side-by-side.
ONE: only request one buffer.
ERROR-MSG: error message."
  (when-let* ((bufs (fc--list-buffer pred :one one))
              (buf (fc-select prompt bufs
                     :conv (lambda (x)
                             (if relative
                                 (file-relative-name (buffer-file-name x)
                                                     relative)
                               (buffer-name x))))))
    (if pop
        (fc-pop-buf buf :select t)
      (switch-to-buffer buf))))

;; UI yes-or-no
(cl-defun fc-user-confirm (prompt &optional (default-ans t))
  "Ask user y or n question.
PROMPT: user prompt.
DEFAULT-ANS: default answer."
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

;; UI menu
;; popup-menu
(cl-defun fc-create-simple-pop-menu (title items)
  "Create simple pop menu.
TITLE: menu title.
ITEMS: menu items."
  `(,title
    ,(cons "PANE"
           items)))

(cl-defun fc-create-pop-menu (title items)
  "Create pop menu.
TITLE: menu title.
ITEMS: menu items."
  `(keymap
    ,title
    ,@(--reduce-r-from
       (progn
         (cons (append
                (list
                 (cl-first it)
                 'menu-item
                 (cl-second it)
                 t)
                (last it 2))
               acc))
       nil items)))

(cl-defun fc-show-pop-menu (menu)
  "Popup menu.
MENU: menu."
  (x-popup-menu t menu))

(cl-defun fc-eval-pop-menu (menu)
  "Run popup menu.
MENU: menu."
  (fc-funcall (fc-show-pop-menu menu)))

;; popup message

(require 'notifications)

(defun fc--popup-posframe (name text &rest rest)
  (seq-let (bg fg) (if (fc-dark-theme-p)
                       '("RoyalBlue4" "PeachPuff")
                     '("RoyalBlue3" "LightYellow"))
    (apply #'posframe-show name
           :string text
           :background-color bg
           :foreground-color fg
           :border-width 4
           :border-color "navy"
           rest)))

(defun fc--popup-tip (text)
  (let ((lines (seq-count (lambda (x) (eq x ?\n)) text)))
    (unless (string-suffix-p "\n" text)
      (cl-incf lines))

    (popup-tip text :height lines)))

(defun fc-popup-tip (content)
  "Popup info application level.
CONTENT: buffer content.
TIMEOUT: buffer show timeout in seconds."
  (if (and (<= 26 emacs-major-version)
           *is-gui*)
      (fc--popup-posframe "*fc-tip*" content
                          :poshandler #'posframe-poshandler-point-bottom-left-corner
                          :max-width 60)
    (fc--popup-tip content)))

(defun fc-popup-tip-hide ()
  (when *is-gui*
    (posframe-hide "*fc-tip*")))

(defun fc--popup-info-local (title content timeout)
  "Popup info application level.
CONTENT: buffer content.
TITLE: buffer title.
TIMEOUT: buffer show timeout in seconds."
  (if (and (<= 26 emacs-major-version)
           *is-gui*)
      (let ((frame (fc--popup-posframe "*fc-info*"
                                       (concat "[" title "]\n\n" content)
                                       :poshandler #'posframe-poshandler-frame-center)))
        (if (eq 0 timeout)
            (read-event)
          (sit-for timeout))
        (posframe-hide "*fc-info*"))
    (fc--popup-tip content)))

(cl-defun fc-popup-info (content &key (title "*fc-info*") (timeout 0) os)
  "Popup information.
CONTENT: buffer content.
TITLE: buffer title.
TIMEOUT: buffer show timeout in seconds.
OS: os level or app level."
  (if os
      (notifications-notify :title title :body content :urgency "critical" :sound-name )
    (fc--popup-info-local title content timeout)))

(defun fc--popup-hide-info ()
  "Hide popup tip buffer."
  (posframe-hide "*fc-info*"))

(add-hook '*fc-ergo-restore-hook* #'fc--popup-hide-info)

;; Overlay
(cl-defun fc-make-overlay (beginning end &rest properties)
  (let ((x (make-overlay beginning end nil t t)))
    (fc-each properties
      (apply #'overlay-put x it))
    x))

(provide 'fc-ui)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-ui.el ends here
