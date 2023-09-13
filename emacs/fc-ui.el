;;; fc-ui.el --- UI -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; UI selector
(defun fc--list-string-width (collection)
  (cl-loop for x in collection sum (string-width x)))

(cl-defun fc--user-select (prompt collection &key fullscreen mouse)
  "Select a item from the collection.
PROMPT: user prompt.
COLLECTION: cadidates collection.
FULLSCREEN: fullscreen ui mode.
MOUSE: allow user to select with mouse."
  (defvar helm-full-frame)

  (cond
   (mouse
    (ivy-read (fc-prompt prompt) collection))

   ((and fullscreen (fboundp 'helm))
    (let ((helm-full-frame t))
      (helm :sources
            (helm-build-sync-source prompt
                                    :candidates collection))))

   ((and (> 9 (length collection))
         (> (- (frame-width) 20 (length prompt))
            (fc--list-string-width collection)))
    (ido-completing-read (fc-prompt prompt) collection))

   (t
    (ivy-read (fc-prompt prompt) collection))))

(defun fc--gen-names (collection)
  (cond ((listp (cl-first collection))
         (fc--gen-names (-map 'car collection)))

        ((stringp (cl-first collection))
         collection)

        (t
         (-map 'fc-string collection))))

(defun fc--gen-collection (collection)
  (cond ((or (listp (cl-first collection))
             (stringp (cl-first collection)))

         collection)

        (t
         (--map (cons (fc-string it) it) collection))))

(defun fc--get-result (collection name)
  (if (listp (cl-first collection))
      (cdr (--first (equal (car it) name)
                    collection))
    name))

(cl-defun fc-user-select (prompt collection &key always fullscreen mouse)
  "Select a item from the collection.
PROMPT: user prompt.
COLLECTION: cadidates collection.
ALWAYS: always ask use to select.
FULLSCREEN: fullscreen ui mode.
MOUSE: allow user to select with mouse."
  (cond ((not collection))

        ((and (not always)
              (= (length collection) 1))
         (fc--get-result collection
                         (cl-first (fc--gen-names collection))))

        (t
         (fc--get-result (fc--gen-collection collection)
                         (fc--user-select prompt
                                          (fc--gen-names collection)
                                          :fullscreen fullscreen
                                          :mouse mouse)))))

(cl-defun fc-user-select-func (prompt collection &key fullscreen default mouse)
  "Select a function to run from collection.
PROMPT: user prompt.
COLLECTION: cadidates collection.
FULLSCREEN: fullscreen ui mode.
DEFAULT: default function."
  (fc-funcall (fc-user-select prompt
                              collection
                              :always t
                              :fullscreen fullscreen
                              :mouse mouse)
              :default default))

(cl-defun fc-user-select-color (prompt colors)
  (let ((color (fc-user-select
                prompt
                (cl-loop for x in colors
                         collect
                         (cons (fc-text x
                                        :face (list :foreground "black"
                                                    :background x))
                               x)))))
    (when (color-defined-p color)
      color)))

;; UI yes-or-no
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
  (let ((lines (s-count-matches "\n" text)))
    (unless (s-suffix? "\n" text)
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

(frame-pixel-width)

(provide 'fc-ui)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-ui.el ends here
