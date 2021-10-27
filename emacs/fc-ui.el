;;; fc-ui.el --- UI -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

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
  (if (listp (cl-first collection))
      (-map 'car collection)
    collection))

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
         (fc--get-result collection
                         (fc--user-select prompt
                                          (fc--gen-names collection)
                                          :fullscreen fullscreen
                                          :mouse mouse)))))

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

(provide 'fc-ui)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-ui.el ends here
