;;; fc-book.el --- handling book -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)

(fc-install 'hide-mode-line)

(defconst *fc-book-chinese-table*
  '(("　" " ") ("．" ".")
    ("０" "0") ("１" "1") ("２" "2") ("３" "3") ("４" "4")
    ("５" "5") ("６" "6") ("７" "7") ("８" "8") ("９" "9")

    ("ａ" "a") ("ｂ" "b") ("ｃ" "c") ("ｄ" "d") ("ｅ" "e")
    ("ｆ" "f") ("ｇ" "g") ("ｈ" "h") ("ｉ" "i") ("ｊ" "j")
    ("ｋ" "k") ("ｌ" "l") ("ｍ" "m") ("ｎ" "n") ("ｏ" "o")
    ("ｐ" "p") ("ｑ" "q") ("ｒ" "r") ("ｓ" "s") ("ｔ" "t")
    ("ｕ" "u") ("ｖ" "v") ("ｗ" "w") ("ｘ" "x") ("ｙ" "y")
    ("ｚ" "z")

    ("Ａ" "A") ("Ｂ" "B") ("Ｃ" "C") ("Ｄ" "D") ("Ｅ" "E")
    ("Ｆ" "F") ("Ｇ" "G") ("Ｈ" "H") ("Ｉ" "I") ("Ｊ" "J")
    ("Ｋ" "K") ("Ｌ" "L") ("Ｍ" "M") ("Ｎ" "N") ("Ｏ" "O")
    ("Ｐ" "P") ("Ｑ" "Q") ("Ｒ" "R") ("Ｓ" "S") ("Ｔ" "T")
    ("Ｕ" "U") ("Ｖ" "V") ("Ｗ" "W") ("Ｘ" "X") ("Ｙ" "Y")
    ("Ｚ" "Z")))

(defconst *fc-book-check-regex*
  '("[.。,，:：、]\\{2,3\\}"
    "^ *”"
    "“$"
    "“[^”]+“"
    "”[^“]+”"
    "^ *[,，.。”’]"
    "^[^#\n][^\n]+[^？。！…～”’）〗】}：※》a-z—；*—]\n$"
    "“[^”]*\n+[^”]*”"
    "^[^\\#\n].*[^”。！？：…}）〗】※*—～]\n$"))

(defun fc-book-replace (pairs)
  "Batch strings replacing.
PAIRS: replacement list."
  (interactive)

  (--each pairs
    (fc-replace-string (cl-first it)
                       (cl-second it)
                       :from-start t)))

(defun fc-book-init ()
  "Init buffer for book editing."
  (interactive)

  (save-excursion
    (goto-char (point-min))
    (save-excursion (dos2unix))
    (save-excursion (fc-book-replace *fc-book-chinese-table*))
    (save-excursion (fc-remove-empty-line))
    (save-excursion (whitespace-cleanup))))

(defun fc--toc-replace-regexp (regex to-string)
  "Rexexp replacing in TOC.
REGEX: target regexp.
TO-STRING: new string."
  (fc-replace-regexp regex
                     #'(lambda ()
                         (when (or
                                (and (fc-not-void-p (match-string 1))
                                     (fc-not-void-p (match-string 3)))
                                (and (fc-void-p (match-string 1))
                                     (fc-void-p (match-string 3))))
                           (replace-match to-string)))))

(cl-defun fc-fix-zh-single-qoute ()
  (interactive)

  (save-excursion
    (fc-replace-regexp
     "‘[^“”‘’]+’"
     #'(lambda ()
         (let* ((start (match-beginning 0))
                (end (match-end 0))
                (left-bound (max (- (line-beginning-position) 1024) 0))
                (left-quote (save-excursion (search-backward "“" left-bound t)))
                (right-quote (save-excursion (search-backward "”" left-bound t))))
           (goto-char start)
           (when (or (and (null left-quote) (null right-quote))
                     (and (null left-quote) right-quote)
                     (and left-quote right-quote (< left-quote right-quote)))
             (fc-replace-text start 1 "“")
             (fc-replace-text end -1 "”"))
           (goto-char end)))
     :from-start t)))

(defun fc-remove-extra-return ()
  "Remove extra return."
  (interactive)

  (save-excursion
    (fc-replace-regexp "\\([^[:space:]]\\)\n\\([^\\[:space:]]\\)"
                       "\\1\\2"
                       :from-start t)))

(defun fc-duplicate-return ()
  "Double return."
  (interactive)

  (save-excursion
    (fc-replace-string "\n" "\n\n" :from-start t)))

(defun fc-remove-extra-whitespace ()
  "Remove extra whitespace."
  (interactive)

  (save-excursion
    (fc-replace-regexp "\\([^a-zA-Z,.\\\\]\\) +"
                       "\\1"
                       :from-start t)
    (fc-replace-regexp "\\([a-zA-Z]\\) +\\([^a-zA-Z]\\)"
                       "\\1\\2"
                       :from-start t)))

(let ((fbook-tmp-test-regex nil)
      (fbook-check-finished t)
      (fc-last-overlay nil))

  (defun fc-check-regex ()
    (if (null fbook-tmp-test-regex)
        (progn
          (message "Check book OK, no more error")
          (setf fbook-check-finished t))

      (if fc-last-overlay
          (delete-overlay fc-last-overlay))

      (if (re-search-forward (car fbook-tmp-test-regex) (- (point-max) 2) t)
          (progn
            (setf fc-last-overlay (make-overlay (match-beginning 0) (point)))
            (overlay-put fc-last-overlay 'face 'underline)
            (when *fc-dev-mode*
              (message (car fbook-tmp-test-regex))))

        (goto-char (point-min))
        (setf fbook-tmp-test-regex (cdr fbook-tmp-test-regex))
        (fc-check-regex))))

  (defun fc-check-book ()
    "continue checking book for errors"
    (interactive)

    (unless fbook-check-finished
      (fc-check-regex)))

  (defun fc-recheck-book ()
    "check book for errors again"
    (interactive)

    (setf fbook-tmp-test-regex *fc-book-check-regex*
          fbook-check-finished nil)

    (fc-ergo-repeat-func 'fc-check-book)
    (fc-check-book)))

(defun fc-book-format ()
  "Format BOOK."
  (interactive)

  (save-excursion
    (fc-remove-empty-line)

    (fc-replace-regexp "^ +"
                       "
"
                       :from-start t)

    (fc-replace-regexp "\\([^\n]\\)\n\\([^\n]\\)"
                       "\\1\\2"
                       :from-start t)))

(defvar *fc-reading-scale* 3)
(defvar *fc-reading-mode* nil)
(defvar *fc-reading-fill* 50)
(defvar *fc-reading-margin* 0)
(defvar *fc-reading-window-width* 0)

(make-local-variable '*fc-reading-mode*)
(make-local-variable '*fc-reading-margin*)
(make-local-variable '*fc-reading-window-width*)

(defun fc-reading-config ()
  "Setup reading mode."
  (when (and *fc-reading-mode*
             (not (eql *fc-reading-window-width* (window-width))))
    (if (> (+ (window-width) *fc-reading-margin*) (+ *fc-reading-fill* 6))
        (let ((margin (/ (- (window-width) *fc-reading-fill*) 2)))
          (set-window-fringes (selected-window) *fc-fringe-width* 0 t)
          (set-window-margins (selected-window) margin margin)

          (setf *fc-reading-margin* margin)
          (setf *fc-reading-window-width* (window-width)))

      (set-window-fringes (selected-window) *fc-fringe-width* 0 nil)
      (set-window-margins (selected-window) 0 0))))

(defun fc-reading-enter ()
  "Enter reading mode."
  (interactive)

  (unless (facep 'fc-read-face)
    (add-hook 'window-configuration-change-hook
              #'fc-reading-config)

    (when *is-gui*
      (make-face 'fc-read-face)
      (set-face-attribute 'fc-read-face nil
                          :family *fc-read-font*)
      (set-face-attribute 'fc-read-face nil :fontset "fontset-read")))

  (fc-reading-config)
  (hl-line-mode 1)
  (hide-mode-line-mode 1)

  (unless buffer-display-table
    (setq-local buffer-display-table #^[nil nil display-table nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil 92 nil nil nil nil]))

  (set-display-table-slot buffer-display-table 'wrap ?\x20)

  (when *is-gui*
    (buffer-face-set 'fc-read-face)
    (text-scale-set *fc-reading-scale*)))

(defun fc-reading-quit ()
  "Quit reading mode."
  (interactive)

  (setf *fc-reading-margin* 0)
  (setf *fc-reading-window-width* 0)

  (hl-line-mode -1)
  (hide-mode-line-mode -1)

  (set-window-fringes (selected-window) *fc-fringe-width* 0 nil)
  (set-window-margins (selected-window) 0 0)
  (set-display-table-slot buffer-display-table 'wrap ?\\)
  (buffer-face-mode -1)
  (text-scale-set 0))

(defun fc-reading-toggle ()
  "Toggle reading mode."
  (interactive)

  (if *fc-reading-mode*
      (progn
        (setq-local *fc-reading-mode* nil)
        (fc-reading-quit))
    (setq-local *fc-reading-mode* t)
    (fc-reading-enter)))

(cl-defun fc-add-book-local-var ()
  (save-excursion
    (add-file-local-variable
     'eval
     '(visual-line-mode 1))

    (add-file-local-variable
     'eval
     '(text-scale-set *fc-reading-scale*))))

(cl-defun fc-merge-short-line ()
  (interactive)

  (save-excursion
    (fc-replace-regexp "\\([^\n]\\)\n\\([^\n]\\)"
                       #'(lambda ()
                           (replace-match
                            (let ((a (match-string 1))
                                  (b (match-string 2)))
                              (if (and (< (elt a 0) 128)
                                       (< (elt b 0) 128))
                                  "\\1 \\2"
                                "\\1\\2")))))))

(cl-defun fc-merge-all-line ()
  (interactive)

  (save-excursion
    (fc-replace-regexp "\n\n+" "\n"))

  (save-excursion
    (fc-replace-regexp "\\([^\n]\\)\n\\([^\n]\\)"
                       #'(lambda ()
                           (replace-match
                            (let ((a (match-string 1))
                                  (b (match-string 2)))
                              (if (and (< (elt a 0) 128)
                                       (< (elt b 0) 128))
                                  "\\1 \\2"
                                "\\1\\2")))))))

(cl-defun fc-book-flat-footnote (regex)
  (fc-replace-regexp
   regex
   #'(lambda ()
       (let* ((start (match-beginning 0))
              (end (match-end 0))
              (text (match-string 0))
              (ft-text (fc--search (concat "^" text "\\([^\n]+\\)")
                                   :sub 1
                                   :bound 20480)))
         (if ft-text
             (progn
               (fc-replace-text start
                                (- end start)
                                (concat "\\footnote{" ft-text "}"))
               (goto-char (line-beginning-position))
               (delete-char (- (line-end-position) (line-beginning-position)))
               (goto-char end))
           (message "error %s" (concat "^" text "\\([^\n]+\\)")))))
   :from-start t))

(provide 'fc-book)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-book.el ends here
