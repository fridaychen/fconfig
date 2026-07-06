;;; fc-language.el --- Language -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; input methmod

(defvar *fc-input-methods* nil "Enabled input methods list.")

(fc-load 'rime
  :after (progn
           (setf rime-show-candidate 'posframe)

           (add-to-list '*fc-input-methods* 'rime)))

(fc-load 'mozc
  :enable (executable-find "mozc_emacs_helper")
  :idle t
  :before (setq mozc-leim-title "あ")
  :after (add-to-list '*fc-input-methods* 'japanese-mozc))

(defun fc-next-input-method ()
  "Switch to next input method."
  (interactive)

  (unless (null *fc-input-methods*)
    (setf *fc-input-methods* (-rotate 1 *fc-input-methods*))
    (set-input-method (cl-first *fc-input-methods*))))

(defun fc--disable-input-method ()
  "Disable input method."
  (set-input-method nil))

(defvar *fc--input-method-bak* nil)

(defun fc--auto-toggle-input-method ()
  "Automatically toggle input method."
  (if fc-modal-mode
      (progn
        (setf *fc--input-method-bak* current-input-method)
        (when current-input-method
          (fc--disable-input-method)))
    (set-input-method *fc--input-method-bak*)))

(add-hook '*fc-modal-hook* #'fc--auto-toggle-input-method)

;; utilities

(defun fc-zh-to-number (str)
  "Convert chinese number string to number.
STR: chinese number string."
  (let ((al '((?零 . 0)
              (?一 . 1)
              (?二 . 2)
              (?两 . 2)
              (?三 . 3)
              (?四 . 4)
              (?五 . 5)
              (?六 . 6)
              (?七 . 7)
              (?八 . 8)
              (?九 . 9)
              (?十 . 10)
              (?百 . 100)
              (?千 . 1000)
              (?万 . 10000)))
        (n 0)
        (ret 0))
    (fc-each (append str nil)
      (let ((v (cdr (assoc it al))))
        (if (< v 10)
            (setf n (+ (* n 10) v))
          (when (and (= n 0) (= v 10))
            (setf n 1))
          (cl-incf ret (* n v))
          (setf n 0))))
    (+ ret n)))

(defconst *fc-en-number* (fc-make-hash-table
                          `(
                            (,(intern "zero") 0)
                            (,(intern "one") 1)
                            (,(intern "two") 2)
                            (,(intern "three") 3)
                            (,(intern "four") 4)
                            (,(intern "five") 5)
                            (,(intern "six") 6)
                            (,(intern "seven") 7)
                            (,(intern "eight") 8)
                            (,(intern "nine") 9)
                            (,(intern "ten") 10)
                            (,(intern "eleven") 11)
                            (,(intern "twelve") 12)
                            (,(intern "thirteen") 13)
                            (,(intern "fourteen") 14)
                            (,(intern "fifteen") 15)
                            (,(intern "sixteen") 16)
                            (,(intern "seventeen") 17)
                            (,(intern "eighteen") 18)
                            (,(intern "nineteen") 19)
                            (,(intern "twenty") 20)
                            (,(intern "thirty") 30)
                            (,(intern "forty") 40)
                            (,(intern "fifty") 50)
                            (,(intern "sixty") 60)
                            (,(intern "seventy") 70)
                            (,(intern "eighty") 80)
                            (,(intern "ninety") 90)
                            (,(intern "hundred") 100)
                            (,(intern "thousand") 1000))))

(defun simple-english-to-number (s)
  "Convert one word english number to numver.
S: one word english numver."
  (car (gethash (intern s) *fc-en-number* '(0))))

(cl-defun fc-en-to-number (s)
  "Convert english number to number.
S: english number."
  (let ((n (simple-english-to-number s)))
    (when (> n 0)
      (cl-return-from fc-en-to-number n)))

  (save-match-data
    (cl-loop with sum = 0
             for x in (split-string s "[ -]")
             do
             (let ((num (simple-english-to-number x)))
               (pcase num
                 ((or 100 1000)
                  (setq sum (* sum num)))
                 (_
                  (cl-incf sum num))))
             finally return sum)))

;; detech char-script of string
(defun fc-detect-char-script (text)
  "Detect char set.
TEXT: target text."
  (let ((map (make-hash-table)))
    (mapc (lambda (x) (puthash (aref char-script-table x) t map))
          text)
    map))

(defun fc-detect-has-wide-char (text)
  "Detect if wide char exists.
TEXT: target text."
  (let ((map (fc-detect-char-script text)))
    (fc-first '(han kana emoji)
      (gethash it map))))

(cl-defun fc-detect-buf-has-wide-char (&optional (buffer (current-buffer)) (max 512))
  "Detect if wide char exists in buffer.
BUFFER: buffer.
MAX: test limitation."
  (with-current-buffer buffer
    (fc-detect-has-wide-char (buffer-substring 1 (min max (point-max))))))

(provide 'fc-language)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-language.el ends here
