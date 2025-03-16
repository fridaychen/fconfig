;;; fc-lang.el --- language utility -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(cl-defun fc-funcall (func &key default args)
  "Call function.
FUNC: function.
DEFAULT: function used if func is nil.
ARGS: argument for function."
  (when (and (symbolp func)
             (not (fboundp func)))
    (setf func default))

  (cond
   ((not func))

   ((and (commandp func t)
         (null args))
    (call-interactively func))

   ((functionp func)
    (apply func args))

   (t
    (fc-each args
      (eval-expression it))
    (eval-expression func))))

(cl-defun fc-replace-string (from to &key from-start)
  "String replacement.
FROM: from-string.
TO: to-string.
FROM-START: non-nil means starts from beginning."
  (when from-start
    (goto-char (point-min)))

  (cl-loop while (search-forward from nil t) do
           (if (stringp to)
               (replace-match to)
             (fc-funcall to))
           count 1))

(cl-defun fc-replace-regexp (regex to &key from-start)
  "REGEXP String replacement.
REGEX: regexp.
TO: to-string.
FROM-START: non-nil means starts from beginning."
  (when from-start
    (goto-char (point-min)))

  (cl-loop while (re-search-forward regex nil t) do
           (if (stringp to)
               (replace-match to)
             (fc-funcall to))
           count 1))

(cl-defun fc-replace-region (start end to)
  (delete-region start end)
  (goto-char start)
  (if (stringp to)
      (insert to)
    (fc-funcall to)))

(cl-defun fc--replace-text (pos del-n sub)
  (goto-char pos)
  (delete-char del-n)
  (insert sub))

(cl-defun fc-replace-text (pos del-n sub)
  (save-excursion
    (fc--replace-text pos del-n sub)))

(defun fc-string (obj)
  "Convert any obj into string.
OBJ: object"
  (cond ((null obj)
         "")

        ((stringp obj)
         obj)

        ((numberp obj)
         (number-to-string obj))

        ((symbolp obj)
         (symbol-name obj))

        ((object-p obj)
         (cl-print-object obj nil))

        (t
         (format "%S" obj))))

(cl-defun fc-concat (&rest rest)
  (when-let* ((x (fc-first rest
                   (not (null it)))))
    (pcase (type-of x)
      ('string (apply #'concat rest))
      ('cons (apply #'seq-concatenate 'list rest))
      (_ (mapconcat #'fc-string rest "")))))

(cl-defun fc--search (regex &key sub bound count begin (default ""))
  (when begin
    (goto-char (point-min)))

  (if (re-search-forward regex (and bound (+ (point) bound)) t (or count 1))
      (match-string (or sub 0))
    default))

(cl-defun fc-search (regex &key sub bound count begin default)
  (save-excursion
    (fc--search regex
                :sub sub
                :bound bound
                :count count
                :begin begin
                :default default)))

(cl-defun fc-add-to-list (name &rest rest)
  (fc-each rest
    (add-to-list name it)))

(cl-defun fc-add-to-hook (hook &rest rest)
  (fc-each rest
    (add-hook hook it)))

(cl-defun fc-bool (obj)
  (unless obj
    (cl-return-from fc-bool nil))

  (let* ((s (fc-string obj))
         (v (intern (downcase s))))
    (member v '(true yes))))

(cl-defun fc-make-hash-table (data)
  (let ((table (make-hash-table)))
    (mapc (lambda (x)
            (puthash (car x) (cdr x) table))
          data)
    table))

(cl-defun fc-member (var set)
  (cond
   ((or (numberp set) (symbolp set))
    (eq var set))

   ((stringp set)
    (string-equal var set))

   ((sequencep set)
    (cl-position var set))

   ((hash-table-p set)
    (gethash var set))))

(cl-defmacro fc-val (s)
  `(and (boundp (quote ,s)) ,s))

(provide 'fc-lang)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-lang.el ends here
