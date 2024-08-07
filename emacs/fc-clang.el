;; fc-clang.el --- setup c environment -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(defun fc-c-portal ()
  "Show c portal."
  (fc-select-func
   "C"
   `(
     ("insert standard headers" . fc-c-insert-std-header)
     ("insert unix headers"     . fc-c-insert-unix-header)
     ("mark ifdef"              . mark-ifdef)
     )))

(defconst *fc-c-map*
  (fc-make-keymap
   `(
     ("h c" fc-c-insert-std-header)
     ("h u" fc-c-insert-unix-header)
     ("i d" ,(fc-manual (fc-expand-snippet "do")))
     ("i f" ,(fc-manual (fc-expand-snippet "f")))
     ("i i" ,(fc-manual (fc-expand-snippet "if")))
     ("i s" ,(fc-manual (fc-expand-snippet "switch")))
     ("i w" ,(fc-manual (fc-expand-snippet "while")))
     ("j" c-backward-conditional)
     ("k" c-forward-conditional)
     ("C" fc--clang-format-off-region)
     ("SPC" fc-c-portal)
     )
   "fc-c-map"
   *fc-func-mode-map*)
  "KEYS h c: insert c headers  h u: insert unix headers  i d: insert do  i f: insert func  i i: insert if  i w: insert switch  i w: insert while  C: clang-format off  E: org exit edit.")

(defun fc--c-mode-func ()
  "Mode func."
  (fc-modal-head-key "C" '*fc-c-map*))

(defun fc--c-ts-mode-func ()
  "Mode func."
  (fc-modal-head-key "C" '*fc-c-map*))

(defun fc--c++-mode-func ()
  "Mode func."
  (fc-modal-head-key "CC" '*fc-c-map*))

(defun fc--c++-ts-mode-func ()
  "Mode func."
  (fc-modal-head-key "CC" '*fc-c-map*))

(defun fc--clang-format-off-region (start end)
  "Format region.
START: start of region.
END: end of region."
  (interactive "r")

  (fc-region start end
    (goto-char (point-min))
    (insert "  /* clang-format off */")
    (c-indent-line-or-region)
    (insert "\n")

    (goto-char (point-max))
    (unless (zerop (current-column))
      (insert "\n"))
    (insert "  /* clang-format on */")
    (c-indent-line-or-region)))

(defun fc-c-insert-std-header ()
  "Insert standard c header files."
  (interactive)
  (insert "#include <stdio.h>\n"
          "#include <stdlib.h>\n"
          "#include <string.h>\n"))

(defun fc-c-insert-unix-header ()
  "Insert unix header files."
  (interactive)
  (insert "#include <unistd.h>\n"
          "#include <errno.h>\n"
          "#include <fcntl.h>\n"
          "#include <sys/types.h>\n"
          "#include <sys/stat.h>\n"))

(fc-install 'protobuf-mode)

(fc-load 'cc-mode
  :local t

  :after (progn
           (setf c-default-style '((java-mode . "java")
                                   (awk-mode . "awk")
                                   (other . "linux")))

           (fc-each (list c-mode-map c++-mode-map objc-mode-map java-mode-map)
             (fc-unbind-keys '("M-i" "M-j" "M-k" "M-l" "M-C-i" "M-C-j" "M-C-k" "M-C-l") it)
             (fc-bind-keys `(("TAB" ,(fc-manual (fc-tab-key #'c-indent-line-or-region)))) it))

           (require 'elide-head)
           (add-hook 'c-mode-common-hook 'elide-head)

           (defun fc--c-setup ()
             (c-toggle-electric-state 0)
             (c-toggle-auto-newline 1)
             (c-toggle-hungry-state 1)

             (setf indent-tabs-mode nil
                   c-basic-offset 4
                   tab-width 4))

           (defalias 'fc--c++-setup 'fc--c-setup)

           (cl-defun fc-tree2str (tree &optional (str "{"))
             (when (null tree)
               (cl-return-from fc-tree2str (concat str "}")))

             (let* ((cons (car tree))
                    (key (car cons))
                    (value (cdr cons))
                    (rest (cl-rest tree)))
               (setq str (concat str (fc-string key) ": "))

               (if (listp value)
                   (setq str (concat str (fc-tree2str value) ","))
                 (setq str (concat str
                                   (cond
                                    ((eq t value)
                                     "true")

                                    ((eq nil value)
                                     "false")

                                    (t
                                     (fc-string value)))
                                   ",")))

               (fc-tree2str rest str)))

           (setf *fc-clang-format-style*
                 (fc-tree2str
                  '(
                    (BasedOnStyle . "%s")

                    (AllowAllArgumentsOnNextLine . false)
                    (AllowAllParametersOfDeclarationOnNextLine . false)
                    (AlignAfterOpenBracket . true)
                    (AlignConsecutiveAssignments . true)
                    (AlignConsecutiveBitFields . true)
                    (AlignEscapedNewlines . Left)
                    (AlignConsecutiveMacros . true)
                    (AlignOperands . true)
                    (AlignTrailingComments . true)
                    (AllowShortBlocksOnASingleLine . false)

                    (AllowShortFunctionsOnASingleLine . None)
                    (AllowShortIfStatementsOnASingleLine . false)
                    (AllowShortLoopsOnASingleLine . false)
                    (AlwaysBreakAfterDefinitionReturnType . None)
                    (AlwaysBreakAfterReturnType . None)
                    (AlwaysBreakBeforeMultilineStrings . false)
                    (AlwaysBreakTemplateDeclarations . false)

                    (BinPackArguments . false)
                    (BinPackParameters . false)
                    (BreakBeforeBraces . "Linux")
                    (BraceWrapping
                     (AfterStruct . false)
                     (BeforeElse . false)
                     (IndentBraces . false))
                    (BreakBeforeBinaryOperators . "NonAssignment")
                    (BreakConstructorInitializersBeforeComma . false)
                    (ColumnLimit . "%d")
                    (ConstructorInitializerAllOnOneLineOrOnePerLine . false)
                    (Cpp11BracedListStyle . false)
                    (DisableFormat . false)
                    (ExperimentalAutoDetectBinPacking . false)
                    (IndentCaseLabels . false)
                    (IndentWidth . "%d")
                    (SortIncludes . false)
                    (UseTab . "%s"))))

           (defun fc--generate-clang-style ()
             (format *fc-clang-format-style*
                     (if (boundp 'fc-proj-clang-style)
                         fc-proj-clang-style
                       "Google")
                     *fc-column-limit*
                     (if (eq c-basic-offset 'set-from-style)
                         4
                       c-basic-offset)
                     (if indent-tabs-mode
                         "Always"
                       "Never")))

           (cl-defun fc-generate-clang-cmd ()
             (list
              "clang-format"
              "--assume-filename"
              buffer-file-name
              "-style"
              (if (fc-locate-file-in-path '(".clang-format"))
                  "file"
                (fc--generate-clang-style))))

           (fc-add-fmt 'c-mode #'fc-generate-clang-cmd nil)
           (fc-add-fmt 'c++-mode #'fc-generate-clang-cmd nil)
           (fc-add-fmt 'protobuf-mode #'fc-generate-clang-cmd nil)

           (add-hook 'c-mode-hook #'fc--c-setup)
           (add-hook 'c++-mode-hook #'fc--c-setup)))

(fc-load 'c-ts-mode
  :local t
  :after
  (progn
    (defun fc--c-ts-setup ()
      (setq c-ts-mode-indent-offset 4
            tab-width 4))

    (add-hook 'c-ts-mode-hook #'fc--c-ts-setup)

    (fc-add-fmt 'c-ts-mode #'fc-generate-clang-cmd nil)))

(provide 'fc-clang)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-clang.el ends here
