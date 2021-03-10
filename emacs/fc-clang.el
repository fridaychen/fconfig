;; fc-clang.el --- setup c environment -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(defconst *fc-c-map*
  (fc-make-keymap
   `(("m" mark-ifdef)
     ("C" fc--clang-format-off-region)
     )
   "fc-c-map"
   *fc-func-mode-map*)
  "KEYS m: mark ifdef  C: clang-format off  E: org exit edit.")

(defun fc-c-mode-func ()
  "Mode func."
  (fc-modal-head-key "C" '*fc-c-map*))

(defun fc-c++-mode-func ()
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

(fc-install 'protobuf-mode)

(fc-load 'cc-mode
  :local t

  :after (progn
           (setq-default c-default-style "linux"
                         indent-tabs-mode t)

           (--each (list c-mode-map c++-mode-map objc-mode-map java-mode-map)
             (fc-unbind-keys '("M-i" "M-j" "M-k" "M-l" "M-C-i" "M-C-j" "M-C-k" "M-C-l") it)
             (fc-bind-keys `(("TAB" ,(fc-manual (fc-tab-key #'c-indent-line-or-region)))) it))

           (defun fc--setup-c-mode ()
             (c-toggle-electric-state 1)

             (--each '("FIXME:" "TODO:" "ToDo:" "MEMO:"
                       "FIXME :" "TODO :" "MEMO :")
               (highlight-phrase it)))

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
                    (AlignEscapedNewlines . Left)
                    (AlignConsecutiveMacros . true)
                    (AlignOperands . true)
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
                       "LLVM")
                     *fc-column-limit*
                     c-basic-offset
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

           (fc-add-fmt 'c-mode 'fc-generate-clang-cmd nil)
           (fc-add-fmt 'c++-mode 'fc-generate-clang-cmd nil)
           (fc-add-fmt 'protobuf-mode 'fc-generate-clang-cmd nil)

           (add-hook 'c-mode-hook 'fc--setup-c-mode)
           (add-hook 'c++-mode-hook 'fc--setup-c-mode)))

(provide 'fc-clang)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-clang.el ends here
