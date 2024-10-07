;;; fc-common.el --- DESCRIPTION -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:

(require 'cl-lib)
(require 'whitespace)

;; fonts
(defconst *fc-cjk-fonts*
  (cond
   (*is-linux*
    '((:family "TsangerJinKai01-27533")
      (:family "方正宋刻本秀楷简补全")
      (:family "STFangSong")
      (:family "Microsoft Yahei")))

   (*is-mac*
    '((:family "TsangerJinKai01-27533")
      (:family "STFangSong")
      (:family "Microsoft Yahei")))

   (t
    '(("Microsoft Yahei")))))

(defconst *fc-symbol-fonts*
  '((:family "Noto Color Emoji")
    (:family "Symbloa")
    (:family "Apple Color Emoji")))

(defconst *fc-english-fonts*
  (cond
   (*is-linux*
    '((:family "Sarasa Mono SC")
      (:family "Iosevka SS02")
      (:family "Iosevka Curly")
      (:family "Iosevka")
      (:family "Fira Code")
      (:family "Hack")
      ))

   (*is-mac*
    '((:family "Sarasa Mono SC")
      (:family "Iosevka SS02")
      (:family "Iosevka Curly")
      (:family "Fira Code")
      (:family "Hack")
      (:family "Fira Mono")))

   (t
    '(("Monaco")))))

(defconst *fc-english-reading-fonts*
  '((:family "ITC Cerigo Std Book")
    (:family "Bookerly")
    (:family "Helvetica")
    (:family "Baskerville")
    (:family "Garamond")
    (:family "Literata")
    (:family "Constantia")))

(defconst *fc-cjk-reading-fonts*
  '((:family "LXGW WenKai")
    (:family "方正宋刻本秀楷简补全")))

(defconst *fc-mode-line-fonts*
  '((:family "Sarasa Mono Slab SC")
    ))

(cond
 (*is-linux*
  (fc-add-to-list 'face-font-rescale-alist
                  (cons "Symbols Nerd Font Mono" 0.95)
                  (cons "Noto Color Emoji" 0.9)))

 (*is-mac*
  (fc-add-to-list 'face-font-rescale-alist
                  (cons "Symbols Nerd Font Mono" 0.9)
                  (cons "Apple Color Emoji" 0.8))))

;; sounds
(defconst *fc-sounds* '((welcome "startrek.mp3")
                        (incoming "incomingmsg.mp3")
                        (sweep "sweep.mp3")
                        (ringtong "ctu24.mp3")
                        (intercom "intercom.mp3")
                        (cheerup "mission_impossible_theme.mp3"
                                 "the_imperial_march.mp3"
                                 "terminator.mp3")))

;; completion
(defconst *fc-completion* 'vertico)
(defconst *fc-enable-company* nil)

;; theme
(setf *fc--work-themes* '((dark
                           (ayu-grey . ayu-theme)
                           leuven-dark
                           zenburn)
                          (light
                           leuven
                           (gruvbox-light-soft . gruvbox-theme)
                           (ef-cyprus . ef-themes))
                          (deep-dark
                           modus-vivendi-deuteranopia
                           (sanityinc-tomorrow-night . color-theme-sanityinc-tomorrow)))

      ;; fringe width 2.5mm for laptop, otherwise 3mm
      *fc-fringe-width* (if *is-gui*
                            (truncate (* (fc-display-ppi)
                                         (/ (if *is-laptop* 2.5 2.8) 24.5)))
                          0)
      ;; font height 145 for laptop, otherwiseo 160
      *fc-font-height* (if *is-laptop* 145
                         (cond
                          (*is-mac* 180)
                          (*is-linux* 142)
                          (*is-cygwin* 142)
                          (*is-windows* 142)))
      *fc-font-mode-line-delta* 0)

(require 'classic-theme)

(defvar *fc-soothe-light-deltas* '((light -0.08 -0.05 -0.04)
                                   (dark 0.06 0.03 0.02)
                                   (deep-dark 0.08 0.05 0.03)))

(defun fc--soothe-theme ()
  "Soothe theme.
PERCENT: produce background color by darken this percent.
COLOR: background color."
  (cl-multiple-value-bind (level-1 level-2 level-3)
      (alist-get *fc-theme-mode* *fc-soothe-light-deltas*)

    (fc-each '(font-lock-keyword-face
               font-lock-function-name-face)
      (when (facep it)
        (fc--adjust-face-bg-light it level-1)))

    (fc-each '(font-lock-string-face
               font-lock-type-face
               font-lock-constant-face
               font-lock-property-name-face
               font-lock-variable-name-face

               font-lock-preprocessor-face
               font-lock-function-call-face
               font-lock-variable-use-face
               font-lock-property-use-face)
      (when (facep it)
        (fc--adjust-face-bg-light it level-2)))

    (fc-each '(font-lock-builtin-face
               font-lock-doc-face)
      (when (facep it)
        (fc--adjust-face-bg-light it level-3)))))

(defvar *fc-common-light-theme-bg* "cornsilk2")

(defun fc-patch-theme-before-theme-changed ()
  (fc-set-faces '(font-lock-keyword-face
                  font-lock-function-name-face

                  font-lock-string-face
                  font-lock-doc-face
                  font-lock-type-face
                  font-lock-constant-face
                  font-lock-property-name-face
                  font-lock-variable-name-face

                  font-lock-builtin-face
                  font-lock-comment-face
                  font-lock-preprocessor-face
                  font-lock-function-call-face
                  font-lock-variable-use-face
                  font-lock-property-use-face)
                :foreground 'unspecified
                :background 'unspecified
                :box 'unspecified
                :underline 'unspecified
                :overline 'unspecified
                :slant 'unspecified))

(defun fc--patch-face-hl-line ()
  (fc-set-faces '(highlight hl-line)
                :underline 'unspecified)
  )

(defun fc--patch-face-type ()
  (fc-set-face 'font-lock-type-face nil :slant 'italic))

(defun fc--patch-face-func-name ()
  (fc-set-face 'font-lock-function-name-face nil :overline t))

(defun fc--patch-face-mode-line ()
  (fc-set-faces '(mode-line mode-line-active mode-line-inactive)
                :box 'unspecified)
  (fc-set-face 'mode-line-inactive nil
               :foreground (if (fc-dark-face-p 'mode-line-inactive)
                               "cornsilk"
                             "black")))

(defun fc--patch-theme-whitespace-trailing ()
  (fc--set-face-bg-light 'whitespace-trailing 0.5))

(defvar *fc-default-face-bg-light* '((light 0.8)
                                     (dark 0.2)
                                     (deep-dark 0.2)))
(defvar *fc-default-face-fg-light* '((light 0.1)
                                     (dark 0.95)
                                     (deep-dark 0.7)))

(defun fc--patch-face-default ()
  (fc--set-face-bg-light 'default
                         (car (alist-get *fc-theme-mode* *fc-default-face-bg-light*)))
  (fc--set-face-fg-light 'default
                         (car (alist-get *fc-theme-mode* *fc-default-face-fg-light*))))

(defun fc--patch-face-comment ()
  (fc--set-face-contrast 'font-lock-comment-face 0.5))

(defun fc--patch-face-aw-leading ()
  (fc-set-face 'aw-leading-char-face nil
               :height (* *fc-font-height* 2)
               :foreground "red"))

(defvar *fc-patch-theme-hook* (list #'fc--patch-face-hl-line
                                    #'fc--patch-face-type
                                    #'fc--patch-face-func-name
                                    #'fc--patch-face-mode-line
                                    #'fc--patch-theme-whitespace-trailing
                                    #'fc--patch-face-default
                                    #'fc--patch-face-comment
                                    #'fc--patch-face-aw-leading
                                    #'fc--soothe-theme
                                    ))

(defun fc--patch-specific-theme ()
  (pcase *fc-current-theme*
    ('ayu-grey
     (fc-set-face 'org-verse nil
                  :foreground "beige")
     (fc-set-face 'highlight nil
                  :background "IndianRed4"))

    ('classic
     (fc-set-face 'link nil
                  :foreground "cyan2"))

    ('ef-cyprus
     (fc-set-face 'default nil
                  :background "#C1E6C6"))

    ('leuven
     (fc-set-face 'show-paren-match nil
                  :background "pink"))

    ('leuven-dark
     (fc-set-face 'hl-line nil
                  :background "PaleVioletRed4")
     (fc-set-face 'highlight nil
                  :background "IndianRed4"))

    ('tango-dark
     (fc-set-face 'mode-line nil
                  :foreground "black"
                  :background "cornsilk4")
     (fc-set-face 'highlight nil
                  :foreground "white"
                  :background "IndianRed4")
     (fc-set-face 'hl-line nil
                  :foreground "pale goldenrod"
                  :background "gray30"))

    ('zenburn
     (fc-set-face 'default nil
                  :background "#263238")
     (fc-set-face 'vertico-current nil
                  :background "IndianRed4"
                  :underline nil)
     (fc-set-face 'hl-line nil
                  :background (cond (*is-gui* "#505050")
                                    (*is-colorful* "#505050")
                                    (t "white"))))))

(defun fc-patch-theme ()
  "Patch theme."
  (fc--patch-specific-theme)

  (run-hooks '*fc-patch-theme-hook*))

;; players
(cl-defun fc-init-user-player ()
  "Init user players."
  (unless *fc-enable-player*
    (cl-return-from fc-init-user-player))

  (cond
   (*is-linux*
    (setf *fc-prefer-players*
          '("Lollypop" "quodlibet" "rhythmbox")))

   (*is-cygwin*
    (setf *fc-player* (fc-player-foobar :name "foobar")))

   (*is-mac*
    (setf *fc-player* (fc-player-quodlibet :name "Quod Libet [app]"))))

  (when (fboundp #'fc-player-auto-select)
    (fc-idle-delay
      (fc-player-auto-select))))

;; files
(defconst *fc-ignore-dir* '("\\.git" "\\.cquery_cached_index" "\\.ccls-cache" "\\.snv" "\\.bin" "depend" "__pycache__"))
(defconst *fc-ignore-file* '("TOP" "GRTAGS" "GTAGS" "GPATH" "\\.DS_Store" "\\.o\$" "\\.pyc\$" "\\.mobi\$" "\\.azw3\$" "\\.pdf\$" "\\.bin\$" "\\.d\$" "\\.elc\$" "\\.old\$" "\\.bak\$" "~\$" "#\$"))

(fc-add-to-list 'completion-ignored-extensions
                ".DS_Store"
                ".mobi" ".azw3" ".epub" ".pdf"
                ".bin" ".d" ".pyc" ".o" ".elc"
                ".old" ".bak"
                "TOP" "GRTAGS" "GTAGS" "GPATH")

;; environment
(cond
 (*is-mac*
  (setenv "FIND" "gfind")
  (setenv "GREP" "ggrep")
  (setenv "XARGS" "gxargs"))

 (t
  (setenv "FIND" "find")
  (setenv "GREP" "grep")
  (setenv "XARGS" "xargs")))

;; hide-show modes setting
(defconst *fc-doc-modes* '(html-mode
                           latex-mode
                           markdown-mode
                           org-mode
                           yaml-mode
                           xml-mode))

;; Plot settings
(defvar *fc-plot-font* "Sarasa Gothic CL")
(defvar *fc-plot-bg* "#C1E6C6")
(defvar *fc-plot-dpi* (if *is-gui* (fc-display-ppi) 96))

(provide 'fc-common)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-common.el ends here
