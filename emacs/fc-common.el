;;; fc-common.el --- DESCRIPTION -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:

(require 'cl-lib)

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
(setf *fc-light-theme* '(
                         (ef-cyprus . ef-themes)
                         (faff . faff-theme)
                         (gruvbox-light-hard . gruvbox-theme)
                         (plan9 . plan9-theme)
                         )
      *fc-very-light-theme* '(
                              (acme . acme-theme)
                              (leuven . leuven-theme)
                              (material-light . material-theme)
                              (modus-operandi . modus-themes)
                              )
      *fc-dark-theme* (if *is-gui*
                          '(
                            (fantom . fantom-theme)
                            (gruvbox-dark-hard . gruvbox-theme)
                            (material . material-theme)
                            (nord . nord-theme)
                            (sanityinc-tomorrow-eighties . color-theme-sanityinc-tomorrow)
                            (srcery . srcery-theme)
                            )
                        '(
                          (gruvbox-dark-soft . gruvbox-theme)
                          (nord . nord-theme)
                          (sanityinc-tomorrow-eighties . color-theme-sanityinc-tomorrow)
                          )
                        )
      *fc-deep-dark-theme* (if *is-gui*
                               '(
                                 (deeper-blue)
                                 (gotham . gotham-theme)
                                 (hybrid-reverse . hybrid-reverse-theme)
                                 (jazz . jazz-theme)
                                 (sanityinc-tomorrow-night . color-theme-sanityinc-tomorrow)
                                 )
                             '(
                               (deeper-blue)
                               (jazz . jazz-theme)
                               (sanityinc-tomorrow-night . color-theme-sanityinc-tomorrow)
                               ))
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

(defun fc-soothe-face (face percent color)
  (let* ((new-bg (or color
                     (color-darken-name
                      (fc-get-face face :background)
                      percent))))
    (fc-set-face face nil :background new-bg)))

(defun fc-soothe-theme (percent color)
  "Soothe theme.
PERCENT: produce background color by darken this percent.
COLOR: background color."
  (--each '(font-lock-keyword-face
            font-lock-function-name-face)
    (when (facep it)
      (fc-soothe-face it percent color)))

  (--each '(font-lock-string-face
            font-lock-doc-face
            font-lock-type-face
            font-lock-constant-face
            font-lock-property-name-face
            font-lock-variable-name-face

            font-lock-preprocessor-face
            font-lock-function-call-face
            font-lock-variable-use-face
            font-lock-property-use-face)
    (when (facep it)
      (fc-soothe-face it (* percent 0.7) color))))

(defvar *fc-common-light-theme-bg* "cornsilk2")

(defvar *fc-soothe-dark-percent* -4)
(defvar *fc-soothe-light-percent* 8)

(defconst *fc-soothe-color* (make-hash-table))
(--each '((material "gray20")
          (tango-dark "gray21"))
  (puthash (cl-first it) (cl-second it) *fc-soothe-color*))

(defun fc-patch-theme-before-theme-changed ()
  (fc-set-faces '(font-lock-keyword-face
                  font-lock-string-face
                  font-lock-type-face
                  font-lock-function-call-face
                  font-lock-function-name-face
                  font-lock-constant-face
                  font-lock-variable-name-face
                  font-lock-variable-use-face
                  font-lock-property-name-face
                  font-lock-property-use-face)
                :foreground 'unspecified
                :background 'unspecified
                :box 'unspecified
                :underline 'unspecified
                :overline 'unspecified
                :slant 'unspecified))

(defvar *fc-patch-modes* (list #'fc--markdown-patch-theme
                               #'fc--org-patch-theme
                               ))

(defun fc-patch-theme ()
  "Patch theme."
  (when (member *fc-current-theme* '(adwaita
                                     ef-cyprus
                                     ef-deuteranopia-light
                                     ef-tritanopia-light
                                     faff
                                     leuven
                                     modus-operandi))
    (fc-set-face 'default nil
                 :background *fc-common-light-theme-bg*))

  (let ((soothe-percent (if (fc-dark-theme-p)
                            *fc-soothe-dark-percent*
                          *fc-soothe-light-percent*)))
    (pcase *fc-current-theme*
      ('acme
       (setf soothe-percent 6))

      ('adwaita
       (setf soothe-percent 4)
       (fc-set-face 'default nil
                    :foreground "black")
       (fc-set-face 'fringe nil
                    :background "#ee9800")
       (fc-set-face 'font-lock-constant-face nil
                    :foreground "#C52A2A")
       (fc-set-face 'font-lock-string-face nil
                    :foreground "dark green")
       (fc-set-face 'whitespace-trailing nil
                    :background "red4")
       (fc-set-face 'mode-line nil
                    :background "cornsilk3")
       (fc-set-face 'mode-line-inactive nil
                    :background "cornsilk3")
       (fc-set-face 'mode-line-inactive nil
                    :foreground "gray40"))

      ('classic
       (setf soothe-percent -2)
       (fc-set-face 'link nil
                    :foreground "cyan2")
       (fc-set-face 'ivy-current-match nil
                    :background "gray60")
       (fc-set-face 'default nil
                    :foreground "wheat"
                    :background (color-darken-name
                                 (fc-get-face 'default :background)
                                 6))
       (fc-set-face 'fringe nil
                    :background "coral"))

      ('deeper-blue
       (fc-set-face 'mode-line nil
                    :background "gray61"))

      ('faff
       (setf soothe-percent 4))

      ('fantom
       (fc-set-face 'fringe nil
                    :background "#F2A4AC"))

      ((or 'gruvbox-light-soft 'gruvbox-light-medium 'gruvbox-light-hard)
       (setf soothe-percent 6))

      ('leuven
       (setf soothe-percent 6)
       (fc-set-face 'show-paren-match nil
                    :background "pink")
       (fc-set-face 'default nil
                    :foreground "black"))

      ('leuven-dark
       (setf soothe-percent -26)

       (fc-set-face 'aw-leading-char-face nil
                    :height (* *fc-font-height* 2)
                    :foreground "red")
       (fc-set-face 'font-lock-type-face nil
                    :slant 'italic)
       (fc-set-face 'mode-line-inactive nil
                    :foreground "black")
       (fc-set-face 'hl-line nil
                    :background "MediumPurple4")
       (fc-set-face 'highlight nil
                    :background "IndianRed4")
       (fc-set-face 'default nil
                    :foreground "cornsilk"
                    :background "gray22"))

      ('material
       (fc-set-face 'font-lock-type-face nil
                    :slant 'italic)
       (fc-set-face 'fringe nil
                    :background "#ff9800")
       (fc-set-face 'default nil
                    :foreground "wheat")
       (fc-set-face 'ivy-current-match nil
                    :background "gray50"))

      ('material-light
       (setf soothe-percent 2)
       (fc-set-face 'default nil
                    :background (color-darken-name "honeydew" 2)))

      ('modus-operandi
       (defvar modus-modified nil)
       (unless modus-modified
         (setf soothe-percent 6)))

      ('monokai
       (when *is-gui*
         (fc-set-face 'fringe nil
                      :background monokai-orange)
         (fc-set-face 'default nil
                      :background "#282a3a")))

      ('monokai-pro
       (fc-set-face 'minibuffer-prompt nil
                    :foreground (fc-get-face
                                 'font-lock-keyword-face
                                 :foreground))
       (fc-set-face 'vertical-border nil
                    :foreground "gray50"))

      ('monokai-pro-octagon
       (fc-set-face 'minibuffer-prompt nil
                    :foreground (fc-get-face
                                 'font-lock-keyword-face
                                 :foreground))
       (fc-set-face 'font-lock-comment-face nil
                    :foreground "gray80")
       (fc-set-face 'font-lock-doc-face nil
                    :foreground "gray80"))

      ('plan9
       (setf soothe-percent -3)
       (fc-set-face 'default nil
                    :background (color-darken-name
                                 (fc-get-face 'default :background)
                                 8)))

      ('sanityinc-tomorrow-eighties
       (fc-set-face 'ivy-current-match nil
                    :background "gray35")
       (fc-set-face 'fringe nil
                    :background "#de935f"))

      ('sanityinc-tomorrow-night
       (fc-set-face 'default nil
                    :background "#303030"))

      ('srcery
       (fc-set-face 'default nil
                    :background "#303030")
       (fc-set-face 'ivy-current-match nil
                    :background "#404040"))

      ('tango-dark
       (fc-set-face 'default nil
                    :foreground "wheat")
       (fc-set-face 'mode-line nil
                    :foreground "black"
                    :background "cornsilk4")
       (fc-set-face 'hl-line nil
                    :foreground "pale goldenrod"
                    :background "gray30"))

      ('zenburn
       (setf soothe-percent -24)

       (fc-set-face 'font-lock-type-face nil
                    :slant 'italic)
       (fc-set-face 'mode-line nil
                    :box 'unspecified)
       (fc-set-face 'mode-line-inactive nil
                    :foreground "coral"
                    :box 'unspecified)
       (fc-set-face 'fringe nil
                    :background "#ff9800")
       (fc-set-face 'default nil
                    :background "#263238"
                    :foreground "beige")
       (fc-set-face 'aw-leading-char-face nil
                    :height (* *fc-font-height* 2)
                    :foreground "red")
       (fc-set-face 'hl-line nil
                    :background (cond (*is-gui* "#505050")
                                      (*is-colorful* "#505050")
                                      (t "white")))))

    (--each *fc-patch-modes*
      (fc-funcall it))

    (fc-soothe-theme soothe-percent
                     (gethash *fc-current-theme*
                              *fc-soothe-color*))))

(defun fc--org-patch-theme ()
  (fc-set-face 'org-agenda-structure nil
               :height 1.1)
  (fc-set-face 'org-agenda-date nil
               :height 1.1)
  (fc-set-face 'org-agenda-date-today nil
               :height 1.2)
  (fc-set-face 'org-agenda-date-weekend nil
               :height 1.1)

  (pcase *fc-current-theme*
    ('leuven-dark
     (fc-set-face 'org-todo nil
                  :foreground "pale green"))

    ('zenburn
     (fc-set-face 'org-superstar-header-bullet nil
                  :foreground "tomato"))
    ))

(defun fc--markdown-patch-theme ()
  (pcase *fc-current-theme*
    ('material
     (fc-set-face 'markdown-header-face-1 nil
                  :height 1.2)
     )

    ('modus-operandi
     (fc-set-face 'markdown-code-face nil :background "gray90")
     )

    ('zenburn
     (fc-set-face 'markdown-header-delimiter-face nil
                  :foreground "tomato"))
    ))

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
