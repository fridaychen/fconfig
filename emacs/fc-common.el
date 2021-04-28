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
    '((:family "STFangSong")
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
    '((:family "Fira Code")
      (:family "Hack")
      ))

   (*is-mac*
    '((:family "Fira Code")
      (:family "Hack")
      (:family "Fira Mono")))

   (t
    '(("Monaco")))))

(defconst *fc-reading-fonts*
  '((:family "DejaVu Serif")
    (:family "Baskerville")
    (:family "Garamond")
    (:family "Literata")
    (:family "Constantia")))

(when *is-gui*
  (create-fontset-from-fontset-spec
   "-*-*-*-*-*-*-*-*-*-*-*-*-fontset-fc,
    ascii:-*-monaco-*-*-*-*-*-*-*-*-*-*-iso8859-1,
    japanese-jisx0208:-*-kaiti-*-*-*-*-*-*-*-*-*-*-*-*,
    chinese-gbk:-*-kaiti-*-*-*-*-*-*-*-*-*-*-*-*")
  (cond
   (*is-mac*
    (create-fontset-from-fontset-spec
     "-*-*-*-*-*-*-*-*-*-*-*-*-fontset-read,
   ascii:-*-monaco-*-*-*-*-*-*-*-*-*-*-iso8859-1,
   chinese-gbk:-*-stfangsong-*-*-*-*-*-*-*-*-*-*-*-*"))
   (*is-linux*
    (create-fontset-from-fontset-spec
     "-*-*-*-*-*-*-*-*-*-*-*-*-fontset-read,
   ascii:-*-baskerville-*-*-*-*-*-*-*-*-*-*-iso8859-1,
   chinese-gbk:-*-方正宋刻本秀楷简补全-*-*-*-*-*-*-*-*-*-*-*-*"))
   (t
    (create-fontset-from-fontset-spec
     "-*-*-*-*-*-*-*-*-*-*-*-*-fontset-read,
   ascii:-*-monaco-*-*-*-*-*-*-*-*-*-*-iso8859-1,
   chinese-gbk:-*-microsoft yahei-*-*-*-*-*-*-*-*-*-*-*-*"))))

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
(if *is-mac*
    (defconst *fc-completion* 'ivy)
  (defconst *fc-completion* 'helm))

;; screen
(setf *fc-enable-hideshowvis* nil)

;; theme
(setf *fc-light-theme* '(
                         (gruvbox-light-hard . gruvbox-theme)
                         (material-light . material-theme)
                         (sanityinc-tomorrow-day . color-theme-sanityinc-tomorrow)
                         tango
                         )
      *fc-dark-theme* (if *is-gui*
                          '(
                            (fantom . fantom-theme)
                            (gruvbox-dark-hard . gruvbox-theme)
                            (material . material-theme)
                            (sanityinc-tomorrow-eighties . color-theme-sanityinc-tomorrow)
                            (srcery . srcery-theme)
                            )
                        '(
                          (gruvbox-dark-soft . gruvbox-theme)
                          (sanityinc-tomorrow-eighties . color-theme-sanityinc-tomorrow)
                          )
                        )
      *fc-deep-dark-theme* (if *is-gui*
                               '(
                                 (gotham . gotham-theme)
                                 (hybrid-reverse . hybrid-reverse-theme)
                                 (jazz . jazz-theme)
                                 (nord . nord-theme)
                                 )
                             '(
                               (jazz . jazz-theme)
                               (nord . nord-theme)
                               (sanityinc-tomorrow-night . color-theme-sanityinc-tomorrow)
                               ))
      ;; fringe width 2.5mm for laptop, otherwise 3mm
      *fc-fringe-width* (if *is-gui*
                            (truncate (* (fc-display-ppi)
                                         (/ (if *is-laptop* 2.5 3) 24.5)))
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

(defun fc-soothe-theme (percent color)
  "Soothe theme.
PERCENT: produce background color by darken this percent.
COLOR: background color."
  (--each '(font-lock-keyword-face
            font-lock-function-name-face
            font-lock-variable-name-face
            font-lock-type-face
            font-lock-constant-face)
    (let* ((new-bg (if color color
                     (color-darken-name
                      (fc-get-face-attribute it :background)
                      percent))))
      (fc-set-face-attribute it nil :background new-bg))))

(defvar *fc-soothe-percent* -4)
(defconst *fc-soothe-color* (make-hash-table))
(--each '((material "gray20")
          (tango-dark "gray23"))
  (puthash (cl-first it) (cl-second it) *fc-soothe-color*))

(defun fc-patch-theme ()
  "Patch theme."
  (let ((soothe-percent *fc-soothe-percent*))
    (pcase *fc-current-theme*
      ('classic
       (setf soothe-percent -2)
       (fc-set-face-attribute 'default nil
                              :background (color-darken-name
                                           (fc-get-face-attribute 'default :background)
                                           2)))

      ('material
       (fc-set-face-attribute 'default nil
                              :foreground "#dfdfdf"
                              :background "#102a20")
       (fc-set-face-attribute 'markdown-header-face-1 nil
                              :height 1.2)
       (fc-set-face-attribute 'org-level-1 nil
                              :height 1.2)
       (fc-set-face-attribute 'org-level-2 nil
                              :height 1.1))

      ('monokai-pro-octagon
       (fc-set-face-attribute 'font-lock-comment-face nil
                              :foreground "gray80")
       (fc-set-face-attribute 'font-lock-doc-face nil
                              :foreground "gray80")
       )

      ('tango-dark
       (fc-set-face-attribute 'default nil
                              :background "#203420")
       (fc-set-face-attribute 'hl-line nil
                              :foreground "LightPink2"
                              :background "gray30"))

      ('zenburn
       (fc-set-face-attribute 'default nil
                              :background "#383838")
       (fc-set-face-attribute 'hl-line nil
                              :background (cond (*is-gui* "#1E3124")
                                                (*is-colorful* "#505050")
                                                (t "white")))))

    (fc-soothe-theme soothe-percent
                     (gethash *fc-current-theme*
                              *fc-soothe-color*))

    (fc-set-face-attribute 'hi-yellow nil
                           :background "yellow3")))

;; players
(cl-defun fc-init-user-player ()
  "Init user players."
  (unless *fc-enable-player*
    (cl-return-from fc-init-user-player))

  (setf *fc-players* (cond
                      (*is-linux*
                       (list (fc-player-mpris :name "Lollypop")
                             (fc-player-mpris :name "quodlibet")
                             (fc-player-mpris :name "rhythmbox")))

                      (*is-cygwin*
                       (list (fc-player-foobar :name "foobar")))

                      (*is-mac*
                       (list (fc-player-itunes :name "iTunes")))))

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
(defconst *fc-prog-modes* '(c-mode
                            c++-mode
                            emacs-lisp-mode
                            go-mode
                            haskell-mode
                            lisp-mode
                            ocaml-mode
                            sh-mode
                            python-mode))

(defconst *fc-doc-modes* '(html-mode
                           latex-mode
                           markdown-mode
                           org-mode
                           yaml-mode
                           xml-mode))

(provide 'fc-common)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-common.el ends here
