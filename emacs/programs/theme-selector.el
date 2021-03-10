;;; Program ---  -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(let* ((theme (fc-user-select "Themes"
                              (custom-available-themes)))

       (mode-theme (if *fc-enable-sml*
                       (fc-user-select "Mode line themes"
                                       '(smart-mode-line-powerline
                                         smart-mode-line-light-powerline))
                     nil))

       (modeline-separator (fc-user-select "Mode separator"
                                           '(arrow arrow-fade bar box brace butt chamfer contour curve rounded roundstub slant wave zigzag))))

  (if (fc-void-p modeline-separator)
      (setf modeline-separator (symbol-name powerline-default-separator)))
  (if (fc-void-p theme)
      (setf theme (symbol-name *fc-current-theme*)))

  (setf theme (intern theme)
        modeline-separator (intern modeline-separator))

  (unless (and (eql modeline-separator powerline-default-separator)
               (eql theme *fc-current-theme*))
    (fc-load-theme theme
                   (and mode-theme (intern mode-theme))
                   modeline-separator)))
