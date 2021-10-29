;;; fc-xpm.el --- XPM -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(defun fc--xpm-header (width height color-def)
  (let ((values (format "\"%d %d %d 1\","
                        width height (length color-def)))
        (colors (cl-loop for x in color-def
                         collect (format "\"%s c %s\",\n" (car x) (cdr x)))))
    (format "/* XPM */
static char * xpm [] = {
%s
%s"
            values (fc--text "" colors))))

(defun fc--xpm-footer ()
  "};")

(defun fc--xpm-create (width height color-def func)
  (fc--text ""
            (fc--xpm-header width height color-def)
            (cl-loop for x from 1 to height
                     collect (format "\"%s\",\n"
                                     (apply func x width height nil)))
            (fc--xpm-footer)))

(defun fc-make-xpm (width height color-def func)
  (create-image (fc--xpm-create width height color-def func)
                'xpm t
                :ascent 'center))

(provide 'fc-xpm)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-xpm.el ends here
