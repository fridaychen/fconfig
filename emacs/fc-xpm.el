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

;; helper
(defun fc--create-xpm-data-with-pattern1 (height data)
  (cl-loop with data-len = (length data)
           for x from 0 to (1- height)
           collect (nth (% x data-len) data)))

(defun fc--create-xpm-data-with-pattern3 (height header center footer)
  (seq-concatenate 'list
                   header
                   (fc--create-xpm-data-with-pattern1 (- height (length header) (length footer))
                                                      center)
                   footer))

(defun fc--create-xpm-data-with-pattern (height pattern)
  (cond
   ((= (length pattern) 1)
    (fc--create-xpm-data-with-pattern1 height
                                       (cl-first pattern)))

   ((= (length pattern) 3)
    (fc--create-xpm-data-with-pattern3 height
                                       (cl-first pattern)
                                       (cl-second pattern)
                                       (cl-third pattern)))))

(defun fc-make-xpm-with-pattern (width height color-def pattern)
  (let ((data (fc--create-xpm-data-with-pattern height pattern)))
    (fc-make-xpm width height color-def
                 (lambda (line &rest _rest)
                   (nth (1- line) data)))))

(provide 'fc-xpm)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-xpm.el ends here
