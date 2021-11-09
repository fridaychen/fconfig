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
                :scale 1
                :ascent 'center))

;; helper
(defun fc--create-xpm-data-with-pattern1 (height data)
  (cl-loop with data-len = (length data)
           for x from 0 to (1- height)
           collect (nth (% x data-len) data)))

(defun fc--create-xpm-data-with-pattern3 (height header center footer)
  (cond
   ((eq (length center) 1)
    (fc-concat
     header
     (fc--create-xpm-data-with-pattern1 (- height (length header) (length footer))
                                        center)
     footer))

   ((and (eq (length header) 1)
         (eq (length footer) 1))
    (let* ((center-count (/ height (length center)))
           (header-count (/ (- height (* center-count (length center))) 2))
           (footer-count (- height header-count (* center-count (length center)))))
      (fc-concat
       (fc--create-xpm-data-with-pattern1 header-count header)
       (fc--create-xpm-data-with-pattern1 (* center-count (length center)) center)
       (fc--create-xpm-data-with-pattern1 footer-count footer))))))

(defun fc--create-xpm-data-with-pattern5 (height header pattern center second-pattern footer)
  (let* ((pattern-len (/ (- height (length header) (length footer) (length center)) 2))
         (second-pattern-len (- height (length header) (length footer) (length center) pattern-len)))
    (fc-concat
     header
     (fc--create-xpm-data-with-pattern1 pattern-len pattern)
     center
     (fc--create-xpm-data-with-pattern1 second-pattern-len pattern)
     footer)))

(defun fc--create-xpm-data-with-pattern (height pattern)
  (cond
   ((= (length pattern) 1)
    (fc--create-xpm-data-with-pattern1 height
                                       (cl-first pattern)))

   ((= (length pattern) 3)
    (fc--create-xpm-data-with-pattern3 height
                                       (cl-first pattern)
                                       (cl-second pattern)
                                       (cl-third pattern)))

   ((= (length pattern) 5)
    (fc--create-xpm-data-with-pattern5 height
                                       (cl-first pattern)
                                       (cl-second pattern)
                                       (cl-third pattern)
                                       (cl-fourth pattern)
                                       (cl-fifth pattern)))))

(defun fc-make-xpm-with-pattern (height color-def pattern &optional reverse)
  (let* ((orig-data (fc--create-xpm-data-with-pattern height pattern))
         (data (if reverse (mapcar #'reverse orig-data) orig-data)))
    (fc-make-xpm (length (caar pattern)) height color-def
                 (lambda (line &rest _rest)
                   (nth (1- line) data)))))

(defun fc-make-xpm-with-gradient (width height color1 color2 &optional reverse)
  (let* ((chars (mapcar 'char-to-string (fc-concat (number-sequence ?0 ?9)
                                                   (number-sequence ?a ?z)
                                                   (number-sequence ?A ?Z))))
         (colors (mapcar #'(lambda (rgb) (apply #'color-rgb-to-hex rgb))
                         (color-gradient (color-name-to-rgb color1)
                                         (color-name-to-rgb color2)
                                         width)))
         (color-def (cl-mapcar #'cons chars colors))
         (orig-data (substring (apply #'fc-concat chars) 0 width))
         (data (if reverse (reverse orig-data) orig-data)))
    (fc-make-xpm width height color-def
                 (lambda (&rest _rest)
                   data))))

(provide 'fc-xpm)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-xpm.el ends here
