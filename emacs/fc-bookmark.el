;;; fc-bookmark.el --- bookmark management -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(defvar *fc-bookmark-file* "~/.emacs.d/fbookmarks")

(defvar *fc-active-bookmarks* '((:name "bookmark")) "active bookmarks (tree)")
(defvar *fc-active-bookmarks-count* 0)
(defvar *fc-current-bookmark-buffer* nil "current bookmark buffer")
(defvar *fc-current-bookmark* nil "current bookmark")
(defvar *fc-last-bookmark-annotation* "")

(defvar *fc-bookmark-metadata* '((:name "bookmark")) "bookmark metadata (tree)")

(defface bm-face
  '((((class grayscale)
      (background light)) (:background "DimGray"))
    (((class grayscale)
      (background dark))  (:background "LightGray"))
    (((class color)
      (background light)) (:foreground "White" :background "DarkOrange1"))
    (((class color)
      (background dark))  (:foreground "Black" :background "DarkOrange1")))
  "Face used to highlight current line."
  :group 'bm)

(defface bm-persistent-face
  '((((class grayscale)
      (background light)) (:background "DimGray"))
    (((class grayscale)
      (background dark))  (:background "LightGray"))
    (((class color)
      (background light)) (:foreground "White" :background "DarkBlue"))
    (((class color)
      (background dark))  (:foreground "White" :background "DarkBlue")))
  "Face used to highlight current line if bookmark is persistent."
  :group 'bm)

(defface bm-annotation-face
  '((((class grayscale)
      (background light)) (:background "DimGray"))
    (((class grayscale)
      (background dark))  (:background "LightGray"))
    (((class color)
      (background light)) (:foreground "White" :background "DarkOrange1"))
    (((class color)
      (background dark))  (:foreground "Black" :background "DarkOrange1")))
  "Face used to highlight current line."
  :group 'bm)

(defun fc-jump-to-bookmark (ol)
  (switch-to-buffer (overlay-buffer ol))
  (goto-char (overlay-start ol))
  (fc-highlight-cursor))

(defun fc-active-bookmark-list ()
  (apply #'append
	 (-map #'cl-rest
	       (cl-rest *fc-active-bookmarks*))))

(cl-defun fc--next-error-function (n &optional reset)
  (forward-line n)

  (let ((raw (thing-at-point 'line t)))
    (when (< (length raw) 3)
      (cl-return-from fc--next-error-function))
    (let* ((data (s-split ":" raw t))
	   (file (cl-first data))
	   (pos (cl-second data)))

      (switch-to-buffer (find-buffer-visiting file))
      (goto-char (string-to-number pos))
      (fc-highlight-cursor))))

(defun fc-list-bookmark ()
  (interactive)

  (let ((buf (get-buffer-create "*bookmarks*"))
	(all (fc-active-bookmark-list)))
    (with-current-buffer buf
      (erase-buffer)
      (insert "\n")
      (--each all
	(insert (propertize (format "%s:%d: %s\n"
				    (buffer-file-name (overlay-buffer it))
				    (overlay-start it)
				    (overlay-get it 'fannotation))
			    'face (if (overlay-get it 'fpersist)
				      'bm-persistent-face
				    'bm-face))))
      (insert "\n")
      (goto-char (point-min))

      (setf next-error-function #'fc--next-error-function
	    next-error-move-function #'fc--next-error-move-function
	    next-error-last-buffer buf))

    (display-buffer buf)))

(defun fc-select-bookmark ()
  (interactive)

  (let* ((all (--map (cons (format "%s : %s"
				   (file-relative-name (buffer-file-name (overlay-buffer it)))
				   (overlay-get it 'fannotation))
			   it)
		     (fc-active-bookmark-list)))
	 (ol (if all
		 (fc-user-select "Select bookmark" all :fullscreen t)
	       nil)))
    (cond
     ((not all)
      (message "No bookmark !!!"))

     (ol
      (fc-jump-to-bookmark ol)))))

(cl-defun fc-find-bookmark (direction)
  (if (eq *fc-active-bookmarks-count* 0)
      (cl-return-from fc-find-bookmark nil))

  (unless *fc-current-bookmark-buffer*
    (setf *fc-current-bookmark-buffer* (car (cl-second *fc-active-bookmarks*))))

  (let* ((all (if (> direction 0) (fc-active-bookmark-list) (reverse (fc-active-bookmark-list))))
	 (head (car all)))
    (if (null *fc-current-bookmark*)
	head
      (let ((l (cdr (member *fc-current-bookmark* all))))
	(cond
	 ((null l) head)

	 ((eq head *fc-current-bookmark*)
	  (cl-second all))

	 (t head))))))

(cl-defun fc-find-bookmark (direction)
  (if (eq *fc-active-bookmarks-count* 0)
      (cl-return-from fc-find-bookmark nil))

  (let* ((all (if (> direction 0) (fc-active-bookmark-list) (reverse (fc-active-bookmark-list))))
	 (head (car all)))
    (cond
     ((null *fc-current-bookmark*)
      head)

     (t
      (let ((l (cdr (member *fc-current-bookmark* all))))
	(cond
	 ((null l) head)

	 ((eq head *fc-current-bookmark*)
	  (cl-second all))

	 (t head)))))))

(defun fc-next-bookmark ()
  (interactive)

  (setf *fc-current-bookmark* (fc-find-bookmark 1))

  (if *fc-current-bookmark*
      (fc-jump-to-bookmark *fc-current-bookmark*)
    (message "No bookmarks !")))

(defun fc-previous-bookmark ()
  (interactive)

  (setf *fc-current-bookmark* (fc-find-bookmark -1))

  (if *fc-current-bookmark*
      (fc-jump-to-bookmark *fc-current-bookmark*)
    (message "No bookmarks !")))

(defun fc-current-bookmark (beginning &optional end)
  (-first (lambda (x) (equal (overlay-get x 'category) 'fbm))
	  (if end
	      (overlays-in beginning end)
	    (overlays-at beginning))))

(defun fc-delete-bookmark (beginning &optional end)
  (let* ((x (fc-current-bookmark beginning end))
	 (filename buffer-file-name)
	 (x-list (cl-rest (atree-get *fc-active-bookmarks* filename))))
    (when x
      (setf x-list (delq x x-list))
      (atree-set *fc-active-bookmarks* x-list filename)

      (let ((m-list (cl-rest (atree-get *fc-bookmark-metadata* filename))))
	(setf m-list (delete (list (overlay-start x)
				   (overlay-end x)
				   (overlay-get x 'fannotation))
			     m-list))

	(atree-set *fc-bookmark-metadata*
		   m-list
		   filename))

      (cl-decf *fc-active-bookmarks-count*)
      (delete-overlay x))))

(defun fc--create-annotation (ol annotation)
  (let* ((width (string-width (buffer-substring
			       (line-beginning-position)
			       (line-end-position))))
	 (align (if (>= width 90) 4 (- 90 width))))
    (overlay-put ol
		 'after-string
		 (concat (propertize (concat (make-string align ? ) " ➠ ")
				     'cursor t)

			 (propertize annotation
				     'face 'bm-annotation-face
				     'cursor t)))))

(defun fc--add-bookmark (beginning end annotation persist &optional meta)
  (let* ((x (make-overlay beginning end))
	 (filename buffer-file-name)
	 (x-list (cl-rest (atree-get *fc-active-bookmarks* filename))))
    (overlay-put x 'face (if persist 'bm-persistent-face 'bm-face))
    (overlay-put x 'category 'fbm)
    (overlay-put x 'fpersist persist)
    (overlay-put x 'fannotation annotation)

    (fc--create-annotation x annotation)

    (setf x-list (if x-list
		     (sort (append x-list (list x))
			   (lambda (x y) (< (overlay-start x)
					    (overlay-start y))))
		   (list x)))

    (atree-set *fc-active-bookmarks* x-list filename)
    (when (and meta persist)
      (let ((m-list (cl-rest (atree-get *fc-bookmark-metadata* filename)))
	    (new (list (list beginning end annotation))))
	(setf m-list (if m-list (append m-list new) new))

	(atree-set *fc-bookmark-metadata*
		   m-list
		   filename)))

    (cl-incf *fc-active-bookmarks-count*)))

(defun fc--read-annotation (&optional default)
  (let ((a (string-trim (read-string "Annotation : " (or default *fc-last-bookmark-annotation*)))))
    (setf *fc-last-bookmark-annotation* a)
    a))

(defun fc-add-bookmark (beginning end)
  (let* ((annotation (fc--read-annotation)))
    (fc--add-bookmark beginning end annotation *fc-ergo-prefix* t)))

(cl-defun fc-toggle-bookmark ()
  (interactive)

  (unless buffer-file-name
    (message "Only support bookmark on normal file.")
    (cl-return-from fc-toggle-bookmark))

  (cond
   ((region-active-p)
    (fc-delete-bookmark (region-beginning) (region-end))
    (fc-add-bookmark (region-beginning) (region-end))
    (deactivate-mark))

   ((fc-current-bookmark (point))
    (fc-delete-bookmark (point)))

   (t
    (fc-add-bookmark (line-beginning-position) (line-end-position)))))

(defun fc-edit-bookmark-annotation ()
  (interactive)

  (let* ((x (fc-current-bookmark (point)))
	 (a (if x (fc--read-annotation) "")))
    (fc--create-annotation x a)))

(defun fc-restore-bookmark ()
  (let* ((meta (assoc buffer-file-name *fc-bookmark-metadata*)))
    (--each (cl-rest meta)
      (fc--add-bookmark (cl-first it)
			(cl-second it)
			(cl-third it)
			t))))

(defun fc-remove-active-bookmark ()
  (let ((data (atree-get *fc-active-bookmarks* buffer-file-name)))
    (when (and data
	       (< 1 (length data)))
      (atree-set *fc-active-bookmarks* nil buffer-file-name))))

(defun fc-save-bookmark ()
  (setf *fc-bookmark-metadata*
	(--filter (or (not (listp it))
		      (> (length it) 1))
		  *fc-bookmark-metadata*))
  (fc-serialize *fc-bookmark-file* *fc-bookmark-metadata*))

(defun fc-load-bookmark ()
  (setf *fc-bookmark-metadata*
	(car (fc-unserialize *fc-bookmark-file* '((:name "bookmark") nil)))))

(fc-load-bookmark)

(add-hook 'find-file-hook #'fc-restore-bookmark)
(add-hook 'kill-buffer-hook #'fc-remove-active-bookmark)
(add-hook 'kill-emacs-hook #'fc-save-bookmark)

(provide 'fc-bookmark)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; fc-bookmark.el ends here
