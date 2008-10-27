;;;
;;; find-file-in-project.el
;;;
;;; Build cache of files in a project for quick switching
;;;

;;; find-file-in-project

(defvar find-file-in-project-version "0.4")

(defvar project-root nil
	"Place holder for where to begin loading `project-files-table'
	with `project-files' for use with `find-file-in-project'")

(defvar project-files-table ()
	"alist of filenames and their path for use with `find-file-in-project'")

(defvar find-file-in-project-excludes nil
	"a list of patterns against which directories are compared as the `project-files-table' is built.  Matches to one of the patterns are excluded from the `project-files-table'")

(defvar find-file-in-project-file-excludes "~$")

(defvar find-file-in-project-cache-results nil
	"boolean variable that triggers caching of the `project-files-table'.  Setting this to true means `find-file-in-project' won't find new files until `find-file-in-project-rebuild-cache' is run.")

(defun find-file-in-project (file)
	"Using an alist of all the files in a project, select one of them to find"
  (interactive (list (ido-completing-read "Find file in project: " (mapcar 'car (project-files)))))
  (find-file (cdr (assoc file project-files-table))))

(defun find-file-in-project-other-window (file)
	"Using an alist of all the files in a project, select one of them to find in `other-window'"
  (interactive (list (ido-completing-read "Find file in project: " (mapcar 'car (project-files)))))
  (find-file-other-window (cdr (assoc file project-files-table))))

(defun find-file-in-project-rebuild-cache()
	(interactive)
	(setq project-files-table nil))

(defun set-project-root(dir)
	"Sets the `project-root' for use with `find-file-in-project'"
	(interactive "DSet project root: ")
	(setq project-root dir))

(defun clear-project-root(clear)
	"Clears the `project-root' for `find-file-in-project' so that
	it will go back to trying to guess the project root"
	(interactive (list (and (y-or-n-p (format "Clear the project root (%s)? " project-root))(message ""))))
	(if clear
			(setq project-root nil)))

(defun populate-project-files-table (file)
  (if (file-directory-p file)
      (let* ((pattern (ffip-simple-regexp-opts find-file-in-project-excludes))
						 (exclude (if pattern (string-match pattern file))))
				(unless exclude (mapc 'populate-project-files-table (directory-files file t "^[^\.]"))))
    (let* ((file-name (file-name-nondirectory file))
					 (existing-record (assoc file-name project-files-table))
					 (unique-parts (get-unique-directory-names file (cdr existing-record))))
      (unless (string-match "^$" file-name)
				(if existing-record
					(let ((new-key (concat file-name " - " (car unique-parts)))
								(old-key (concat (car existing-record) " - " (cadr unique-parts))))
						(setf (car existing-record) old-key)
						(setq project-files-table (ffip-aappend new-key file project-files-table)))
				(setq project-files-table (ffip-aappend file-name file project-files-table)))))))

(defun project-files ()
	"Populates the `project-files-table' and sorts it according to
`buffer-list' for use with `find-file-in-project'"
  (unless find-file-in-project-cache-results (setq project-files-table nil))
	(unless project-files-table (populate-project-files-table (rails-root)))
	(let ((subtracted-alist project-files-table))
		(delete nil 
						(append 
						 (mapcar 
							(lambda(elem) 
								(setq subtracted-alist (rassq-delete-all elem subtracted-alist))
								(rassoc elem project-files-table)) (buffer-file-name-list))
						 subtracted-alist))))

(defun buffer-file-name-list()
  (cdr (mapcar (lambda(buffer) (buffer-file-name buffer)) (buffer-list))))

(defun get-unique-directory-names (path1 path2)
  (let* ((parts1 (and path1 (split-string path1 "/" t)))
	 (parts2 (and path2 (split-string path2 "/" t)))
	 (part1 (pop parts1))
	 (part2 (pop parts2))
	 (looping t))
    (while (and part1 part2 looping)
	   (if (equal part1 part2)
	       (setq part1 (pop parts1) part2 (pop parts2))
	     (setq looping nil)))
    (list part1 part2)))

(defun ffip-simple-regexp-opts(patterns)
	(let ((tmp-pattern))
		(mapc '(lambda (pattern)
						 (setq tmp-pattern (concat pattern (if tmp-pattern "\\|") tmp-pattern))) patterns)
		tmp-pattern))

(defun ffip-aappend (key value alist)
	(setq alist (nconc (list (cons key value)) alist)))

(defun rails-root (&optional dir)
	(if project-root
			project-root
		(or dir (setq dir default-directory))
		(if (file-exists-p (concat dir "config/environment.rb"))
				dir
			(if (equal dir  "/")
					nil
				(rails-root (expand-file-name (concat dir "../")))))))


(provide 'find-file-in-project)
