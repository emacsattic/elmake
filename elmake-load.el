;;; elmake-load.el --- initializing elmake and projects installed by it
;; 


;;; Commentary:
;; 

;;; Code:


;; we need these vars here as well.
(defvar elmake-installed-alist nil)
(defvar elmake-require-list nil)


(defun elmake-internal-adjust-load-path ()
  "Used to adjust the load path to add the elmake dir and all subdirs.
Shamelessly ripped from startup.el (Emacs 21)"
(let (dirs
	attrs
	(pending (list default-directory)))
    ;; This loop does a breadth-first tree walk on DIR's subtree,
    ;; putting each subdir into DIRS as its contents are examined.
    (while pending
      (setq dirs (cons (car pending) dirs))
      (setq pending (cdr pending))
      (let* ((this-dir (car dirs))
	     (contents (directory-files this-dir))
	     (default-directory this-dir)
	     (canonicalized (and (eq system-type 'windows-nt)
				 (untranslated-canonical-name this-dir))))
	;; The Windows version doesn't report meaningful inode
	;; numbers, so use the canonicalized absolute file name of the
	;; directory instead.
	(setq attrs (or canonicalized
			(nthcdr 10 (file-attributes this-dir))))
	(unless (member attrs normal-top-level-add-subdirs-inode-list)
	  (setq normal-top-level-add-subdirs-inode-list
		(cons attrs normal-top-level-add-subdirs-inode-list))
	  (while contents
	    ;; The lower-case variants of RCS and CVS are for DOS/Windows.
	    (unless (member (car contents) '("." ".." "RCS" "CVS" "rcs" "cvs"))
	      (when (and (string-match "\\`[[:alnum:]]" (car contents))
			 ;; Avoid doing a `stat' when it isn't necessary
			 ;; because that can cause trouble when an NFS server
			 ;; is down.
			 (not (string-match "\\.elc?\\'" (car contents)))
			 (file-directory-p (car contents)))
		(let ((expanded (expand-file-name (car contents))))
		  (unless (file-exists-p (expand-file-name ".nosearch"
							   expanded))
		    (setq pending (nconc pending (list expanded)))))))
	    (setq contents (cdr contents))))))
    (cdr (nreverse dirs))))

(defun elmake-initialize-load-path ()
  "Add elmake dir and subdirs to load path."
(add-to-list 'load-path elmake-base-dir)
  (let ((default-directory elmake-base-dir))
    (setq load-path (nconc (elmake-internal-adjust-load-path) load-path))))


(defun elmake-initialize-database ()
  "Load information about installed projects from \"elmake-db\"."
  (let ((oldbuf (current-buffer)) buf)
    (save-excursion
      (setq buf (find-file-noselect (concat elmake-base-dir "/elmake-db")))
      (set-buffer buf)
      (goto-char (point-min))
      (setq  elmake-installed-alist
	     (condition-case nil
		 (read buf)
	     (error nil)))
      (setq  elmake-require-list
	     (condition-case nil
		 (read buf)
	     (error nil)))
      (kill-buffer buf))
  ;  (switch-to-buffer oldbuf)
))
	      

(defun elmake-save-database ()
  "Save information about installed projects into \"elmake-db\"."
  (let ((oldbuf (current-buffer)) buf)
    (save-excursion
      (setq buf (find-file-noselect (concat elmake-base-dir "/elmake-db")))
      (set-buffer buf)
      (delete-region (point-min) (point-max))
      (insert (prin1-to-string elmake-installed-alist))
      (insert "\n")
      (insert (prin1-to-string elmake-require-list))
      (save-buffer buf)
      (kill-buffer buf))
  ;  (switch-to-buffer oldbuf)
    )
  (message "Saved elmake database."))

(defun elmake-load-needed-data ()
  "Load all requirements listed in the elmake database."
  (message "-- elmake requires: %S" elmake-require-list)
  (let ((l elmake-require-list))
    (while l
      (elmake-require (car l))
      (setq l (cdr l)))))

(defun elmake-require (feature)
  "Require a FEATURE or execute some Lisp code."
  (if (listp feature)
      (eval feature)
    (require feature)))

;; and now run all those things...
(elmake-initialize-load-path)
(elmake-initialize-database)
(elmake-load-needed-data)

(provide 'elmake-load)

;;; elmake-load.el ends here