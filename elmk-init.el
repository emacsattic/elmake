;;; elmk-init.el --- initializing elmake and projects installed by it
;; $Id$

;;; Commentary:

;; contains code that is needed at every emacs startup.
;;
;; this file does not need autoloads, as it is loaded at startup anyway
;;
;; insert contents of this file into elmake-db.el?


;;; History:
;; 

;;; Code:

(if (not (functionp 'when))
    (require 'cl))   ;; when, unless

(defvar elmake-init-before-hook nil)
(defvar elmake-init-after-hook nil)
(defvar elmake-site-alist nil)
(defvar normal-top-level-add-subdirs-inode-list nil)
(defvar elmake-other-base-dir nil)
(defvar elmake-other-info-dir nil)

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

(defun elmake-initialize-info-path ()
  "Add elmake info dir to info path."
  (unless (member elmake-info-dir Info-default-directory-list)
    (setq Info-default-directory-list
	  (append Info-default-directory-list (list elmake-info-dir)))))

(defun elmake-register-site ()
  "Register this site into `elmake-site-alist'."
(add-to-list 'elmake-site-alist (list elmake-site-name elmake-base-dir elmake-info-dir)))

(defun elmake-init ()
  "Initialize an elmake base dir."
  (run-hooks 'elmake-init-before-hook)
  (elmake-register-site)
  (elmake-initialize-load-path)
  (elmake-initialize-info-path)
  (run-hooks 'elmake-init-after-hook))

(defun elmake-init-before-hook-other-path ()
  "Example `elmake-init-before-hook'.
Use this if the path saved in your elMake site and the real path
differ."
  (if elmake-other-base-dir
      (setq elmake-base-dir elmake-other-base-dir
	    elmake-info-dir elmake-other-info-dir
	    elmake-other-base-dir nil)))

(provide 'elmk-init)

;;; elmk-init.el ends here