;;; elmk-site.el --- manage more than one elmake "site"
;; $Id$

;;; Commentary:
;; 

;;; History:
;; 

;;; Code:

(require 'elmake)

;;;###autoload
(defun elmake-site-rename (newname)
  "Rename current elMake site to NEWNAME.
When called interactively, prompt for the new name."
  (interactive "MNew name: ")
  (elmake-test-site)
  (if (aget elmake-site-alist newname)
      (error "There is already a site named `%s'" newname))
  (let ((oldname elmake-site-name) (sitelist elmake-site-alist))
    (setq elmake-site-name newname)
    (setq elmake-site-alist (mapcar (lambda (x)
				      (if (string= (car x) oldname)
					  (cons newname (cdr x))
					x))
				    elmake-site-alist)))
  (elmake-save-database))

;;;###autoload
(defun elmake-site-add (&optional sitename basedir infodir registerto
				  initfunc)
  "Create a new elMake site and add its load code to a file.
All arguments are optional.  You are prompted for non-specified
values.  SITENAME specifies the name of the new site, BASEDIR is the
dir where elisp files go to, INFODIR is the dir for info files,
REGISTERTO is a file name of an existing file where to put the load
code into.  If INITFUNC is non-nil, it is called instead of copying
the necessary information from an old site.  This is used for the
first creation of a site."
  (interactive)
  (while (not sitename)
    (setq sitename (read-string "Site name: " nil))
    (if (or (string= sitename "") (aget elmake-site-alist sitename))
	(setq sitename nil)))
  (while (not basedir)
    (setq basedir (read-file-name "Base dir (for .el(c) files): "
				  "~/" nil nil "elmake-lisp"))
    
    (elmake-site-init-dir basedir))
  (while (not infodir)
    (setq infodir (read-file-name "Info dir: "
				  (elmake-directory-name basedir t)
				  nil nil "elmake-info"))
    (elmake-site-init-dir infodir))
  (unless registerto
    (setq registerto (read-file-name "Where to add load code: " "~/" nil t
				     ".emacs")))
  (let ((oldbase elmake-base-dir))
    (setq elmake-base-dir basedir
	  elmake-info-dir infodir
	  elmake-site-name sitename
	  elmake-installed-alist nil
	  elmake-require-list nil)
    (unless oldbase ;; this is the first project
      (let ((pth (locate-library "elmk-init.el" t)))
	(unless pth
	  (error "Cannot find elmk-init.el"))
	(setq oldbase (elmake-directory-name pth))))
    (if initfunc
	(funcall initfunc)
      (copy-file (concat oldbase "/elmk-init.el")
		 (concat basedir "/elmk-init.el") t)
      (copy-file (concat oldbase "/elmk-init.elc")
		 (concat basedir "/elmk-init.elc") t)
      (elmake-save-database)))
  ;; load the new database
  (load (concat elmake-base-dir "/elmake-db"))
  ;; register this site
  (save-excursion
    (with-current-buffer (find-file-noselect registerto)
      (goto-char (point-max))
      (insert "
;; added by elmake-site-add
\(load \"" elmake-base-dir "/elmake-db\")
;; end added by elmake-site-add
")
      (save-buffer)
      (kill-buffer nil))))

(defmacro elmake-site-init-dir (dir)
  "Check and set up directory DIR."
  `(progn
     (when (string-match "/$" ,dir)
       (setq ,dir (substring ,dir 0 -1)))
     (unless (file-directory-p ,dir)
       (condition-case nil
	   (make-directory ,dir t)
	 (error nil)))))

;;;###autoload
(defun elmake-site-select (newsite)
  "Select NEWSITE for future elmake commands (like rename, install...).
When called interactively, prompt for the new site name."
  (interactive
   (list (completing-read "Select: " elmake-site-alist nil t)))
  (let ((sinfo (aget elmake-site-alist newsite)))
    (unless sinfo
      (error "Site does not exist"))
    (load (concat (car sinfo) "/elmake-db"))))

(provide 'elmk-site)

;;; elmk-site.el ends here
