;;; elmk-singl.el --- install single files into an elmake site

;;; Commentary:
;; 
;; installs single files (like from gnu.emacs.sources) into current
;; elmake site.

;;; Code:

(require 'elmake)

(defconst elmake-single-file-place "/single-files")

;;;###autoload
(defun elmake-single-install ()
  "Install the currently loaded .el file."
  (interactive)
  (let ((fn (buffer-file-name)))
    (if (string-match "\\([^/]*\\)$" fn)
	(setq fn (match-string 0 fn)))
    (make-directory (concat elmake-base-dir elmake-single-file-place) t)
    (copy-file (buffer-file-name)
	       (concat elmake-base-dir elmake-single-file-place "/" fn) t)
    (elmake-single-do-it (list fn))))

;;;###autoload
(defun elmake-single-uninstall (file)
  "Uninstall an installed single file FILE."
  (save-excursion
    (with-current-buffer (find-file-noselect (concat elmake-base-dir
						     elmake-single-file-place
						     "/" file))
      (delete-region (point-min) (point-max))
      (save-buffer 0)
      (kill-buffer nil)))
    (elmake-single-do-it (list file) t))

(defun elmake-single-do-it (filelist &optional delete)
  "Register all files in FILELIST for elmake.
All these files have to be in the subdir `elmake-single-file-place' of
current elmake base dir.  If optional argument DELETE is non-nil,
delete the files after registering them.  This is used for
uninstalling single files."
  (let ((default-directory (concat elmake-base-dir elmake-single-file-place)))
    (add-to-list 'load-path default-directory)
    (save-excursion
      (with-current-buffer (get-buffer-create "*elMake*")
	(setq default-directory (concat elmake-base-dir
					elmake-single-file-place))))
    (setq elmake-project-name "single-file"
	  elmake-project-version "0"
	  elmake-project-target-alist nil
	  elmake-project-string-alist nil
	  elmake-project-filelist-alist nil)
    (aput 'elmake-project-filelist-alist
	  'elfile (list (concat "\\`" (regexp-opt filelist) "\\'")))
    (aput 'elmake-project-string-alist
	  'aloadfile "elmk-sf-al.el")
    (aput 'elmake-project-filelist-alist
	  'aloadfile (list "\\`elmk-sf-al\\.el\\'"))
    (aput 'elmake-project-target-alist
	  (elmake-parse-string "install")
	  '((update-autoloads aloadfile elfile)
	    (compile elfile aloadfile)
	    (register-require elmk-sf-al)))
    (elmake-run-target "install")
    (cond
     (delete
      (let ((fl filelist))
	(while fl
	  (delete-file (car fl))
	  (delete-file (concat (car fl) "c"))
	  (setq fl (cdr fl))))
      (message "File(s) successfully uninstalled."))
     (t (message "File(s) successfully installed.")))))

;;;###autoload
(defun elmake-single-install-buffer ()
  "Install an .el file from current buffer.
Use the file header and footer marks to get the file name."
  (interactive)
  ;; fixme: implement this
  nil)

(provide 'elmk-singl)

;;; elmk-singl.el ends here
