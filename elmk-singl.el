;;; elmk-singl.el --- install single files into an elmake site

;;; Commentary:
;; 
;; installs single files (like from gnu.emacs.sources) into current
;; elmake site.

;;; Code:

(require 'elmake)

;;;###autoload
(defun elmake-single-install ()
  "Install the currently loaded .el file."
  (interactive)
  (let ((fn (buffer-file-name)))
    (if (string-match "\\([^/]*\\)$" fn)
	(setq fn (match-string 0 fn)))
    (make-directory (concat elmake-base-dir "/single-files") t)
    (copy-file (buffer-file-name)
	       (concat elmake-base-dir "/single-files/" fn) t)
    (elmake-single-do-it (list fn))))

(defun elmake-single-do-it (filelist)
  "Register all files in FILELIST for elmake.
All these files have to be in the subdir `/single-files' of current
elmake base dir."
  (let ((default-directory (concat elmake-base-dir "/single-files")))
    (add-to-list 'load-path default-directory)
    (save-excursion
      (with-current-buffer (get-buffer-create "*elMake*")
	(setq default-directory (concat elmake-base-dir "/single-files"))))
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
    (message "File(s) successfully installed")))

;;;###autoload
(defun elmake-single-install-buffer ()
  "Install an .el file from current buffer.
Use the file header and footer marks to get the file name."
  (interactive)
  ;; fixme: implement this
  nil)

(provide 'elmk-singl)

;;; elmk-singl.el ends here
