;;; elmk-info.el --- makeinfo support for elMake


;;; Commentary:
;; 


;;; Code:
(require 'elmake)

(defvar elmake-makeinfo-executable "makeinfo")
(defvar elmake-makeinfo-use-texinfo-format-buffer nil)

;;;###autoload
(defun elmake-makeinfo (filename &optional indir)
  "Create an info file from FILENAME in INDIR."
  (elmake-log 0 ">>> Running makeinfo...")
  (set-buffer (get-buffer-create "*elMake*"))
  (if indir (cd indir))
  (let (res problem)
    (setq res
	  (condition-case problem
	      (call-process elmake-makeinfo-executable nil t t filename)
	    (error (message "Message: %S" problem) nil)))
    (cond
     ((and (not res) elmake-makeinfo-use-texinfo-format-buffer)
      (elmake-makeinfo-emacs filename indir))
     ((not res)
      (elmake-log 0 ">>> makeinfo not found - texinfo-format-buffer disabled")
      (elmake-log 0 "    either enable it or live without info files"))
     ((and (integerp res) (= res 0))
      (elmake-log 0 ">>> makeinfo successful.")
      t)
     (t
      (elmake-log ">>> makeinfo failed (reason: %S)." res)
      nil))))

(defun elmake-makeinfo-emacs (filename indir)
  "Create an info file from FILENAME in INDIR using `texinfo-format-buffer'."
  (elmake-log 0 ">>> makeinfo not found, using texinfo-format-buffer;")
  (elmake-log 0 "    that one is slow, please wait...")
  ;; for the cd:
  (set-buffer (get-buffer-create "*elMake*"))
  (if indir (cd indir))
  (let ((buf (find-file-noselect filename)))
    (set-buffer buf)
    (texinfo-format-buffer)
    (message "Buf: %S" (current-buffer))
    (save-buffer))
  (elmake-log 0 ">>> texinfo-format-buffer successful.")
  (set-buffer (get-buffer-create "*elMake*"))
  t)

;;;###autoload
(defun elmake-makeinfo-dest-file (filename)
  "Implement this!
FILENAME"
  ;; ripped from makeinfo.el
  (let (makeinfo-output-file-name)
    (save-excursion
      (with-current-buffer (find-file-noselect filename)
	(goto-char (point-min))
	(let ((search-end (save-excursion (forward-line 100) (point))))
	  (if (re-search-forward 
	       "^@setfilename[ \t]+\\([^ \t\n]+\\)[ \t]*"
	       search-end t)
	      (setq makeinfo-output-file-name 
		    (buffer-substring (match-beginning 1) (match-end 1)))
;	    (setq makeinfo-output-file-name "non-existent.file")
	    (error
	     "The texinfo file needs a line saying: @setfilename <name>")
;
))
      (kill-buffer nil)))
    makeinfo-output-file-name))

(provide 'elmk-info)

;;; elmk-info.el ends here
