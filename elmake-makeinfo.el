;;; elmake-makeinfo.el --- makeinfo support for elMake


;;; Commentary:
;; 


;;; Code:
(require 'elmake)

(defvar elmake-makeinfo-executable "makeinfo")
(defvar elmake-makeinfo-use-texinfo-format-buffer nil)

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

(defun elmake-makeinfo-dest-file (filename)
  "Implement this!
FILENAME"
"non-existent.file")
;; to implement.


(provide 'elmake-makeinfo)

;;; elmake-makeinfo.el ends here
