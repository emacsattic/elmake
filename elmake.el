;;; elmake.el --- a "make" for emacs lisp projects in pure emacs lisp

(defvar elmake-version "0.0")

;;; Commentary:
;; elmake is GPLed

;;; Code:
(defvar elmake-base-dir nil
  "The directory where elmake is installed to.")
(defvar elmake-info-dir nil)

(defvar elmake-project-name nil)
(defvar elmake-project-version nil)
(defvar elmake-project-target-alist nil)
(defvar elmake-project-string-alist nil)
(defvar elmake-project-filelist-alist nil)
(defvar elmake-copy-source t)

(defun elmake-load-elmakefile (buf)
  "Load an elMakefile from buffer BUF.
The data is stored into the elmake-project-* variables."
  (setq elmake-project-name nil
	elmake-project-version nil
	elmake-project-target-alist nil
	elmake-project-string-alist nil
	elmake-project-filelist-alist nil)
  (let ((mybuf (get-buffer "*elMake*"))
	(buff (current-buffer))
	makedir)
    (when mybuf
      (set-buffer buf)
      (setq makedir default-directory)
      (set-buffer mybuf)
      (setq default-directory makedir)))
  (let* ((mfile (read buf)) first cfirst)
      (unless (eq (car (car mfile)) 'elmakefile)
	(error "File is no valid elmakefile (signature missing"))
      (setq elmake-project-name (elmake-parse-string (car (cdr (car mfile))))
	    elmake-project-version (elmake-parse-string
				    (car (cdr (cdr (car mfile)))))
	    mfile (cdr mfile))
      (while mfile
	  (setq first (car mfile)
		mfile (cdr mfile)
		cfirst (car first))
	  (cond
	   ((eq cfirst 'target)
	    (aput 'elmake-project-target-alist
		  (elmake-parse-string (car (cdr first)))
		  (cdr (cdr first))))
	   ((eq cfirst 'string)
	    (let ((key (car (cdr first)))
		  (value (elmake-parse-string (car (cdr (cdr first))))))
	    (unless (aget elmake-project-string-alist key)
	      (aput 'elmake-project-string-alist key value))))
	   ((eq cfirst 'filelist)
	    (aput 'elmake-project-filelist-alist
		  (car (cdr first))
		  (cdr (cdr first))))
	    (t (error "Cannot parse : %s" first)))))
  (aput 'elmake-project-string-alist
	'targetdir (concat elmake-base-dir "/"
			   elmake-project-name "-"
			   elmake-project-version))
  (aput 'elmake-project-string-alist
	'basedir elmake-base-dir)
  (aput 'elmake-project-string-alist
	'infodir elmake-info-dir))
	    
  
(defun elmake-parse-string (s)
  "Parse a string S.
Allowed values are string literals, symbols (they
are looked up from `elmake-project-string-alist') and (concat ...)
forms (to concatenate all parameters which are parsed by this function
as well) and (eval . func) forms that evaluate the given function and
assign the result."
  (cond
   ((not s) s)
   ((symbolp s)
    (elmake-parse-string (aget elmake-project-string-alist s)))
   ((and (listp s) (eq (car s) 'concat))
    (mapconcat 'elmake-parse-string (cdr s) ""))
   ((and (listp s) (eq (car s) 'eval))
    (elmake-parse-string (eval (cdr s))))
   (t s)))


(defun elmake-parse-filelist-entry (elem)
  "Parse ELEM, an entry of a (filelist ...) clause.
Returns a list of filenames.  This is either a regex of files to be
matched, or a form (remove FILES . EXCLUDE) or (combine FILES ...)
consisting of such filelist entries.  A symbol is treated as a filelist
already defined and stored in `elmake-project-filelist-alist'."
  (cond
   ((not elem) elem)
   ((and (listp elem) (eq (car elem) 'combine))
	 (elmake-parse-filelist-entry (car (cdr elem)))) ;; to implement...
   ((and (listp elem) (eq (car elem) 'remove))
    (let ((inc (elmake-parse-filelist-entry (car (cdr elem))))
	  (exc (elmake-parse-filelist-entry (cdr (cdr elem)))))
      (while exc
	(setq inc (remove (car exc) inc)
	      exc (cdr exc)))
      inc))
   ((and (symbolp elem) (aget elmake-project-filelist-alist elem))
    (elmake-parse-filelist (aget elmake-project-filelist-alist elem)))
   (t
    (directory-files "." nil (elmake-parse-string elem)))))

(defun elmake-parse-filelist (fl)
"Parse a file list clause FL.  This is done by parsing all entries separately and concatenating the resulting lists."
(let ((val fl) (result nil))
  (while val
    (setq result (nconc result (elmake-parse-filelist-entry (car val)))
	  val (cdr val)))
  result))


(defun elmake-run-target (target)
  "Execute a given TARGET from the loaded elMakefile."
  (switch-to-buffer (get-buffer-create "*elMake*"))
  (set-buffer (get-buffer-create "*elMake*"))
  (setq truncate-lines t)
  (goto-char (point-max))
  (insert (format "\n\n===== running target %s =====\n" target))
  (setq elmake-targets-done nil)
  (elmake-run-target-0 target 0)
  (elmake-log 0 "===== Done =====\n\n")
  (recenter -2))

(defun elmake-indent (level)
  "Produce indentation spaces according to current nesting LEVEL."
  (if (= level 0) ""
    (concat (elmake-indent (1- level)) "   ")))

(defun elmake-log (level string)
  "Log a string into the log buffer.
LEVEL specifies how many spaces to add before it, STRING the string to
add."
  (set-buffer (get-buffer-create "*elMake*"))
  (insert (concat (elmake-indent level) string "\n"))
  (sit-for 0))

(defun elmake-run-target-0 (targetname level)
  "Internal function to run a target TARGETNAME at level LEVEL."
  (elmake-log level (format "--- Running %s" targetname))
  (add-to-list 'elmake-targets-done targetname)
  (let ((actions (aget elmake-project-target-alist targetname)))
    (unless actions
      (error "Unknown target %s" targetname))
    (while actions
      (elmake-run-action (car actions) level)
      (setq actions (cdr actions))))
  (elmake-log level (format "--- Finished %s" targetname)))


(defun elmake-run-action (action level)
  "Run one ACTION.
LEVEL specifies the nesting level of the target running this action."
  (unless (listp action)
    (error "Unknown action: %s" action))
  (cond
   ((eq (car action) 'depends) ;;; depends ;;;
    (let ((params (cdr action)))
      (while params
	(unless (member (car params) elmake-targets-done)
	  (elmake-run-target-0 (car params) (1+ level)))
	(setq params (cdr params)))))
   ((eq (car action) 'message) ;;; message ;;;
    (elmake-log level (format "+++ %s +++" ""
		    (elmake-parse-string (car (cdr action))))))
   ((eq (car action) 'mkdir) ;;; mkdir ;;;
    (unless (file-directory-p (elmake-parse-string (car (cdr action))))
      (make-directory (elmake-parse-string (car (cdr action))) t)))
   ((eq (car action) 'chdir) ;;; chdir ;;;
    (set-buffer (get-buffer-create "*elMake*"))
    (when (file-directory-p (elmake-parse-string (car (cdr action))))
      (cd (elmake-parse-string (car (cdr action)))) t))
   ((eq (car action) 'rmdir) ;;; rmdir ;;;
    (condition-case nil
	(delete-directory (elmake-parse-string (car (cdr action))))
      (error nil)))
   ((eq (car action) 'compile) ;;; compile ;;;
    (let ((flist (elmake-parse-filelist (cdr action))))
      (while flist
	(when (file-newer-than-file-p (car flist) (concat (car flist) "c"))
	  (elmake-log level (format "   byte compiling %s"
			  (car flist)))
	  (unless (byte-compile-file (car flist))
	    (elmake-log level (format "*** error compiling %s"))))
	(setq flist (cdr flist)))))
   ((eq (car action) 'makeinfo) ;;; makeinfo ;;;
    (let ((flist (elmake-parse-filelist (cdr action))))
      (require 'elmake-makeinfo)
      (while flist
	(when (file-newer-than-file-p (car flist) (elmake-makeinfo-dest-file (car flist)))
	  (elmake-log level (format "   making info for %s"
			  (car flist)))
	  (unless (elmake-makeinfo (car flist))
	    (elmake-log level (format "*** error-making info for %s"))))
	(setq flist (cdr flist)))))
   ((or (eq (car action) 'copy) ;;; copy ;;;
	(and (eq (car action) 'copy-source) ;;; copy-source ;;;
	     elmake-copy-source))
    (let ((dest (elmake-parse-string (car (cdr action))))
	  (flist (elmake-parse-filelist (cdr (cdr action)))))
      (while flist
	(when (file-newer-than-file-p (car flist)
				      (concat dest "/" (car flist) ))
	  (elmake-log level (format "   copying %s to %s"
			  (car flist) dest))
	  (copy-file (car flist) (concat dest "/" (car flist)) t t))
	(setq flist (cdr flist)))))
   ((eq (car action) 'copy-source) ;;; copy-source when disabled ;;;
    nil)
   ((eq (car action) 'delete) ;;; delete;;;
    (let ((dest (elmake-parse-string (car (cdr action))))
	  (flist (elmake-parse-filelist (cdr (cdr action)))))
      (while flist
	(when (file-exists-p (concat dest "/" (car flist)))
	  (elmake-log level (format "   deleting %s"
			  (car flist)))
	  (delete-file(concat dest "/" (car flist))))
	(setq flist (cdr flist)))))
   ((eq (car action) 'register-require)  ;;; register-require ;;;
    (add-to-list 'elmake-require-list (car (cdr action)))
    (elmake-save-database)
    (elmake-require (car (cdr action))))
   ((eq (car action) 'unregister-require)  ;;; unregister-require ;;;
    (setq elmake-require-list (remove (car (cdr action))
				      elmake-require-list))
    (elmake-save-database))
   ((eq (car action) 'register-installed)  ;;; register-installed ;;;
    (aput 'elmake-installed-alist elmake-project-name
	  elmake-project-version)
    (elmake-save-database))
   ((eq (car action) 'register-uninstalled)  ;;; unregister-installed ;;;
    (adelete 'elmake-installed-alist elmake-project-name)
    (elmake-save-database))
   (t (error "Unknown action: %s" action))))
 

(defun elmake-install (arg)
  "Install the elMakefile in current buffer.
With an argument ARG, ask for the target - otherwise check if this
elMakefile has already be installed and run the \"uninstall\" target in
that case.  Otherwise run the \"install\" target.  If an older version has
been installed, uninstall that one first."
  (interactive "P")
  (let* ((dir (buffer-file-name))
	 (match (string-match "\\(.*\\)/" dir))
	 (substr (match-string-no-properties 1 dir)))
    (cd substr))
  (goto-char (point-min))
  (elmake-load-elmakefile (current-buffer))
  (let ((target nil))
    (cond
     (arg (setq target (read-string "Target: ")))
     ((not (aget elmake-installed-alist elmake-project-name))
      (setq target "install"))
     ((string= elmake-project-version
	       (aget elmake-installed-alist elmake-project-name))
	(setq target "uninstall"))
     (t (setq target "<>")))
    (when target
      (when (string= target "<>")
	(elmake-uninstall-old-version elmake-project-name)
	(setq target "install"))
      (elmake-run-target target)
      (message "Target `%s' successful." target))))


(defun elmake-uninstall-old-version (name)
  "Uninstalls the installed version of NAME.
This loads the makefile of this program and runs the \"uninstall\"
target from it."
  (let ((oldbuf (current-buffer)) buf)
    (save-excursion
      (setq buf (find-file (concat elmake-base-dir "/" name "-"
				   (aget elmake-installed-alist name)
				   "/elMakefile")))
      (switch-to-buffer buf)
      (goto-char (point-min))
      (elmake-load-elmakefile buf)
      (kill-buffer buf)
      (elmake-run-target "uninstall"))
    (switch-to-buffer oldbuf)
    (goto-char (point-min))
    (elmake-load-elmakefile (current-buffer))))

(provide 'elmake)

;;; elmake.el ends here
