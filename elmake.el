;;; elmake.el --- a "make" for emacs lisp projects in pure emacs lisp

;; Copyright (C) 2003 Michael Schierl

;; Author: Michael Schierl <schierlm-public@gmx.de>
;; Keywords: makefile install elisp

;; Version: 0.0

(defvar elmake-version "0.0"
  "Version of elMake.")

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  if not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307 USA

;;; Commentary:

;;; Code:

(require 'assoc)

(defvar elmake-base-dir nil
  "The directory where elmake is installed to.")
(defvar elmake-info-dir nil)
(defvar elmake-site-name nil)

(defvar elmake-project-name nil)
(defvar elmake-project-version nil)
(defvar elmake-project-target-alist nil)
(defvar elmake-project-string-alist nil)
(defvar elmake-project-filelist-alist nil)
(defvar elmake-targets-done nil)
(defvar elmake-copy-source t)
(defvar elmake-installed-alist nil)
(defvar elmake-require-list nil)

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
	(error "File is no valid elmakefile (signature missing)"))
      (setq elmake-project-name (elmake-parse-string (car (cdr (car mfile))))
	    elmake-project-version (elmake-parse-string
				    (car (cdr (cdr (car mfile)))))
	    mfile (cdr mfile))
      (aput 'elmake-project-string-alist
	    'targetdir (concat elmake-base-dir "/"
			       elmake-project-name "-"
			       elmake-project-version))
      (aput 'elmake-project-string-alist
	    'basedir elmake-base-dir)
      (aput 'elmake-project-string-alist
	    'infodir elmake-info-dir)
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
	    (t (error "Cannot parse : %s" first))))))
	    
  
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
  (let ((actions (aget elmake-project-target-alist targetname))
	(load-path (cons (aget elmake-project-string-alist "targetdir")  
			 (cons "." load-path))))
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
    (elmake-log level (format "+++ %s +++"
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
;      (require 'elmake-makeinfo)
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
   ((eq (car action) 'register-include)  ;;; register-include ;;;
    (add-to-list 'elmake-require-list (car (cdr action))) ;; to be changed
    (elmake-save-database)
    (elmake-require (car (cdr action))))
   ((eq (car action) 'unregister-include)  ;;; unregister-include ;;;
    (setq elmake-require-list (remove (car (cdr action)) ;; to be changed
				      elmake-require-list))
    (elmake-save-database))
   ((eq (car action) 'register-installed)  ;;; register-installed ;;;
    (aput 'elmake-installed-alist elmake-project-name
	  elmake-project-version)
    (elmake-save-database))
   ((eq (car action) 'register-uninstalled)  ;;; unregister-installed ;;;
    (adelete 'elmake-installed-alist elmake-project-name)
    (elmake-save-database))
   ((eq (car action) 'update-autoloads)  ;;; update-autoloads ;;;
    (let* ((dest (elmake-parse-string (car (cdr action))))
	  (flist (elmake-parse-filelist (cdr (cdr action))))
	  (generated-autoload-file (expand-file-name dest))
	  (filex (file-exists-p generated-autoload-file)))
      ;; if autoload file does not exist, create it
      (unless filex
	(save-excursion
	  (with-current-buffer (find-file-noselect generated-autoload-file)
	    (insert (concat ";; Autoloads generated by elmake -- do not edit"
			    "\n;;\n(provide '"
			    (elmake-feature-from-file generated-autoload-file)
			    ")\n\f\n"))
	    (save-buffer 0)
	    (kill-buffer nil))))
      (while flist
	(when (or (not filex) (file-newer-than-file-p (car flist) generated-autoload-file))
	  (elmake-log level (format "   updating autoloads for %s in %s"
			  (car flist) dest))
	  (update-file-autoloads (car flist)))
	  (setq flist (cdr flist)))
      (save-excursion
	(with-current-buffer (find-file-noselect generated-autoload-file)
	  (save-buffer 0)
	  (kill-buffer nil)))))
   ((eq (car action) 'nop) ;;; nop ;;;
    ;; no operation, but evaluate all args, if any
    (mapconcat 'elmake-parse-string (cdr action) ""))
   (t (error "Unknown action: %s" action))))
 
(defun elmake-feature-from-file (filename)
  "Convert FILENAME to a feature name."
  (let* ((fl (file-name-sans-extension filename))
	 (p (string-match "[^/]*$" fl))
	 (fl2 (substring fl p)))
    fl2))
	    

;;;###autoload
(defun elmake-install (arg)
  "Install the elMakefile in current buffer.
With an argument ARG, ask for the target - otherwise check if this
elMakefile has already be installed and do nothing in
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
     (arg (setq target (read-string "Target: "))) ;; fixme: autocompletion
     ((not (aget elmake-installed-alist elmake-project-name))
      (setq target "install"))
     ((string= elmake-project-version
	       (aget elmake-installed-alist elmake-project-name))
      (if (y-or-n-p "Package is up to date.  Update anyway? ")
	  (setq target "<>")
	(setq target nil)
	(message "Never mind.")))
     (t (setq target "<>")))
    (when target
      (when (string= target "<>")
	(message "Uninstalling old version...")
	(elmake-uninstall-old-version elmake-project-name
				      "uninstall-for-update")
	(setq target "update")
	(message "Uninstalling old version...done"))
      (message "Installing Target `%s'..." target)
      (elmake-run-target target)
      (message "Installing Target `%s'...done" target))))

;;;###autoload
(defun elmake-uninstall (arg)
  "Install the elMakefile in current buffer.
With an argument ARG, ask for the target - otherwise check if this
elMakefile has already be installed and do uninstall it in
that case.  Otherwise throw an error."
  (interactive "P")
  (let* ((dir (buffer-file-name))
	 (match (string-match "\\(.*\\)/" dir))
	 (substr (match-string-no-properties 1 dir))
	 (load-path (cons "." load-path)))
    (cd substr))
  (goto-char (point-min))
  (elmake-load-elmakefile (current-buffer))
  (let ((target nil))
    (cond
     (arg (setq target (read-string "Target: ")))
     ((not (aget elmake-installed-alist elmake-project-name))
      (error "Package is not installed"))
     ((string= elmake-project-version
	       (aget elmake-installed-alist elmake-project-version))
	(setq target "uninstall"))
     (t (setq target "<>")))
    (when target ;; FIXME: use cond
      (when (string= target "<>")
	(message "Uninstalling old version...")
	(elmake-uninstall-old-version elmake-project-name
				      "uninstall")
	(message "Uninstalling old version...done")
	(switch-to-buffer (get-buffer-create "*elMake*")))
      (unless (string= target "<>")
	(message "Installing Target `%s'..." target)
	(elmake-run-target target)
	(message "Installing Target `%s'...done" target)))))

(defun elmake-uninstall-old-version (name target)
  "Uninstalls the installed version of NAME.
This loads the makefile of this program and runs the TARGET
target from it."
  (let ((oldbuf (window-buffer (selected-window))) buf)
    (save-excursion
      (setq buf (find-file (concat elmake-base-dir "/" name "-"
				   (aget elmake-installed-alist name)
				   "/elMakefile")))
      (switch-to-buffer buf)
      (goto-char (point-min))
      (elmake-load-elmakefile buf)
      (kill-buffer buf)
      (elmake-run-target target))
    (switch-to-buffer oldbuf)
    (set-buffer oldbuf)
    (goto-char (point-min))
    (elmake-load-elmakefile (current-buffer))))

(defun elmake-save-database ()
  "Write the database file and compile it."
  (save-excursion
    (with-current-buffer (find-file-noselect
			  (concat elmake-base-dir "/elmake-db.el"))
      (delete-region (point-min) (point-max))
      (insert ";;; this is a generated file -- do not edit.\n\n")
      (insert (prin1-to-string (elmake-build-database)))
      (save-buffer 0)
      (save-excursion
	(emacs-lisp-byte-compile))
      (kill-buffer nil))))
  
(defun elmake-build-database ()
  "Build the elmake database.
Returns a list containing a valid Lisp file that contains all
information that has to be in the elmake database."
  `(progn
     (setq elmake-base-dir ,elmake-base-dir
	   elmake-info-dir ,elmake-info-dir
	   elmake-site-name ,elmake-site-name
	   elmake-installed-alist (quote ,elmake-installed-alist)
	   elmake-require-list (quote ,elmake-require-list))
     (require 'elmk-init ,(concat elmake-base-dir "/elmk-init"))
     (elmake-init) .
     ,(let ((reql elmake-require-list) req1 (res nil))
	(while reql
	  (setq req1 (car reql)
		reql (cdr reql))
	  (setq res (append res (list (if (listp req1) req1 `(require (quote ,req1)))
			      ))))
	res)))

(defun elmake-require (feature)
  "Require a FEATURE or execute some Lisp code."
  (if (listp feature)
      (eval feature)
    (require feature)))

(provide 'elmake)

;;; elmake.el ends here
