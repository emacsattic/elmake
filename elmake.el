;;; elmake.el --- a "make" for emacs lisp projects in pure emacs lisp
;; $Id$

;; Copyright (C) 2003 Michael Schierl

;; Author: Michael Schierl <schierlm-public@gmx.de>
;; Keywords: makefile install elisp

(defvar elmake-version "0.1"
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
;;

;;; History:
;; 

;;; Code:

(require 'assoc)

(if (not (functionp 'when))
    (require 'cl))   ;; when, unless

(defgroup elmake nil
  "Emacs Make System."
  :group 'development)

(defcustom elmake-copy-source t
  "*Whether to copy elisp source files into the install dir."
  :type 'boolean
  :group 'elmake)

(defvar elmake-base-dir nil
  "The directory where elmake is installed to.")
(defvar elmake-info-dir nil)
(defvar elmake-site-name nil)

(defvar elmake-project-name nil)
(defvar elmake-project-version nil)
(defvar elmake-project-target-alist nil)
(defvar elmake-project-string-alist nil)
(defvar elmake-project-filelist-alist nil)
(defvar elmake-project-elmakefile nil)
(defvar elmake-targets-done nil)
(defvar elmake-installed-alist nil)
(defvar elmake-require-list nil)

(defvar elmake-batch-mode nil)

(defun elmake-load-elmakefile (buf)
  "Load an elMakefile from buffer BUF.
The data is stored into the elmake-project-* variables."
  (setq elmake-project-name nil
	elmake-project-version nil
	elmake-project-elmakefile (buffer-file-name buf)
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
  (let* ((mfile (read buf)) cfirst)
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
      (mapc
       (lambda (first)
	 (setq cfirst (car first))
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
	  (t (error "Cannot parse : %s" first))))
       mfile)))
	    
  
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
    (let ((result (elmake-parse-filelist-entry (car (cdr elem)))))
      (mapc (lambda (dir)
	      (mapc
	       (lambda (entry)
		 (add-to-list 'result entry))
	       (elmake-parse-filelist-entry dir)))
	    (cdr (cdr elem)))
      result))
   ((and (listp elem) (eq (car elem) 'filenames))
    (elmake-parse-filelist-entry (concat "\\`" (regexp-opt (cdr elem) t)
					 "\\'")))
   ((and (listp elem) (eq (car elem) 'filesuffixes))
    (elmake-parse-filelist-entry (concat (regexp-opt (cdr elem) t)
					 "\\'")))
   ((and (listp elem) (eq (car elem) 'exact))
    (elmake-parse-string (cdr elem)))
   ((and (listp elem) (eq (car elem) 'all-files))
    (let (result)
      (mapc
       (lambda (file)
	 (cond
	  ((or (string= "." file) (string= ".." file))
	   nil)
	  ((file-directory-p file)
	   (setq result (append result
				  (elmake-parse-filelist-entry
				   `(indir ,file (all-files))))))
	  (t
	   (setq result (append result (list file))) )))
       (directory-files "."))
      result))
   ((and (listp elem) (eq (car elem) 'indir))
    (let ((default-directory default-directory) res)
      (cd (expand-file-name (car (cdr elem))))
      (setq res (elmake-parse-filelist (cdr (cdr elem))))
      (mapcar (lambda (x)
		(concat (car (cdr elem)) "/" x)) res)))
   ((and (listp elem) (eq (car elem) 'remove))
    (let ((inc (elmake-parse-filelist-entry (car (cdr elem))))
	  (exc (elmake-parse-filelist-entry (cdr (cdr elem)))))
      (mapc
       (lambda (elm)
	 (setq inc (remove elm inc)))
       exc)
      inc))
   ((and (symbolp elem) (aget elmake-project-filelist-alist elem))
    (elmake-parse-filelist (aget elmake-project-filelist-alist elem)))
   (t
    (directory-files "." nil (elmake-parse-string elem)))))

(defun elmake-parse-filelist (fl &optional indir)
"Parse a file list clause FL.
When optional INDIR is given, use that directory as base directory.
This is done by parsing all entries separately and concatenating the
resulting lists."
(let ((default-directory (if indir
			     (expand-file-name indir)
			   default-directory)))
  (apply 'append
	 (mapcar 'elmake-parse-filelist-entry fl))))

(defun elmake-parse-destination (dest file)
  "Parse a destination DEST.
This takes a file FILE and returns the name of it at that destination."
  (cond
   ((null dest) ;; nil leaves file as is
    file)
   ((and (consp dest) (eq (car dest) 'flat))
    (let* ((dest2 (elmake-parse-destination (car (cdr dest)) ""))
	   (p (string-match "[^/]*$" file))
	   (file2 (if p
		      (substring file p)
		    p)))
      (concat dest2 file2)))
   ((and (consp dest) (eq (car dest) 'replace))
    (let (result (dst (cdr dest)))
      (while (and (null result) dst)
	(when (string= (substring file 0 (length (car dst))) (car dst))
	  (setq result (concat (nth 1 dst)
			       (substring file (length (car dst))))))
	(setq dst (nthcdr 2 dst)))
      (if result
	  result
	(error "No matching search term qfor %S and file %S"
	       dest file))))
   ((stringp (elmake-parse-string dest))
    (concat (elmake-parse-string dest) "/" file))
   (t (error "Unknown destination %S for file %S" dest file))))

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
  (make-string (* 3 level) ? ))

(defun elmake-log (level string)
  "Log a string into the log buffer.
LEVEL specifies how many spaces to add before it, STRING the string to
add."
  (set-buffer (get-buffer-create "*elMake*"))
  (insert (concat (elmake-indent level) string "\n"))
  (if elmake-batch-mode
      (send-string-to-terminal (concat (elmake-indent level) string "\n")))
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
    (mapc
     (lambda (action)
       (elmake-run-action action level))
     actions))
    (elmake-log level (format "--- Finished %s" targetname)))

(defun elmake-run-action (action level)
  "Run one ACTION.
LEVEL specifies the nesting level of the target running this action."
;;   (message ">>>%S" action)
  (unless (listp action)
    (error "Unknown action: %s" action))
  (cond
   ((eq (car action) 'depends) ;;; depends ;;;
    (mapc
     (lambda (elem)
	(unless (member elem elmake-targets-done)
	  (elmake-run-target-0 elem (1+ level))))
     (cdr action)))
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
    (mapc
     (lambda (file)
	(when (file-newer-than-file-p file (concat file "c"))
	  (elmake-log level (format "   byte compiling %s" file))
	  (unless (byte-compile-file file)
	    (elmake-log level (format "*** error compiling %s" file)))))
     (elmake-parse-filelist (cdr action))))
   ((eq (car action) 'makeinfo) ;;; makeinfo ;;;
    (mapc
     (lambda (file)
       (when (file-newer-than-file-p file (elmake-makeinfo-dest-file file))
	  (elmake-log level (format "   making info for %s" file))
	  (unless (elmake-makeinfo file)
	    (elmake-log level (format "*** error-making info for %s" file)))))
     (elmake-parse-filelist (cdr action))))
   ((eq (car action) 'install-info) ;;; install-info ;;;
    (mapc
     (lambda (fl)
       (elmake-log level (format "   adding %s to `dir' node" fl))
       (elmake-install-info fl))
     (elmake-parse-filelist (cdr action) elmake-info-dir)))
   ((eq (car action) 'uninstall-info) ;;; uninstall-info ;;;
    (mapc
     (lambda (fl)
       (elmake-log level (format "   removing %s from `dir' node" fl))
       (elmake-install-info fl))
     (elmake-parse-filelist (cdr action) elmake-info-dir)))
   ((or (eq (car action) 'copy) ;;; copy ;;;
	(and (eq (car action) 'copy-source) ;;; copy-source ;;;
	     elmake-copy-source))
    (mapc
     (lambda (file)
       (let ((destfile (elmake-parse-destination (car (cdr action)) file)))
	 (when (file-newer-than-file-p file destfile)
	   (elmake-log level (format "   copying %s to %s"
				     file destfile))
	   (copy-file file destfile t))))
     (elmake-parse-filelist (cdr (cdr action)))))
   ((eq (car action) 'copy-elmakefile) ;;; copy-elmakefile ;;;
    (let ((dest (elmake-parse-string (car (cdr action)))))
      (when (file-newer-than-file-p elmake-project-elmakefile
				    (concat dest "/elMakefile"))
	(elmake-log level (format "   copying %s to %s"
				  (elmake-basename elmake-project-elmakefile)
				  dest))
	(copy-file elmake-project-elmakefile
		   (concat dest "/elMakefile") t t))))
   ((eq (car action) 'copy-source) ;;; copy-source when disabled ;;;
    nil)
   ((eq (car action) 'delete) ;;; delete;;;
    (let ((dest (car (cdr action))))
      (mapc
       (lambda (file)
	 (let ((destfile (elmake-parse-destination dest file)))
	   (when (file-exists-p destfile)
	     (elmake-log level (format "   deleting %s"
				       file))
	     (delete-file destfile))))
       (elmake-parse-filelist (cdr (cdr action))
			      (elmake-parse-destination dest "")))))
   ((eq (car action) 'delete-elmakefile) ;;; delete-elmakefile;;;
    (let ((dest (elmake-parse-string (car (cdr action)))))
      (when (file-exists-p (concat dest "/elMakefile"))
	(elmake-log level "   deleting elMakefile")
	(delete-file(concat dest "/elMakefile")))))
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
   ((eq (car action) 'run) ;;; run ;;;
    (eval (cons 'progn  (cdr action))))
   ((eq (car action) 'touch) ;;; touch ;;;
    (let* ((dest (nth 1 action))
	   (file (elmake-parse-string (nth 2 action)))
	   (destfile (elmake-parse-destination dest file))
	   (contents (nth 3 action)))
      (save-excursion
	(with-current-buffer (find-file-noselect destfile)
	  (set-buffer-modified-p t)
	  (when contents
	    (delete-region (point-min) (point-max))
	    (insert (elmake-parse-string contents)))
	  (save-buffer 0)
	  (kill-buffer nil)))))
   ((eq (car action) 'copy-modified) ;;; copy-modified ;;;
    (let* ((dest (car (cdr action)))
	   (file (elmake-parse-string (nth 2 action)))
	   (mods (cdr (cdr (cdr action)))))
      (elmake-log level (format "   copying modified version of %s to %s"
				file
				(elmake-parse-destination dest file)))
      (save-excursion
	(with-current-buffer (find-file-noselect file)
	  (mapc 'elmake-run-buffer-mod mods)
	  (write-file (elmake-parse-destination dest file))
	  (kill-buffer nil)))))
   ((eq (car action) 'depends-if) ;;; depends-if ;;;
    (if (eval (car (cdr action)))
	(elmake-run-action (cons 'depends (cdr (cdr action))) level)))
   ((eq (car action) 'needs-integer) ;;; needs-integer ;;;
    (let ((var (nth 1 action))
	  (tocomp (nth 2 action)))
      (unless (and (boundp var) (>= (eval var) tocomp))
	(elmake-log level (concat "Dependency error: "
				  (elmake-parse-string (nth 3 action))))
	(error "Dependency not met"))))
   ((eq (car action) 'needs-string) ;;; needs-string ;;;
    (let ((var (nth 1 action))
	  (tocomp (elmake-parse-string (nth 2 action))))
      (unless (and (boundp var) (or (string= (eval var) tocomp)
				    (string< tocomp (eval var))))
	(elmake-log level (concat "Dependency error: "
				  (elmake-parse-string (nth 3 action))))
	(error "Dependency not met"))))
   ((eq (car action) 'needs-version) ;;; needs-version ;;;
    (let ((var (nth 1 action))
	  (tocomp (elmake-parse-string (nth 2 action))))
      (unless (and (boundp var) (or (string= (eval var) tocomp)
				    (elmake-earlier-versionp tocomp
							     (eval var))))
	(elmake-log level (concat "Dependency error: "
				  (elmake-parse-string (nth 3 action))))
	(error "Dependency not met"))))
   ((eq (car action) 'needs-package) ;;; needs-package ;;;
    (let ((var (aget elmake-installed-alist
		     (elmake-parse-string (nth 1 action))))
	  (tocomp (elmake-parse-string (nth 2 action))))
      (unless (and var (or (string= (eval var) tocomp)
			   (elmake-earlier-versionp tocomp
						    (eval var))))
	(elmake-log level (concat "Dependency error: "
				  (elmake-parse-string (nth 3 action))))
	(error "Dependency not met"))))
   (t (error "Unknown action: %s" action)))) ;;; end of action list ;;;
 

(defun elmake-run-buffer-mod (mod)
  "Run buffer mod MOD.
This mod might change the contents of the current buffer.  Buffer mods
are used for the `copy-modified' command."
  (cond
   ((not mod)
    nil)
   ((not (consp mod))
    (error "Unsupported buffer mod: %S" mod))
   ((eq (car mod) 'eval)
    (eval (cons 'progn (cdr mod))))
   ((eq (car mod) 'replace)
    (let ((args (cdr mod)) fromstring tostring)
      (while (and args (cdr args))
	(setq fromstring (elmake-parse-string (car args))
	      tostring (elmake-parse-string (car (cdr args)))
	      args (cdr (cdr args)))
	(goto-char (point-min))
	(while (search-forward fromstring nil t)
	  (replace-match tostring t t)))
      (if args (error "Replace needs an even number of arguments"))))
   ((eq (car mod) 'replace-regexp)
    (let ((args (cdr mod)) fromstring tostring)
      (while (and args (cdr args))
	(setq fromstring (elmake-parse-string (car args))
	      tostring (elmake-parse-string (car (cdr args)))
	      args (cdr (cdr args)))
	(goto-char (point-min))
	(while (re-search-forward fromstring nil t)
	  (replace-match tostring nil nil)))
      (if args (error "Regexp replace needs an even number of arguments"))))
   (t
    (error "Unsupported buffer mod: &S" mod))))

(defun elmake-feature-from-file (filename)
  "Convert FILENAME to a feature name."
  (let* ((fl (file-name-sans-extension filename))
	 (p (string-match "[^/]*$" fl))
	 (fl2 (substring fl p)))
    fl2))
	    

(defun elmake-basename (filename)
  "Return the basename (name without directory) of FILENAME."
  (let ((p (string-match "[^/]*$" filename)))
    (if p
	(substring filename p)
      p)))

(defun elmake-directory-name (filename &optional endslash)
  "Return the directory name of FILENAME.
When optional ENDSLASH is non-nil, always return a trailing slash."
  (let ((p (string-match "^\\(.*\\)/" filename)))
    (if p
	(let ((fn (match-string-no-properties 1 filename)))
	  (if (or endslash (string= "" fn) (string-match ":$" fn))
	      (concat fn "/")
	    fn))
      filename)))

;;;###autoload
(defun elmake-install (arg)
  "Install the elMakefile in current buffer.
With an argument ARG, ask for the target - otherwise check if this
elMakefile has already be installed and do nothing in
that case.  Otherwise run the \"install\" target.  If an older version has
been installed, uninstall that one first."
  (interactive "P")
  (elmake-test-site)
  (let* ((dir (buffer-file-name))
	 (substr (elmake-directory-name dir)))
    (unless (string-match "\\.el[mM]ake_\\'" dir)
      (cd substr)))
  (goto-char (point-min))
  (elmake-load-elmakefile (current-buffer))
  (let ((target nil))
    (cond
     ((eq arg 'batchmode)
      (if (aget elmake-installed-alist elmake-project-name)
	  (setq target "<>")
	(setq target "install")))
     ((and (consp arg)
	   (eq (car arg) 'batchmode))
      (setq target (cdr arg)))
     (arg (setq target (completing-read "Target: "
					elmake-project-target-alist nil t)))
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
  "Uninstall the elMakefile in current buffer.
With an argument ARG, ask for the target - otherwise check if this
elMakefile has already be installed and uninstall it in
that case.  Otherwise throw an error."
  (interactive "P")
  (elmake-test-site)
  (let* ((dir (buffer-file-name))
	 (substr (elmake-directory-name dir))
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
    (cond
     ((not target)
      nil)
     ((string= target "<>")
      (message "Uninstalling old version...")
      (elmake-uninstall-old-version elmake-project-name
				    "uninstall")
      (message "Uninstalling old version...done")
      (switch-to-buffer (get-buffer-create "*elMake*")))
     (t
      (message "Installing Target `%s'..." target)
      (elmake-run-target target)
      (message "Installing Target `%s'...done" target)))))

(defun elmake-uninstall-old-version (name target &optional noread)
  "Uninstalls the installed version of NAME.
This loads the makefile of this program and runs the TARGET target
from it.  If NOREAD is non-nil, do not treat current buffer as an
elMakefile."
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
    (unless noread
      (set-buffer oldbuf)
      (goto-char (point-min))
      (elmake-load-elmakefile (current-buffer)))))

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

(defun elmake-test-site (&optional allow-read-only)
  "Test current elmake site.
If no site exists or the current site is read-only, signal an error.
If ALLOW-READ-ONLY is set, do not signal an error when the site is
read only.  Return whether the site is read-write."
  (unless elmake-base-dir
    (error "No elmake site"))
  (let ((rw (and (file-writable-p (concat elmake-base-dir "/elmake-db.el"))
		 (file-writable-p (concat elmake-base-dir "/dummy.fle")))))
  (unless (or allow-read-only rw)
    (error "Elmake site is read-only"))
  rw))

;; shamelessly ripped from jde.el
(defun elmake-earlier-versionp (ver1 ver2)
  "Return non-nil if VER1 is earlier than VER2."
  (let ((ver1-betap (string-match "beta" ver1))
        (ver2-betap (string-match "beta" ver2)))
    (if (or (and ver1-betap ver2-betap)
            (and (not ver1-betap) (not ver2-betap)))
        (string< ver1 ver2)
      (if ver1-betap
          (progn
            (or (string= ver2 (substring ver1  0 ver1-betap))
                (string< (substring ver1 0 ver1-betap) ver2)))
	(progn
	  (string< ver1 (substring ver2 0 ver2-betap)))))))

(provide 'elmake)

;;; elmake.el ends here
