;;; elmk-info.el --- makeinfo support for elMake
;; $Id$

;;; Commentary:
;; 

;;; History:
;; 

;;; Code:
(require 'elmake)

(defcustom elmake-makeinfo-executable "makeinfo"
  "*Filename of the makeinfo binary."
  :type 'string
  :group 'elmake)

(defcustom elmake-makeinfo-use-texinfo-format-buffer nil
  "*Whether to use `texinfo-format-buffer' when makeinfo is not available."
  :type 'boolean
  :group 'elmake)

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
      (elmake-log 0 (format ">>> makeinfo failed (reason: %S)." res))
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
    (elmake-texinfo-format-buffer)
    (message "Buf: %S" (current-buffer))
    (save-buffer 0))
  (elmake-log 0 ">>> texinfo-format-buffer successful.")
  (set-buffer (get-buffer-create "*elMake*"))
  t)

;;;###autoload
(defun elmake-makeinfo-dest-file (filename)
  "Determine the destination name of texi file FILENAME."
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
	    (error
	     "The texinfo file needs a line saying: @setfilename <name>")))
      (kill-buffer nil)))
    makeinfo-output-file-name))

;;;###autoload
(defun elmake-texinfo-format-buffer ()
"Process the current buffer as texinfo code, into an Info file.
This uses `texinfo-format-buffer' after stripping some tags out of the
file which cannot be handled by that function."
  (interactive)
  (let ((buf (current-buffer))
	(dd default-directory))
    (with-current-buffer (get-buffer-create "*elmake-texinfo*")
      (setq default-directory dd)
      (delete-region (point-min) (point-max))
      (insert-buffer-substring buf)
      (goto-char (point-min))
      (replace-string "\n@copying\n" "\n")
      (replace-string "\n@end copying\n" "\n")
      (replace-string "@insertcopying" "[insert COPYING here]")
      (replace-regexp "^ *@end" "@end")
      ;; fixme: change more things that make texinfo-format-buffer barf
      ;; @macro (semantic.texi)
      ;; 
      (texinfo-format-buffer))))


;;;###autoload
(defun elmake-install-info (filename)
  "Install info file FILENAME into elmake's `dir' file."
  (interactive "fInstall info file: ")
  (elmake-parse-info-entries 'elmake-install-info-entry filename))

(defun elmake-uninstall-info (filename)
  "Install info file FILENAME from elmake's `dir' file."
  (interactive "fUninstall info file: ")
  (elmake-parse-info-entries 'elmake-uninstall-info-entry filename))

(defun elmake-parse-info-entries (func filename)
  "Apply FUNC to all info-dir-entries found in info file FILENAME."
  (with-current-buffer (find-file-noselect (concat elmake-info-dir
						   "/" filename))
    (let ((section "Miscellaneous")
	  endpos now mpos entry)
      (goto-char (point-min))
      (save-excursion
	(setq endpos (search-forward "\037" nil t)))
      (setq now (point))
      (while (search-forward "\nSTART-INFO-DIR-ENTRY\n" endpos t)
	(setq mpos (point))
	(save-excursion
	  (goto-char now)
	  (if (search-forward "\nINFO-DIR-SECTION " mpos t)
	      (setq section (buffer-substring-no-properties
			     (point)
			     (progn (skip-chars-forward "^\n\r")
				    (point))))))
	(when (search-forward "\nEND-INFO-DIR-ENTRY\n" endpos t)
	  (setq entry (buffer-substring-no-properties
		       mpos
		       (1+ (match-beginning 0))))
	  (apply func section entry nil))))
    (kill-buffer nil)))

(defun elmake-install-info-entry (section entry)
  "Add info entry to given SECTION of elmake's `dir' file.
ENTRY is the string containing the complete info-dir-entry, including
a trailing newline.  The section is created if needed."
  (with-current-buffer (find-file-noselect (concat elmake-info-dir
						   "/dir"))
    (let (stpt endpos found comp)
      (elmake-init-info)
      (goto-char (1- (point)))
      (unless (search-forward (concat "\n\n" section "\n") nil 1)
	(unless (bolp)
	  (insert "\n"))
	(insert "\n" section "\n"))
      ;; now point is at the first line of the section.
      (setq stpt (point))
      (save-excursion
	(goto-char (1- (point)))
	(setq endpos (1- (or (search-forward "\n\n" nil t)
			    (1+ (point-max))))
	      found nil))
      (while (and (not found) (< (point) endpos))
	(forward-line)
	(while (not (or (looking-at "\\* ") (>= (point) endpos)))
	  (forward-line))
	;; there was an entry, test it
	(setq comp (buffer-substring-no-properties stpt (point)))
	(if (string< comp entry)
	    (setq stpt (point))
	  (setq found t))
	(if (string= comp entry)
	    (setq found 'exact)))
      (goto-char stpt)
      (unless (eq found 'exact)
	(insert entry)))
    (save-buffer 0)
    (kill-buffer nil)))

(defun elmake-uninstall-info-entry (section entry)
  "Remove info entry from given SECTION of elmake's `dir' file.
ENTRY is the string containing the complete info-dir-entry, including
a trailing newline.  Empty sections are not removed yet."

  (with-current-buffer (find-file-noselect (concat elmake-info-dir
						   "/dir"))
    (let (stpt endpos found comp)
      (elmake-init-info)
      (when (search-forward entry  nil t)
	(delete-region (match-beginning 0) (point))
	;; fixme: check if there are section headers left
	))
    (save-buffer 0)
    (kill-buffer nil)))

(defun elmake-init-info ()
  "Initialize info `dir' file in current buffer.
Place point after the `* Menu:' line."
  (goto-char (point-min))
  (cond
   ((= (point-min) (point-max))
    (insert "-*- Text -*-
This is the file .../info/dir, which contains the
topmost node of the Info hierarchy, called (dir)Top.
The first time you invoke Info you start off looking at this node.
\037
File: dir,	Node: Top	This is the top of the INFO tree

  This (the Directory node) gives a menu of major topics.
  Typing \"q\" exits, \"?\" lists all Info commands, \"d\" returns here,
  \"h\" gives a primer for first-timers,
  \"mEmacs<Return>\" visits the Emacs manual, etc.

  In Emacs, you can click mouse button 2 on a menu item or cross reference
  to select it.

* Menu:
")
    (goto-char (point-max)))
   ((search-forward "* Menu:\n" nil t)
    nil)
   (t
    (goto-char (point-max))
    (unless (bolp)
      (insert "\n"))
    (insert "* Menu:\n")
    (goto-char (point-max)))))

(provide 'elmk-info)

;;; elmk-info.el ends here
