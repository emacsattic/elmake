;;; elmk-pkman.el --- elmake package manager
;; $Id$

;;; Commentary:
;; 
;; this package manager looks ugly.  make it look better.

;;; History:
;; 

;;; Code:

(require 'elmk-singl)
(require 'elmake)

;;;###autoload
(defun elmake-package-manager ()
  "Start the elmake package manager."
  (interactive)
  (elmake-test-site t)
  (pop-to-buffer (get-buffer-create "*elMake Package Manager*"))
  (elmake-pm-mode)
  (setq buffer-read-only nil)
  (delete-region (point-min) (point-max))
  (insert "Elmake Package Manager:
Move point onto a line that has an [action button] and press RET or C-c C-c
to invoke.  Press `q' to quit.

Current site is `" elmake-site-name "'.

Available sites:\n")
  (mapc
   (lambda (csl)
      (insert (car csl) " <" (car (cdr csl)) "> "
	      (if (string= (car csl) elmake-site-name)
		  "(current site)\n"
		"[Select]\n")))
   elmake-site-alist)
  (insert "[Add]\n\nInstalled packages:\n")
  (mapc
   (lambda (csl)
     (insert (car csl) " (" (cdr csl) ") [Uninstall]\n"))
   elmake-installed-alist)
  (insert "\nInstalled single files:\n")
  (mapc
   (lambda (sf)
     (insert (car sf) " [Remove]\n"))
   (elmake-installed-single-file-alist))
  (insert "\n\n----------------------")
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (force-mode-line-update)
  (goto-char (point-min))
  (font-lock-fontify-buffer)
  (message nil))

(defun elmake-package-manager-refresh ()
  "Refresh current display of the elmake package manager."
  (elmake-package-manager))

(defun elmake-package-action ()
  "Run the [Action] that is in same line as point."
  (interactive)
  (elmake-test-site t)
  (let ((line (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
	action context)
  (when (string-match "\\`\\([^ ]*\\).*\\[\\([^][ ]*\\)\\]\\'" line)
    (setq context (match-string-no-properties 1 line)
	  action (match-string-no-properties 2 line))
    (cond
     ((string= action "Add")
      (call-interactively 'elmake-site-add)
      (elmake-package-manager-refresh))
     ((string= action "Select")
      (elmake-site-select context)
      (elmake-package-manager-refresh))
     ((and (string= action "Uninstall")
	   (y-or-n-p (format "Really uninstall %s? " context)))
      (elmake-test-site)
      (elmake-uninstall-old-version context "uninstall" t)
      (elmake-package-manager-refresh)
      (switch-to-buffer (get-buffer-create "*elMake*")))
     ((and (string= action "Remove")
	   (y-or-n-p (format "Really remove %s? " context)))
      (elmake-test-site)
      (elmake-single-uninstall context)
      (elmake-package-manager-refresh)
      (switch-to-buffer (get-buffer-create "*elMake*")))
     (t nil)))))

;; code for elmake-pm-mode follows:

(define-derived-mode elmake-pm-mode fundamental-mode
  "Package Manager"
  "Major mode for showing the Package Manager."
  (define-key elmake-pm-mode-map (kbd "C-c C-c") 'elmake-package-action)
  (define-key elmake-pm-mode-map (kbd "RET") 'elmake-package-action)
  (define-key elmake-pm-mode-map (kbd "q") 'elmake-package-cancel))

(defun elmake-package-cancel ()
  "Cancel the package manager."
  (interactive)
  (kill-buffer (current-buffer))
  (message "Package manager closed."))

(font-lock-add-keywords
 'elmake-pm-mode
 `( ("\\(\\[[^][ ]*\\]\\)"  1 font-lock-warning-face)
    ("^\\(.*:\\)$" 1 font-lock-function-name-face)
    ("^.*(current site)\n" 0 'highlight)))

(provide 'elmk-pkman)

;;; elmk-pkman.el ends here
