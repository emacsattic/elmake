;;; elmake-self-install.el --- bootstrapping elmake
;; 
;;

;;; Commentary:
;; 

(if nil (progn  ;; bootstrapping code:
	  (eval-buffer)
	  (elmake-self-install))
;;
;; place your cursor in the next (empty) line and press C-x C-e

);; have fun with elmake!

;;; Code:

(defvar elmake-base-dir nil)
(defvar elmake-info-dir nil)
(defvar elmake-si-modify-file nil)

(defun elmake-self-install ()
  "Bootstraps elmake."
  (let* ((thisfile (buffer-file-name (current-buffer)))
	 (thisdir (substring thisfile 0 (- (length thisfile) 22))))
    (elmake-si-initialize-variables)
    (add-to-list 'load-path thisdir)
    (require 'elmake-load)
    (require 'elmake-autoload)
    (find-file "./elMakefile")
    (elmake-install-current-buffer nil)
    (elmake-si-write-start-code))
  'Successful)

(defun elmake-si-initialize-variables ()
  "Asks interactively for non-initializes variables."
  (while (not elmake-base-dir)
    (setq elmake-base-dir (read-file-name "Where to install elmake to (need not be in the load path): "
					  "~/" nil nil "elmake-lisp"))
   
    (setq elmake-base-dir (elmake-si-init-dir elmake-base-dir))
  (while (not elmake-info-dir)
    (setq elmake-info-dir (read-file-name "Where to install info files to (need to be in your info path): "))
    (setq elmake-info-dir (elmake-si-init-dir elmake-info-dir))))
  (unless elmake-si-modify-file
    (setq elmake-si-modify-file (read-file-name "Where to add load code for elmake: " "~/" nil t ".emacs"))))

(defun elmake-si-init-dir (dir)
  "Check and set up directory DIR."
  (when (string-match "/$" dir)
    (setq dir (substring dir 0 (1- (length dir)))))
  (if (file-directory-p dir)
      dir
    (condition-case nil
	(progn
	  (make-directory dir)
	  dir)
      (error nil))))


(defun elmake-si-write-start-code ()
  "Writes the elmake start code into your .emacs."
  (let ((oldbuf (current-buffer)) buff)
    (save-excursion
      (setq buff (find-file-noselect elmake-si-modify-file))
      (set-buffer buff)
      (goto-char (point-max))
      (insert (format "

;; added by elmake install

\(setq elmake-base-dir \"%s\")
\(setq elmake-info-dir \"%s\")
\(require 'elmake-load (concat elmake-base-dir \"/elmake-load\"))

;; end added by elmake install
 "
		      elmake-base-dir elmake-info-dir))
      (save-buffer buff)
      (kill-buffer buff))
      (switch-to-buffer oldbuf)))

(provide 'elmake-self-install)

;;; elmake-self-install.el ends here
