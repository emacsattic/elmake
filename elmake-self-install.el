;;; elmake-self-install.el --- bootstrapping elmake
;; $Id$

;;; Commentary:

;; This file allows you to install elmake automatically.  To do so:
;; open this file in emacs (as you are reading this, you probably did
;; that already). Then move the cursor to the point below where it
;; states you should move the point to and press C-x C-e. This will
;; start the installation which will ask you some questions (all these
;; questions apply to the "default site" if you want to use multiple
;; sites (places where installed packages are placed)):

;; Base dir: This is the dir where every package will get a subdir
;; in. This dir will automatically be added to your load path, so it
;; need not be there. This dir should be empty before installation

;; Info dir: The dir where info files for the packages should end
;; up. All info files for all packages will end up in one and only
;; dir. This dir will automatically be added to your info path as well
;; (as long as you use info from within emacs). You can give any dir,
;; it need not be empty, and of course it may be in your info path
;; already.

;; Load code file: Elmake will add code to this file for initializing
;; it. So use a file that will be loaded when Emacs is loaded. For
;; personal sites (where the base dir is below your home dir, best use
;; your .emacs file. For other sites (e.g. in your site-lisp) use your
;; site-init.el file. You may use any other file as well, as long as
;; it is loaded when emacs loads.

;; That was it. elMake should work now. To make sure everything worked
;; as it should, I'd suggest to close Emacs and reopen it.

;;--------------------------------------
(if nil (progn  ;; bootstrapping code:
	  (eval-buffer)
	  (elmake-self-install))
;;--------------------------------------

;; place your cursor in the next (empty) line and press C-x C-e

;; have fun with elmake!
)

;;; History:
;; 

;;; Code:

(defvar elmake-base-dir nil)
(defvar elmake-info-dir nil)
(defvar elmake-si-modify-file nil)

(defun elmake-self-install ()
  "Bootstrap elmake."
  (let* ((thisfile (buffer-file-name (current-buffer)))
	 (thisdir (substring thisfile 0 -23)))
    (add-to-list 'load-path thisdir)
    (require 'elmk-init)
    (require 'elmake)
    (require 'elmk-site)
    (require 'elmk-info)
    (elmake-site-add "default" nil nil nil 'elmake-si-initfunc))
  'Successful)

(defun elmake-si-initfunc ()
  "Initfunc for `elmake-site-add'."
  (find-file "./elMakefile")
  (elmake-install nil)
  (elmake-save-database))

(provide 'elmake-self-install)

;;; elmake-self-install.el ends here
