;;; elmake-mode.el --- a mode to highlight elMakefiles

;;; Commentary:
;; 

;;; Code:

(defconst elmake-mode-keywords
  (eval-when-compile
    (concat "(\\("
	    (regexp-opt '("elmakefile" "filelist" "string" "target" "eval"
			  "concat" "depends" "combine" "remove "))
	    "\\)[ )]")))

(defconst elmake-mode-builtins
  (eval-when-compile
    (concat "(\\("
	    (regexp-opt '("compile" "message" "mkdir" "copy" "copy-source"
			  "delete" "chdir" "makeinfo" "rmdir" "evaluate"
			  "execute" "register-require" "register-installed"
			  "register-uninstalled" "unregister-require"))
	    "\\)[ )]")))
  

(define-derived-mode elmake-mode lisp-mode "elMake"
  "Major mode for editing and running elMakefiles"
  (if (string-match "_$" (buffer-file-name))
      (define-key elmake-mode-map (kbd "C-c C-v") 'elmake-install-to)
    (define-key elmake-mode-map (kbd "C-c C-v") 'elmake-install)))


(add-to-list 'auto-mode-alist
	     '("\\(\\(^\\|/\\)el[Mm]ake\\(file\\|.fle\\)\\|\\.el[Mm]ake_?\\)\\'"
	        . elmake-mode))

(font-lock-add-keywords
 'elmake-mode
 (list (cons elmake-mode-keywords  1)
       (list elmake-mode-builtins 1 'font-lock-builtin-face)))

(defun elmake-install-to (arg dest)
  "Install the elMakefile in current buffer like `elmake-install'.
ARG is the interactive argument passed to `elmake-install', DEST is
the directory where the source files of this project are
expected.  Unlike `elmake-install', this function does not expect the
source files in the same directory as the elmakefile.  This function is
used by default for elmakefiles whose extension is `.elMake_'."
(interactive "P\nDSource directory: ")
  (cd dest)
  (elmake-install arg))

(provide 'elmake-mode)

;;; elmake-mode.el ends here
