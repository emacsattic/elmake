;;; elmk-batch.el --- use elmake from batch mode

;;; Commentary:
;; 
;; if you want to use batch-elmake, you should set an environment
;; variable $ELMAKE to the elmake-db file of your "primary" elmake
;; site.
;;
;; usage:
;; emacs -batch -l $ELMAKE -f batch-elmake-install elMakefile
;; emacs -batch -l $ELMAKE -f batch-elmake-install -target target elMakefile


;;; History:
;; 

;;; Code:

(require 'elmake)

(defvar command-line-args-left nil) ;; to avoid compiler warnings

;;;###autoload
(defun batch-elmake-install ()
  "Install an elmakefile in batch mode."
  (unless noninteractive
    (error "Elmake-batch only works in batch mode"))
  (elmake-test-site)
  (let (target)
    (setq elmake-batch-mode t)
    (when (string= (car command-line-args-left) "-target")
      (setq target (nth 1 command-line-args-left)
	    command-line-args-left (cdr (cdr command-line-args-left))))
    (find-file (car command-line-args-left))
    (if target
	(elmake-install (cons 'batchmode target))
      (elmake-install 'batchmode))
    (setq command-line-args-left nil)))

;;;###autoload
(defun batch-elmake-test ()
  "Send a message to the terminal.
This simplifies testing whether loading elmake in batch mode works."
  (send-string-to-terminal "Elmake batch mode test successful.\n"))



(provide 'elmk-batch)

;;; elmk-batch.el ends here
