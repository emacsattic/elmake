;;; elmake-autoload.el --- autoloads for elmake

;;; Commentary:
;; 

;;; Code:
(autoload 'elmake-install "elmake" "" t nil)

(autoload 'elmake-mode "elmake-mode" "" t nil)
(add-to-list 'auto-mode-alist
	     '("\\(\\(^\\|/\\)el[Mm]akefile\\|\\.el[Mm]ake_?\\)\\'"
	       . elmake-mode))

(provide 'elmake-autoload)

;;; elmake-autoload.el ends here
