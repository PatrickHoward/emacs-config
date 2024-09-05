(require 'org)

(defun org-scratch ()
  (interactive)
  (let ((buffer-doesnt-exist-p (eq (get-buffer "*org-scratch*") nil))
		(buffer (get-buffer-create "*org-scratch*")))
	(switch-to-buffer buffer)
	(with-current-buffer buffer
	  (if buffer-doesnt-exist-p
		  (progn
			(insert "#+BEGIN_COMMENT\n"
					"This is a scratch buffer with Org Mode enabled.\n"
					"Use this as a scratch pad for your ideas!\n"
					"#+END_COMMENT\n\n")
			(org-mode))))))
