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
					"This is a scratch org buffer!\n"
					"#+END_COMMENT\n")
			(org-mode))))))
