(require 'package)

(defun install-config-packages ()
  (interactive)
  (progn
    (package-install 'slime)
    (package-install 'typescript-mode)
    (package-install 'org-jira)
    ))
