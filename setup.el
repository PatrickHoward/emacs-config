(require 'package)

(defun install-config-packages ()
  (interactive)
  (progn
    (package-install 'slime)
    (package-install 'typescript-mode)
    (package-install 'org-jira)
    (package-install 'markdown-mode)
    (package-install 'treemacs)
    (package-install 'clang-format)
    (message "Setup Complete! Enjoy your EMACS! :)")
    ))

(defun compile-init-file ()
  (interactive)
  (byte-compile-file "~/Repos/emacs-config/init.el"))
