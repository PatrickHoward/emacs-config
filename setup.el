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
    (package-install 'rust-mode)
    (package-install 'cloc)
    (package-install 'groovy-mode)
    (package-install 'obsidian)
    (package-install 'pandoc)
    (package-install 'ue)
    (package-install 'spacious-padding)
    (package-install 'org-modern)
    (package-install 'company)
    (package-install 'dashboard)
    (package-install 'nerd-icons)
    (package-install 'ox-pandoc)
    (package-install 'company-irony)
    (package-install 'slime-company)
    (package-install 'slack)
    (package-install 'helm)
    (package-install 'rebecca-theme)
    (byte-compile-file "~/Repos/emacs-config/addl-packages/helm-slack/helm-slack.el")
    (message "Setup Complete! Enjoy your EMACS! :)")
    ))

(defun compile-init-file ()
  (interactive)
  (byte-compile-file "~/Repos/emacs-config/init.el")
  (load "init.elc"))

(defun first-time-setup ()
  (interactive)
  (load "init.el")
  (install-config-packages)
  (compile-init-file))

  
