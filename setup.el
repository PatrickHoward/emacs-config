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
    (package-install 'rebecca-theme)
    (package-install 'org-tree-slide)B
    (message "Setup Complete! Enjoy your EMACS! :)")
    ))

  
