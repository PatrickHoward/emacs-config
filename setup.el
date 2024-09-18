(require 'package)

(defun pmh/install-config-packages ()
  (interactive)
  (progn
	(package-refresh-contents)
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
	(nerd-icons-install-fonts)
    (package-install 'ox-pandoc)
    (package-install 'company-irony)
    (package-install 'slime-company)
    (package-install 'rebecca-theme)
    (package-install 'org-tree-slide)
    (package-install 'doom-modeline)
    (package-install 'doom-themes)
    (package-install 'all-the-icons)
	(package-install 'treemacs-nerd-icons)
	(package-install 'easysession)
	(package-install 'dirvish)
	(package-install 'yaml-mode)
	(package-install 'org-tree-slide-mode)
	(pmh/compile-org-scratch)
	(package-install 'vertico)
	(package-install 'which-key)
	(package-install 'nov)
	(package-install 'vertico-posframe)
	(message "Installation Complete")))

(defun pmh/compile-org-scratch ()
  (interactive)
  (byte-compile-file "./org-scratch.el"))
