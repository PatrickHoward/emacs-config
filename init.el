;; Common keybinds, some of these are modified
;; to account for me messing up frequent ones 
(global-set-key (kbd "C-x C-b") 'switch-to-buffer)
(global-set-key (kbd "C-x C-k") 'kill-buffer)
(global-set-key (kbd "C-SPC") 'set-mark-command)
(global-set-key (kbd "C-x s") 'save-current-buffer)
(global-set-key (kbd "C-S-x C-S-s") 'save-some-buffers)
(global-set-key (kbd "C-x C-r") 'revert-buffer)
(global-set-key (kbd "C-t") nil)
(global-set-key (kbd "C-/") 'comment-or-uncomment-region)
(global-set-key (kbd "M-`") nil)
(global-set-key [mouse-2] nil)

(defun pmh/kill-selected-text ()
  (interactive)
  (if (region-active-p)
      (call-interactively 'kill-region)
    (call-interactively 'kill-line)))

(global-set-key (kbd "C-k") 'pmh/kill-selected-text)

;; When switching buffers, do not open a new window
(set-window-dedicated-p (selected-window) nil)

;; Adjust the size of the frame 
(add-to-list 'default-frame-alist '(width  . 155))
(add-to-list 'default-frame-alist '(height . 50))

(set-face-attribute 'default nil :height 125)
(set-face-attribute 'mode-line-buffer-id nil :foreground "white")

(set-default 'truncate-lines nil)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Hide the UI
(tool-bar-mode 0)
(menu-bar-mode 0)

(if (display-graphic-p)
    (scroll-bar-mode 0))

(setq shell-file-name
      (if (eq system-type 'darwin)
          "/bin/zsh"
        (if (eq system-type 'windows-nt)
            "C:/Program Files/nu/bin/nu.exe"
          "/bin/bash")))

;; If we're on macOS, swap the meta key from OPT to CMD and reenable
;; menu-bar-mode since the global menu is already out of the way
(if (and (eq system-type 'darwin) (display-graphic-p))
    (progn
      (setq mac-command-modifier 'meta
            mac-option-modifier 'super)
	  (menu-bar-mode t)))

(setq-default default-directory "~/")

;; Copypasta'd from
;; https://emacs.stackexchange.com/questions/17210/how-to-place-all-auto-save-files-in-a-directory
;; TODO: Though this doesn't seem to work?
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves/"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

;; Disable beeping, thanks to https://github.com/sban/emacs
(setq visible-bell nil
	  ring-bell-function
	  '(lambda ()
		 (invert-face 'mode-line)
		 (run-with-timer 0.1 nil #'invert-face 'mode-line)))

(use-package use-package
  :ensure t
  :custom
  (package-native-compile t))

(use-package package
  :defer t
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (if (< emacs-major-version 27)
	  (package-initialize)))

(use-package doom-themes
  :if (display-graphic-p)
  :ensure t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  (doom-themes-treemacs-theme "doom-colors")
  :config
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(setq custom-enabled-themes '(misterioso))

;; Checks the hour every 30 mins and sees if we should use
;; daytime or nighttime
(setq pmh/current-theme nil)

(setq pmh/terminal-theme 'misterioso)
(setq pmh/light-theme 'doom-bluloco-light)
(setq pmh/dark-theme 'doom-bluloco-dark)
(setq pmh/daytime-hours '(6 16))

(defun pmh/update-theme-based-on-time ()
  (interactive)
  (let ((daytime-start (car pmh/daytime-hours))
	  (daytime-end (car (cdr pmh/daytime-hours)))
	  (active-theme pmh/current-theme)
	  (hour (string-to-number (substring (current-time-string) 11 13))))
	(setq pmh/current-theme
		  (if (member hour (number-sequence daytime-start daytime-end))
			pmh/light-theme
			pmh/dark-theme))
	(if (not (equal active-theme pmh/current-theme))
		(load-theme pmh/current-theme t nil))))

(if (display-graphic-p)
    (run-with-timer 0 1800 'pmh/update-theme-based-on-time)
  (load-theme pmh/terminal-theme))

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  :config
  (column-number-mode 1)
  :custom
  (doom-modeline-height 30)
  (doom-modeline-window-width-limit nil)
  (doom-modeline-battery t))

(use-package org
  :ensure t
  :defer t
  :hook
  ((org-mode . auto-fill-mode) (org-mode . org-modern-mode))
  :custom
  (org-directory
   (if (or (eq system-type 'darwin) (eq system-type 'gnu/linux))
	   "~/Documents/org"
	 (if (eq system-type 'windows-nt)
		 "~/iCloudDrive/Documents/org")))
  
  (org-support-shift-select t)
  (org-startup-truncated nil)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-insert-heading-respect-content t)
  (org-agenda-files (list org-directory))
  (org-image-actual-width 120)
  (epg-pinentry-mode 'loopback nil nil))

(defun org-disable-autosave-for-file ()
  (interactive)
  (insert "# -*- buffer-auto-save-file-name: nil; -*-"))

(use-package org-tree-slide
  :ensure t
  :defer t)

(use-package ox-pandoc
  :ensure t
  :defer t)

(use-package org-jira
  :ensure t
  :defer t)

(use-package cloc
  :ensure t
  :defer t)

(use-package markdown-mode
  :ensure t
  :defer t)

(use-package groovy-mode
  :ensure t
  :defer t)

(use-package nerd-icons
  :ensure t
  :defer t)

(use-package pandoc
  :ensure t
  :defer t)

(use-package obsidian
  :ensure t
  :defer t)

;; (use-package easysession
;;   :ensure t
;;   :defer t)

(setq-default fill-column 80)

(use-package vertico-posframe
  :ensure t)

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :custom
  (vertico-sort-function 'vertico-sort-history-alpha)
  (vertico-posframe-mode 1))

(use-package which-key
  :defer t
  :ensure t
  :config
  (which-key-mode)
  :custom
  (which-key-max-description-length 40)
  (which-key-lighter nil)
  (which-key-sort-order 'which-key-description-order))

(use-package nov
  :defer t
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(use-package ediff
  :ensure t
  :custom
  (ediff-keep-variants nil)
  (ediff-split-window-function 'split-window-vertically)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package nerd-icons
  :ensure t
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package treemacs
  :if (display-graphic-p)
  :ensure t
  :bind (("C-t s" . treemacs-switch-workspace) ("C-t h" . treemacs))
  :hook
  ((after-init . treemacs) (treemacs . treemacs-follow-project-mode))
  :custom
  (treemacs-show-hidden-files nil))

(use-package treemacs-nerd-icons
  :if (display-graphic-p)
  :ensure t
  :after treemacs
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package alert
  :ensure t
  :defer t
  :custom
  (alert-default-style 'notifier))

(use-package spacious-padding
  :ensure t
  :hook (after-init . 'spacious-padding))

(use-package dashboard
  :ensure t
  :custom
  (dashboard-banner-logo-title "EMACS!")
  (dashboard-startup-banner 'logo)
  (dashboard-center-content t)
  (dashboard-vertically-center-content t)
  (dashboard-display-icons-p t)
  (dashboard-navigation-cycle t)
  (dashboard-week-agenda t)
  (dashboard-filter-agenda-entry 'dashboard-filter-agenda-by-todo)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-items '((recents . 5)
                     (bookmarks . 5)
                     (projects . 4)
                     (agenda . 10)))
  
  :config
  (dashboard-setup-startup-hook)
  (dashboard-open))

(use-package clang-format
  :ensure t
  :defer t
  :bind
  (("C-;" . clang-format-region)
   ("C-:" . clang-format-buffer)))

(use-package dirvish
  :ensure t
  :defer t
  :init
  (dirvish-override-dired-mode))

;; Very neat company mode config taken from here
;; https://www.reddit.com/r/emacs/comments/8z4jcs/tip_how_to_integrate_company_as_completion/
(use-package company
  :ensure t
  :defer t
  :diminish
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay .1)
  (company-minimum-prefix-length 2)
  (company-show-numbers t)
  (company-tooltip-align-annotations 't)
  (global-company-mode t))

(use-package company-box
  :ensure t
  :defer t
  :after company
  :diminish
  :hook
  ((company-mode . company-box-mode)
   (after-init . global-company-mode)))

(use-package magit
  :ensure t
  :defer t
  :bind ("C-x C-g" . magit))

(use-package rust-mode
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode)))

(use-package yaml-mode
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode)))

(use-package slime
  :ensure t
  :defer t
  :custom
  (inferior-lisp-program "sbcl")
  :init
  (add-to-list 'auto-mode-alist '("\\.lisp\\'" . slime)))

(use-package lsp-mode
  :ensure t
  :defer t
  :bind ("M-RET" . lsp-execute-code-action)
  :init
  (add-to-list 'auto-mode-alist '("\\.cs\\'" . lsp)))

(use-package typescript-mode
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode)))

(use-package nushell-mode
  :ensure t
  :defer t)

(redraw-display)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t :background "#2d3743")))
 '(header-line ((t :box (:line-width 4 :color "#808080" :style nil))))
 '(header-line-highlight ((t :box (:color "#e1e1e0"))))
 '(keycast-key ((t)))
 '(line-number ((t :background "#2d3743")))
 '(mode-line ((t :box (:line-width 6 :color "#212931" :style nil))))
 '(mode-line-active ((t :box (:line-width 6 :color "#212931" :style nil))))
 '(mode-line-highlight ((t :box (:color "#e1e1e0"))))
 '(mode-line-inactive ((t :box (:line-width 6 :color "#878787" :style nil))))
 '(tab-bar-tab ((t :box (:line-width 4 :color "grey85" :style nil))))
 '(tab-bar-tab-inactive ((t :box (:line-width 4 :color "grey75" :style nil))))
 '(tab-line-tab ((t)))
 '(tab-line-tab-active ((t)))
 '(tab-line-tab-inactive ((t)))
 '(vertical-border ((t :background "#2d3743" :foreground "#2d3743")))
 '(window-divider ((t (:background "#2d3743" :foreground "#2d3743"))))
 '(window-divider-first-pixel ((t (:background "#2d3743" :foreground "#2d3743"))))
 '(window-divider-last-pixel ((t (:background "#2d3743" :foreground "#2d3743")))))

(custom-set-variables
 '(package-selected-packages
   '(nushell-mode yaml-mode wfnames websocket vertico-posframe typescript-mode treemacs-tab-bar treemacs-nerd-icons theme-changer swift-mode spacious-padding slime-company rust-mode rebecca-theme popup pandoc ox-pandoc org-tree-slide org-roam org-noter org-modern org-jira obsidian oauth2 nu-mode nov nix-mode nix-buffer magit lsp-mode kanban kanagawa-theme indent-bars highlight-indent-guides groovy-mode emojify easysession doom-themes doom-modeline dirvish dired-git dashboard csv-mode company-irony company-box cloc clang-format circe astro-ts-mode anki-mode alert)))
