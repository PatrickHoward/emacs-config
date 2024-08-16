(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-tokyo-night t)
  
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  (setq doom-themes-treemacs-theme "doom-one") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(tab-bar-mode t)
(scroll-bar-mode 0)

(add-to-list 'default-frame-alist '(width  . 155))
(add-to-list 'default-frame-alist '(height . 50))

(set-face-attribute 'default nil :height 125)
(set-face-attribute 'mode-line-buffer-id nil :foreground "white")

(set-default 'truncate-lines nil)
(setq-default tab-width 4)

;; Hide the UI, unless we're on MacOS since the global menu is already out of the way
(tool-bar-mode 0)
(menu-bar-mode 0)

(if (eq system-type 'darwin)
    (progn
      (setq shell-file-name "/bin/zsh")
      (setq mac-command-modifier 'meta)
      (setq mac-option-modifier 'super)
	  (menu-bar-mode t))
  (if (eq system-type 'windows-nt)
      (setq shell-file-name "C:/Program Files/nu/bin/nu.exe")))

(setq inferior-lisp-program "sbcl")

(setq default-directory "~/")

;; Disable beeping, thanks to https://github.com/sban/emacs
(setq visible-bell nil
      ring-bell-function 'flash-mode-line)

(defun flash-mode-line ()
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))

(use-package package
  :defer 5
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize))

(use-package nerd-icons
  :custom 
 (nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package treemacs
  :defer t
  :hook
  ((after-init . treemacs)(treemacs . treemacs-follow-project-mode))
  :config
  (setq treemacs-show-hidden-files nil))

(use-package treemacs-nerd-icons
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config (column-number-mode 1)
  :custom
  (doom-modeline-height 30)
  (doom-modeline-window-width-limit nil)
  (doom-modeline-battery t))

(use-package alert
  :defer 10
  :config
  (setq alert-default-style 'notifier))

(use-package spacious-padding
  :defer t
  :hook (emacs-startup . #'spacious-padding))

(use-package dashboard
  :ensure t
  :config
  (setq dashboard-banner-logo-title "EMACS!")
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content t)
  (setq dashboard-vertically-center-content t)
  (setq dashboard-display-icons-p t)
  (setq dashboard-icon-type 'nerd-icons)
  (dashboard-setup-startup-hook))

(use-package org
  :defer 20
  :hook ((org-mode . #'auto-fill-mode)
		 (org-agenda-finalize . #'org-modern-agenda))
  :config
  (setq org-support-shift-select t)
  (setq org-directory
		(if (eq system-type 'darwin)
			"~/Documents/org"
		  (if (eq system-type 'windows-nt)
			  "~/iCloudDrive/Documents/org")))
  (setq org-startup-truncated nil)
  (setq org-hide-emphasis-markers t)
  (setq org-pretty-entities t)
  (setq org-insert-heading-respect-content t)
  (setq org-agenda-include-diary t))

(with-eval-after-load 'org (global-org-modern-mode))

(use-package recentf
  :defer t
  :init
  (recentf-mode t)
  :bind ("C-x C-r" . recentf-open-files)
  :config
  (setq recentf-max-menu-items 5)
  (setq recentf-max-saved-items 5))

(defun kill-selected-text ()
  (interactive)
  (if (region-active-p)
      (call-interactively 'kill-region)
    (call-interactively 'kill-line)))

(global-set-key (kbd "C-k") 'kill-selected-text)

(add-hook 'after-init-hook 'global-company-mode)

