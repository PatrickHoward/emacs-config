(use-package doom-themes
  :if (display-graphic-p)
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

;;(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

(setq default-directory "~/")

;; Copypasta'd from https://emacs.stackexchange.com/questions/17210/how-to-place-all-auto-save-files-in-a-directory
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
      ring-bell-function 'flash-mode-line)

(defun flash-mode-line ()
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))

(use-package use-package
  :custom
  (use-package-always-ensure t)
  (package-native-compile t))

(use-package package
  :defer t
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (if (< emacs-major-version 27)
  (package-initialize)))

(require 'vertico-posframe)
(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-sort-function 'vertico-sort-history-alpha)
  (vertico-posframe-mode 1))

(use-package which-key
  :config
  (which-key-mode)
  :custom
  (which-key-max-description-length 40)
  (which-key-lighter nil)
  (which-key-sort-order 'which-key-description-order))

(use-package nov
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(use-package ediff
  :ensure nil
  :custom
  (ediff-keep-variants nil)
  (ediff-split-window-function 'split-window-vertically)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package nerd-icons
  :custom 
 (nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package treemacs
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
  :defer t
  :config
  (setq alert-default-style 'notifier))

(use-package spacious-padding
  :hook (emacs-startup . #'spacious-padding))

(use-package dashboard
  :ensure t
  :custom
  (dashboard-banner-logo-title "EMACS!")
  (dashboard-startup-banner 'logo)
  (dashboard-center-content t)
  (dashboard-vertically-center-content t)
  (dashboard-display-icons-p t)
  (dashboard-icon-type 'nerd-icons)
  :config
  (dashboard-setup-startup-hook)
  (dashboard-open))

(with-eval-after-load 'org (global-org-modern-mode))

(setq org-directory
		(if (eq system-type 'darwin)
			"~/Documents/org"
		  (if (eq system-type 'windows-nt)
			  "~/iCloudDrive/Documents/org")))

(use-package org
  :defer t
  :hook (org-mode . auto-fill-mode)
  :custom
  (org-support-shift-select t)
  (org-startup-truncated nil)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-insert-heading-respect-content t)
  (org-agenda-files (list (concat org-directory "/todo.org"))))

(setq-default fill-column 80)

(use-package recentf
  :defer t
  :init
  (recentf-mode t)
  :bind ("C-x C-r" . recentf-open-files)
  :config
  (setq recentf-max-menu-items 5)
  (setq recentf-max-saved-items 5))

(use-package clang-format
  :defer t
  :bind
  (("C-;" . clang-format-region)
   ("C-:" . clang-format-buffer)))

(require 'dirvish)
(dirvish-override-dired-mode)

(defun kill-selected-text ()
  (interactive)
  (if (region-active-p)
      (call-interactively 'kill-region)
    (call-interactively 'kill-line)))

;; Very neat company mode config taken from here
;; https://www.reddit.com/r/emacs/comments/8z4jcs/tip_how_to_integrate_company_as_completion/
(use-package company
  :defer 2
  :diminish
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay .1)
  (company-minimum-prefix-length 2)
  (company-show-numbers t)
  (company-tooltip-align-annotations 't)
  (global-company-mode t))

(use-package company-box
  :after company
  :diminish
  :hook (company-mode . company-box-mode))

(load "org-scratch.elc")

(global-set-key (kbd "C-k") 'kill-selected-text)
(global-set-key (kbd "C-x C-b") 'switch-to-buffer)
(global-set-key (kbd "C-x C-k") 'kill-buffer)
(global-set-key (kbd "C-SPC") 'set-mark-command)

(add-hook 'after-init-hook 'global-company-mode)

; When switching buffers, do not open a new window
(set-window-dedicated-p (selected-window) nil)

(redraw-display)
