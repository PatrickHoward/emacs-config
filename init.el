(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.nnnnnnnnnnn
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(rebecca)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(tab-bar-mode t)
(scroll-bar-mode 0)
(setq inhibit-startup-screen t)

(add-to-list 'default-frame-alist '(width  . 155))
(add-to-list 'default-frame-alist '(height . 50))

(set-face-attribute 'default nil :height 125)
(set-face-attribute 'mode-line-buffer-id nil :foreground "white")

(set-default 'truncate-lines nil)
(setq-default tab-width 4)

;; Hide the UI, unless we're on MacOS since the global menu is already out of the way
(tool-bar-mode 0)

(if (eq system-type 'darwin)
    (progn
      (setq shell-file-name "/bin/zsh")
      (setq mac-command-modifier 'meta)
      (setq mac-option-modifier 'super)
	  (menu-bar-mode t))
  (if (eq system-type 'windows-nt)
      (setq shell-file-name "C:/Program Files/nu/bin/nu.exe")))

(setq inferior-lisp-program "sbcl")

;;(let ((auto-save-dir (concat user-emacs-directory "auto-save/")))
;;    (if (not (file-directory-p auto-save-dir))
;;      (make-directory auto-save-dir t))
;;  (setq auto-save-file-name-transforms
;;  `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" , (concat auto-save-dir "\\2") t)))
;;  (setq backup-directory-alist `(("." . ,(concat auto-save-dir "backups")))))

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

;; Configure Treemacs
(use-package treemacs
  :defer t
  :hook (emacs-startup . treemacs)
  :config
  (setq treemacs-show-hidden-files nil))

(use-package alert
  :defer 10
  :config
  (setq alert-default-style 'notifier))

(use-package spacious-padding
  :defer t
  :hook (emacs-startup . #'spacious-padding))

(use-package dashboard
  :defer t
  :init
  (dashboard-setup-startup-hook)
  :config
  (setq dashboard-banner-logo-title "EMACS!")
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content t)
  (setq dashboard-vertically-center-content t)
  (setq dashboard-display-icons-p t)
  (setq dashboard-icon-type 'nerd-icons))

(use-package org
  :defer 20
  :hook ((org-mode . #'org-modern-mode)
		 (org-mode . #'auto-fill-mode)
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
