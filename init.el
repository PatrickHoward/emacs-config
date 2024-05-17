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
(let ((is-mac (or (eq system-type 'darwin) 0)))
  (menu-bar-mode is-mac))

(if (eq system-type 'darwin)
    (progn
      (setq shell-file-name "/bin/zsh")
      (setq mac-command-modifier 'meta)
      (setq mac-option-modifier 'super))
  (if (eq system-type 'windows-nt)

      (setq shell-file-name "C:/Program Files/nu/bin/nu.exe")))

(setq inferior-lisp-program "sbcl")

(let ((auto-save-dir (concat user-emacs-directory "auto-save/"))) 
  (if (not (file-directory-p auto-save-dir))
      (make-directory auto-save-dir t))
  (setq auto-save-file-name-transforms
	`(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" , (concat auto-save-dir "\\2") t))))

(setq default-directory "~/")

;; Disable beeping, thanks to https://github.com/sban/emacs
(setq visible-bell nil
      ring-bell-function 'flash-mode-line)

(defun flash-mode-line ()
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))

(winner-mode t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Configure Treemacs
(require 'treemacs)
(add-hook 'emacs-startup-hook 'treemacs)
(setq treemacs-show-hidden-files nil)

;; Configure ue-mode
(require 'ue)
(define-key ue-mode-map (kbd "C-c u") 'ue-command-map)
(ue-global-mode +1)

(require 'alert)
(setq alert-default-style 'notifier)

(require 'spacious-padding)
(add-hook 'emacs-startup-hook 'spacious-padding-mode)

(require 'dashboard)
(dashboard-setup-startup-hook)
(setq dashboard-banner-logo-title "EMACS!")
(setq dashboard-startup-banner 'logo)
(setq dashboard-center-content t)
(setq dashboard-vertically-center-content t)
(setq dashboard-display-icons-p t)
(setq dashboard-icon-type 'nerd-icons)


;; Configure Org Mode
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

(add-hook 'org-mode-hook #'org-modern-mode)
(add-hook 'org-mode-hook #'auto-fill-mode)
(add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
(setq org-agenda-include-diary t)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 5)
(setq recentf-max-saved-items 5)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

(message "Initalization Complete!")

(defun kill-selected-text ()
  (interactive)
  (if (region-active-p)
      (call-interactively 'kill-region)
    (call-interactively 'kill-line)))

(global-set-key (kbd "C-k") 'kill-selected-text)
