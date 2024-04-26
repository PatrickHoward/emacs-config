(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.nnnnnnnnnnn
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(deeper-blue)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(tab-bar-mode t)
(setq inhibit-startup-screen t)

;; set transparency
;;(set-frame-parameter (selected-frame) 'alpha-background 98)
;;(add-to-list 'default-frame-alist '(alpha-background . 98))

(add-to-list 'default-frame-alist '(width  . 125))
(add-to-list 'default-frame-alist '(height . 50))

(set-face-attribute 'default nil :height 125)


;; Hide the UI, unless we're on MacOS since the global menu is already out of the way
(tool-bar-mode 0)
(let ((is-mac (or (eq system-type 'darwin) 0)))
  (menu-bar-mode is-mac))

;; TODO: Move this out into its own file when necessary.
(defun ge-system-slash ()
  "Returns the system-specific slash for the local system"
  (if (eq system-type 'windows-nt)
      '"\\"
      '"/"))

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

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Configure Treemacs
(require 'treemacs)
(setq treemacs-python-executable (executable-find "python"))
(add-hook 'emacs-startup-hook 'treemacs)

;; Configure ue-mode
(require 'ue)
(define-key ue-mode-map (kbd "C-c u") 'ue-command-map)
(ue-global-mode +1)

;; Configure Org Mode
(setq org-support-shift-select t)
(setq org-directory
      (if (eq system-type 'darwin)
	  "~/Documents/org"
	(if (eq system-type 'windows-nt)
	    "~/iCloudDrive/Documents/org")))

(message (concat "Hey, " (user-login-name) "! Get ready to coooode!"))
