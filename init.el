(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(misterioso)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq inhibit-startup-screen t)

(set-face-attribute 'default nil :height 105)

;; Hide the UI, unless we're on MacOS since the global menu is already out of the way
(tool-bar-mode 0)
(let ((is-mac (or (eq system-type 'darwin) 0)))
  (menu-bar-mode is-mac))

;; TODO: Move this out into its own file when necessary.
(defun get-system-slash ()
  "Returns the system-specific slash for the local system"
  (if (eq system-type 'windows-nt)
      '"\\"
      '"/"
    )
  )

(if (eq system-type 'darwin)
    (setq shell-file-name "/bin/zsh")
  (if (eq system-type 'windows-nt)
      (setq shell-file-name "C:/Program Files/nu/bin/nu.exe"))
  )

(setq inferior-lisp-program "sbcl")

(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "auto-save/") t)))

;;(setq default-directory (concat (getenv "HOME") (get-system-slash)))
(setq default-directory "~/")

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
