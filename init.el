(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(misterioso))
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(set-face-attribute 'default nil :height 105)

(tool-bar-mode 0)
(menu-bar-mode (not (eq system-type 'darwin)))

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

;;(setq default-directory (concat (getenv "HOME") (get-system-slash)))
(setq default-directory "~/")

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
