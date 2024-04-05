(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(misterioso))
 '(inferior-lisp-program "sbcl")
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(tool-bar-mode 0)

(if (eq system-type 'darwin)
    (menu-bar-mode 0))

(if (eq system-type 'darwin)
    (setq shell-file-name "/bin/zsh")
  (if (eq system-type 'windows-nt)
      (setq shell-file-name "C:/Program Files/nu/bin/nu.exe"))
  )

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
