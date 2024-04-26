Config setup inspired by [this cool guy](https://www.reddit.com/r/emacs/comments/phb5sw/comment/hbhzwpo/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button). 

## Setup
Add this to your startup Emacs config (be that `.emacs` or `~/.config/emacs/`:
```
(setq load-path (append load-path
    (list (expand-file-name "~/Repos/emacs-config"))
))

(load "seutp.el")
(load "init.elc")
```
## M-x commands
* **install-config-packages**
