;; Disable startup message
(setq inhibit-startup-message t)

;; Highlight
(setq show-paren-style 'expression)
(show-paren-mode 2)

;; Disable bars
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

(set-fringe-mode 10)

;; Font
(set-face-attribute 'default nil :font "Fira Code Retina" :height 120)

;; Numbers of line
(column-number-mode)
(global-display-line-numbers-mode t)
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook
		shell-mode-hook))
  (add-hook mode(lambda () (display-line-numbers-mode 0))))

;; Packages
(when window-system
  (load "~/.emacs.d/config/packages.el"))
;; Key binding
(when window-system
  (load "~/.emacs.d/config/keybinding.el"))

;; Other
(when window-system
  (load "~/.emacs.d/config/etc.el"))
(put 'downcase-region 'disabled nil)
