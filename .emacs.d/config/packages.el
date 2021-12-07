;; Packages
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; ivy
(use-package ivy
;;  :diminish
  :bind (
         :map ivy-switch-buffer-map
         ("k" . ivy-switch-buffer-kill))
  :init
  (ivy-mode 1))

;; ivy-rich
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; counsel
(use-package counsel
  :bind ("M-x" . counsel-M-x)
  :config
  (setq ivy-initial-inputs-alist nil))

;; doom-modline
(use-package all-the-icons)
(setq inhibit-compacting-font-caches t)
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom (doom-modeline-height 10))
(doom-modeline-mode 1)
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-tomorrow-night t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; rainbow-delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
;; whitch-key
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.1))

;; helpful
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; general
(use-package general
  :config
  (general-create-definer pxl/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "M-SPC")

  (pxl/leader-keys
    "t"  '(:ignore f :which-key "test")
    "tt" '(counsel-load-theme :which-key "choose theme")))

;;(when window-system
;;  (load "~/.emacs.d/config/evil.el"))
