;;
;;; For fast config
(defun my/configure ()
  "Opens user-init-file"
  (interactive)
  (find-file user-init-file))
(global-set-key (kbd "M-<f1>") 'my/configure)
(defun my/scratch ()
  (interactive)
  (find-file "~/.emacs.d/config/etc.el")
  )
(global-set-key (kbd "M-<f2>") 'my/scratch)

;;; Packages
;;;; Packages config
;;;;; packages
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
(setq custom-file "~/.emacs.d/custom.el")

;;;; General setings
;;;;; emacs
(use-package emacs
  :ensure nil

  :init
  (defalias 'yes-or-no-p 'y-or-n-p)
  (put 'narrow-to-region 'disabled nil)
  
  :custom
  (inhibit-startup-screen t "No startup screen")
  ;;(indicate-empty-lines t)
  (use-dialog-box nil "Disable dialog boxes")
  (enable-recursive-minibuffers t "Allow minibuffer commands in the minibuffer")
  (x-select-enable-clipboard t "Use clipboard")
  (x-select-enable-primary t "Use primary buffer")
  (save-interprogram-paste-before-kill t)
  (mouse-yank-at-point t "Yank at point using mouse")
  (resize-mini-windows t)
  (x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
  ;; Window
  (tool-bar-mode nil)
  (scroll-bar-mode nil)
  (menu-bar-mode nil)
  ;;(frame-title-format "Emacs: %b")
  ;; Cursor
  ;;(line-number-mode t)
  (column-number-mode)
  (global-display-line-numbers-mode t)
  (blink-cursor-mode nil)
  (x-stretch-cursor t)
  ;;(mode-line-position '((line-number-mode ("%l" (column-number-mode ":%c")))))
  ;;(shift-select-mode nil "No selection with <shift>")
  ;; Exit confirmation
  (kill-emacs-query-functions
   (cons (lambda () (yes-or-no-p "Really Quit Emacs? "))
         kill-emacs-query-functions))

  :config
  ;;(global-prettify-symbols-mode)
  (prefer-coding-system 'utf-8)
  (put 'overwrite-mode 'disabled t))

;;;;; faces
(use-package faces
  :ensure nil

  :preface
  (setq
   my/faces/size 18

   ;; TODO: make the font selection more robust
   my/faces/fixed-family "Fira Code Retina"

   my/faces/variable-family
   (if (string-equal system-type "darwin")
       "Fira Code Retina"
     "DejaVu Serif"))

  :diminish (buffer-face-mode "")

  :config
  (set-face-attribute
   'variable-pitch nil
   :font
   (font-spec
    :family my/faces/variable-family
    :size my/faces/size))

  (set-face-attribute
   'fixed-pitch nil
   :font
   (font-spec
    :family my/faces/fixed-family
    :size my/faces/size))

  (set-face-attribute
   'default nil
   :font
   (font-spec
    :family my/faces/fixed-family
    :size my/faces/size)))

;;;;; paren
(use-package paren
  :ensure nil

  :config
  (show-paren-mode t))
;;;;; visual-line
(use-package visual-line
  :ensure nil

  :custom
  (visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

  :hook
  (text-mode . visual-line-mode))

;;;;; reverse-im
(use-package reverse-im
  :demand

  :diminish

  :config
  (reverse-im-activate "russian-computer"))

;;;;; unkillable-scratch
(use-package unkillable-scratch
  :preface
  (defun my/switch-to-scratch ()
    (interactive)
    (switch-to-buffer "*scratch*"))

  :hook
  (after-init . unkillable-scratch))
;;;; Beauty 
;;;;; ivy
(use-package ivy
;;  :diminish
  :bind (
         :map ivy-switch-buffer-map
         ("k" . ivy-switch-buffer-kill))
  :init
  (ivy-mode 1))

;;;;; ivy-rich
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;;;;; counsel
(use-package counsel
  :bind ("M-x" . counsel-M-x)
  :config
  (setq ivy-initial-inputs-alist nil))

;;;;; all-the-icons
(use-package all-the-icons)
(setq inhibit-compacting-font-caches t)
;;;;; telephone-modeline
(use-package telephone-line)
(setq telephone-line-lhs
      '((evil   . (telephone-line-evil-tag-segment))
        (accent . (telephone-line-vc-segment
                   telephone-line-erc-modified-channels-segment
                   telephone-line-process-segment))
        (nil    . (telephone-line-minor-mode-segment
                   telephone-line-buffer-segment))))
(setq telephone-line-rhs
      '((nil    . (telephone-line-misc-info-segment))
        (accent . (telephone-line-major-mode-segment))
        (evil   . (telephone-line-airline-position-segment))))

(setq telephone-line-primary-left-separator 'telephone-line-tan-left
      telephone-line-secondary-left-separator 'telephone-line-tan-hollow-left
      telephone-line-primary-right-separator 'telephone-line-tan-right
      telephone-line-secondary-right-separator 'telephone-line-tan-hollow-right)
(setq telephone-line-height 24
      telephone-line-evil-use-short-tag t)
(telephone-line-mode 1)
;;;;; doom-themes
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
  ;;all-the-icons-install-fonts
;;;;; rainbow-delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;;;; whitch-key
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.1))

;;;;; helpful
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;;;;; outshine
(use-package outshine
  :diminish
  (outline-minor-mode . "")
  (outshine-mode . "⒪")

  :bind
  (:map
   outline-minor-mode-map
   ([C-tab] . outshine-cycle-buffer))

  :hook
  (outline-minor-mode . outshine-mode)
  (prog-mode . outline-minor-mode)

  :custom
  (outshine-preserve-delimiter-whitespace t)
  (outshine-cycle-emulate-tab t)

  :config
  ;; unbind M-tab
  (unbind-key "C-M-i" outline-minor-mode-map))

;;;;; beacon
(use-package beacon
  :custom
  (beacon-mode 1))

;;;;; fireplace
(use-package fireplace)

;;;; Code
;;;;; magit
(use-package magit)

;;;;; markdown-mode
(use-package markdown-mode)

;;;; Spell Checking
;;;;; ispell
(use-package ispell
  :if (executable-find "hunspell")
  :ensure nil

  :commands
  (ispell-buffer)

  :custom
  (ispell-really-aspell nil)
  (ispell-really-hunspell t)
  (ispell-encoding8-command t)
  (ispell-program-name "hunspell")
  (ispell-dictionary "ru_RU,en_US")

  :config
  (when (executable-find "hunspell-wrapper")
    (setq-default ispell-program-name "hunspell-wrapper"))
  ;; ispell-set-spellchecker-params has to be called
  ;; before ispell-hunspell-add-multi-dic will work
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "ru_RU,en_US"))

;;;;; flyspell
(use-package flyspell
  :ensure nil

  :commands (flyspell-buffer flyspell-mode)

  :bind
  ("M-<f5>" . flyspell-buffer)
  ("M-<f8>" . flyspell-goto-next-error)
  (:map
   mode-specific-map
   ("s" . flyspell-correct-word-before-point)))

;;; Keybinding
;;;; Global keys
;;;;; unset keys
(global-unset-key (kbd "C-1"))
(global-unset-key (kbd "C-2"))
(global-unset-key (kbd "C-4"))
(global-unset-key (kbd "C-5"))
(global-unset-key (kbd "C-6"))
(global-unset-key (kbd "C-7"))
(global-unset-key (kbd "C-8"))
(global-unset-key (kbd "C-9"))
(global-unset-key (kbd "C-0"))

;;;;; set keys
(if (eq system-type 'gnu/linux)
    (progn
    (global-set-key (kbd "<f2>") 'counsel-switch-buffer)
    (global-set-key (kbd "<f5>") 'ispell-word)
    ))
(global-set-key (kbd "<escape>")  'keyboard-escape-quit)
(global-set-key (kbd "M-SPC") 'set-mark-command)
(global-set-key (kbd "C-c a") 'org-agenda)
;; 
(when window-system
  (load "~/.emacs.d/config/keys.el"))

;;; Other
;;;; Org mode
;(setq org-agenda-files (list "~/org/todo.org"))
(setq org-log-done 'time)
(setq-default org-display-custom-times t)
(setq org-time-stamp-custom-formats '("<%a %d.%m.%Y>" . "<%a %b %e %Y %H:%M>"))
(setq calendar-week-start-day 1)
(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)")
        (sequence "IMPORTANT(i)" "|" "LOST(l)")))
(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "magenta" :weight bold))
        ("DONE" . (:foreground "lime green" :weight bold))
        ("IMPORTANT" . (:foreground "deep pink" :weight bold))
        ("LOST" . (:foreground "OrangeRed1" :weight bold))
        ))
(setq org-priority-faces '((?A . (:foreground "red" :weight 'bold))
                           (?B . (:foreground "yellow"))
                           (?C . (:foreground "green"))))

;;;; Add to list
;;;;; modes
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;;;; Hooks
;;;;; Numbers line hook
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook
		shell-mode-hook))
  (add-hook mode(lambda () (display-line-numbers-mode 0))))

;;;; Сообщение в буфере scratch
(when window-system
  (load "~/.emacs.d/config/etc.el"))
;;;; Дополнения для конкретной машинки
(when window-system
  (load "~/.emacs.d/config/specific.el"))

;;;; funcs
;;;;; save and kill
(defun my/savekill ()
  (interactive)
  (save-buffer)
  (kill-this-buffer)
  )
(global-set-key (kbd "C-c C-c") 'my/savekill)

;;;; Hacks
;;;;; abbrev-mode
(dolist (hook '(erc-mode-hook
		text-mode-hook))
  (add-hook hook #'abbrev-mode))
