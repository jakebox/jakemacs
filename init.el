;; Jake's Super Simple Emacs init.el (jakemacs)

(scroll-bar-mode -1)
(tool-bar-mode -1)

;;; Package setup
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(package-initialize)
(setq package-enable-at-startup nil)

(dolist (package '(use-package))
   (unless (package-installed-p package)
       (package-install package)))

(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-verbose nil)

;;; Optimization
(use-package gcmh
  :config
  (setq gcmh-idle-delay 5
        gcmh-high-cons-threshold (* 16 1024 1024))  ; 16mb
  (gcmh-mode 1))

;;; Visuals --------
(set-face-attribute 'default nil :font "Roboto Mono" :height 140)

(use-package modus-themes
  :init (modus-themes-load-themes)
  :config (modus-themes-load-operandi)
  (setq modus-themes-bold-constructs t)
  :bind ("<f5>" . modus-themes-toggle))


;;; Keyboard --------
(setq mac-command-modifier 'meta
      mac-option-modifier nil)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-x C-r") 'counsel-recentf)

;;; Basics -----------
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs 'y-or-n-p)
(setq scroll-conservatively 101) ;; Scroll screen with cursor
(setq-default display-line-numbers-width 3)
(fringe-mode '(8 . 8))
(setq-default truncate-lines t)
(setq-default tab-width 4)


(recentf-mode 1)
(show-paren-mode)
(line-number-mode)
(column-number-mode)
(global-hl-line-mode)
(global-display-line-numbers-mode t)


;; Packages
(use-package ivy
  :config
  (ivy-mode 1))

(use-package org
  :hook (org-mode . visual-line-mode)
  :hook (org-mode . org-indent-mode))

(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-leading-bullet ?\s))

(use-package which-key
  :init
  (which-key-mode)
  (which-key-setup-minibuffer)
  :config
  (setq which-key-idle-delay 0.3))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(counsel ivy gcmh which-key use-package org-superstar modus-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
