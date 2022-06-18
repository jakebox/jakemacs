;; Jake's Super Simple Emacs init.el (jakemacs)
;; includes evil.el (Vim emulation)

(scroll-bar-mode -1)
(tool-bar-mode -1)

;;; Package setup -----
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(setq package-enable-at-startup nil)
(require 'use-package)
(setq use-package-always-ensure t)

;;; Visuals --------
(set-face-attribute 'default nil :font "Monaco" :height 140)

(use-package modus-themes
  :init (modus-themes-load-themes)
  :config
  (setq modus-themes-bold-constructs t
		modus-themes-mode-line '(accented 2 borderless))
  (modus-themes-load-operandi)
  :bind ("<f5>" . modus-themes-toggle)) ;; Press <f5> to toggle dark/light

;;; Keyboard --------
(setq mac-command-modifier 'meta
      mac-option-modifier  nil)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-x C-r") 'counsel-recentf)

(use-package evil
  :init
  (setq evil-want-fine-undo t
		evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line))

;;; Basics -----------
(setq use-short-answers t)					;; Use "y" and "n" rather than "yes" and "no"
(setq scroll-conservatively 101)			;; Scroll screen with cursor
(fringe-mode '(8 . 8))						;; Window margin
(setq-default truncate-lines t)				;; Don't wrap text by default
(setq-default tab-width 4)					;; Tab width
(setq visible-bell nil)						;; Make it ring (so no visible bell) (default)
(setq ring-bell-function 'ignore)			;; BUT ignore it, so we see and hear nothing

(recentf-mode 1)
(show-paren-mode)
(line-number-mode)
(column-number-mode)
(global-hl-line-mode)
(global-display-line-numbers-mode t)


;;; Packages ----------
(use-package undo-fu
  :config
  (define-key evil-normal-state-map (kbd "u") 'undo-fu-only-undo)
  (define-key evil-normal-state-map (kbd "U") 'undo-fu-only-redo))

(use-package ivy
  :config
  (ivy-mode 1))

(use-package counsel
  :config
  (counsel-mode 1))

(use-package org
  :hook (org-mode . visual-line-mode)
  :hook (org-mode . org-indent-mode))

(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-leading-bullet ?\s))

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
