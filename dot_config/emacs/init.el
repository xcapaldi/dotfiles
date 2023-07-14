(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(tool-bar-mode -1)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package emacs
  :config
  (load-theme 'modus-operandi t)
  (when (equal system-type 'darwin)
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier 'super)
    (when (member "Monaco" (font-family-list))
          (add-to-list 'initial-frame-alist '(font . "Monaco-13:regular"))
          (add-to-list 'default-frame-alist '(font . "Monaco-13:regular")))
    (set-fontset-font t 'symbol (font-spec :family "Apple Symbols") nil 'prepend)
    (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend)
    (setq-default line-spacing 0.1)))

(use-package asdf
  :load-path "~/Checkout/asdf.el"
  :if (eq system-type 'darwin)
  :config (asdf-enable))

(use-package chezmoi
  :ensure t)

(use-package consult
  ;; Enable automatic preview at point in the *Completions* buffer.
  ;; This is relevant when you use the default completion UI,
  ;; and not necessary for Vertico, Selectrum, etc.
  :ensure t
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind (:map isearch-mode-map
  ("C-c l" . consult-line)))

(use-package copilot
  :load-path "~/Checkout/copilot.el"
  :if (eq system-type 'darwin)
  :hook (prog-mode . copilot-mode))

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode))

(use-package dumb-jump
  :ensure t
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package editorconfig
  :ensure t
  :config (editorconfig-mode 1))

(use-package eglot
  :ensure t
  :after (project flymake xref)
  :commands eglot)

(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :init (exec-path-from-shell-initialize))

(use-package go-mode
  :ensure t
  :mode (("\\.go\\'" . go-mode)
         ("\\.mod\\'" . go-dot-mod-mode)))

(use-package js
  :ensure nil
  :mode "\\.js[x]\\'")

(use-package magit
  :ensure t)

(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init (marginalia-mode 1))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package org
  :config
  ;; Org-babel supported languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (python . t)))
  :custom
  (org-adapt-indentation nil)
  (org-edit-src-content-indentation 0))

(use-package protobuf-mode
  :ensure t
  :if (eq system-type 'darwin)
  :mode ("\\.proto\\'"))

(use-package terraform-mode
  :ensure t
  :if (eq system-type 'darwin)
  :mode ("\\.tf\\'" . terraform-mode))

(use-package typescript-mode
  :ensure t
  :mode "\\.ts[x]\\'")

(use-package vertico
  :ensure t
  :init (vertico-mode 1))

(use-package vundo
  :ensure t
  :commands (vundo)
  :bind ("C-c u" . vundo))

(use-package which-key
  :ensure t
  :custom
  ;; Allow C-h to trigger which-key before it is done automatically
  (which-key-show-early-on-C-h t)
  ;; Don't show normally but refresh quickly when triggered
  (which-key-idle-delay 10000)
  (which-key-idle-secondary-delay 0.05)
  :init (which-key-mode))
