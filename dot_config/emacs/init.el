;;; init.el --- Emacs configuration
;;; Commentary:

;;; Code:...


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
  :init
  (make-directory "~/.config/emacs/autosaves/" t)
  (make-directory "~/.config/emacs/backups/" t)
  :custom
  (auto-save-file-name-transforms '((".*" "~/.config/emacs/autosaves/\\1" t)))
  (backup-directory-alist '((".*" . "~/.config/emacs/backups/")))
  (backup-by-copying t)
  (version-control t)
  (delete-old-versions nil)
  (kept-new-versions 20)
  (kept-old-versions 5)
  :config
  (setq-default
   coding-system-for-read 'utf-8                ; Use UTF-8 by default
   coding-system-for-write 'utf-8
   cursor-in-non-selected-windows t             ; Don't hide the cursor in inactive windows
   help-window-select t                         ; Focus new help windows when opened
   indent-tabs-mode nil                         ; Prefer spaces over tabs
   indicate-empty-lines t                       ; Display bitmap in left fringe on empty lines
   inhibit-startup-screen t                     ; Remove default start screen
   ring-bell-function 1                         ; Use a visual bell
   select-enable-clipboard t                    ; Merge emacs and system clipboard
   tab-always-indent 'complete                  ; Use tab as a completion instead of C-M-i
   tab-width 4                                  ; Set width for tabs
   view-read-only t                             ; Always open read-only buffers in view-mode
   visible-bell t                               ; Use a visual bell
   vc-follow-symlinks t)                        ; Don't ask for confirmation following symlinked files
  (global-visual-line-mode 0)            ; Don't always wrap lines instead of extending past view
  (global-hl-line-mode 0)                ; Highlight current line
  (display-fill-column-indicator-mode 0) ; add indicator for the fill line
  (column-number-mode 1)                 ; Show the column number
  (fset 'yes-or-no-p 'y-or-n-p)          ; Replace yes/no prompts with y/n
  (tool-bar-mode 1)                      ; Hide the toolbar
  (menu-bar-mode 1)                      ; Hide the menubar
  (tooltip-mode 0)                       ; Remove mouse hover tooltips
  (scroll-bar-mode 1)                    ; Hide the scrollbar
  (blink-cursor-mode 0)                  ; Don't blink the cursor
  (show-paren-mode 1)                    ; Show matching parens
  (global-auto-revert-mode 1)            ; If file changes on disk, update the buffer automatically
  (load-theme 'modus-operandi t)
  (when (equal system-type 'darwin)
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier 'super)
    (when (member "Unifont" (font-family-list))
          (set-frame-font "Unifont-15:regular" nil t)
          (add-to-list 'initial-frame-alist '(font . "Unifont-15:regular"))
          (add-to-list 'default-frame-alist '(font . "Unifont-15:regular")))
    (set-fontset-font t 'symbol (font-spec :family "Apple Symbols") nil 'prepend)
    (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend)
    (setq-default line-spacing 0.1)))

(use-package apheleia
  :ensure t
  :init (apheleia-global-mode +1))

(use-package asdf
  :load-path "~/.config/emacs/site-lisp/asdf.el"
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
  :load-path "~/.config/emacs/site-lisp/copilot.el"
  :if (eq system-type 'darwin)
  :hook (prog-mode . copilot-mode))

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode))

(use-package dired
  :custom
  ;; --list-directories-first
  ((dired-listing-switches "-alh"))
  :config
  (put 'dired-find-alternate-file 'disabled nil))

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

(use-package ibuffer
  :bind ([remap list-buffers] . ibuffer)) ;; C-x C-b

(use-package js
  :ensure nil
  :mode "\\.js[x]\\'")

(use-package linum
  :hook (prog-mode . linum-mode))

(use-package magit
  :ensure t)

(use-package magit-todos
  :ensure t
  :hook magit-mode)

(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init (marginalia-mode 1))

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode))

(use-package minions
  :ensure t
  :init (minions-mode 1)
  :custom
  (minions-prominent-modes '(flymake-mode)))

(use-package ob-go
  :ensure t
  :after org-mode)

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

(use-package pulsar
  :ensure t
  :bind (("C-c h p" . pulsar-pulse-line)
         ("C-c h h" . pulsar-highlight-dwim))
  :init (pulsar-global-mode 1)
  :custom
  (pulsar-pulse t)
  (pulsar-delay 0.055)
  (pulsar-iterations 10)
  (pulsar-face 'pulsar-magenta)
  (pulsar-highlight-face 'pulsar-yellow)
  (pulsar-pulse-functions '(recenter-top-bottom
                            move-to-window-line-top-bottom
                            reposition-window
                            bookmark-jump
                            other-window
                            ace-window
                            delete-window
                            delete-other-windows
                            forward-page
                            backward-page
                            scroll-up-command
                            scroll-down-command
                            windmove-right
                            windmove-left
                            windmove-up
                            windmove-down
                            windmove-swap-states-right
                            windmove-swap-states-left
                            windmove-swap-states-up
                            windmove-swap-states-down
                            tab-new
                            tab-close
                            tab-next
                            org-next-visible-heading
                            org-previous-visible-heading
                            org-forward-heading-same-level
                            org-backward-heading-same-level
                            outline-backward-same-level
                            outline-forward-same-level
                            outline-next-visible-heading
                            outline-previous-visible-heading
                            outline-up-heading))

  (pulsar-global-mode 1))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode. rainbow-delimiters-mode))

(use-package subword
  :hook (prog-mode . subword-mode))

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

(use-package whitespace
  :init (global-whitespace-mode 1)
  :custom
  (whitespace-style (quote (face trailing missing-newline-at-eof empty tab-mark)))
  (whitespace-display-mappings
   '(
     ;;(space-mark 32 [183] [46]) ; SPACE 32 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
     ;;(newline-mark 10 [8595 10]) ; LINE FEED,
     (tab-mark 9 [9482 9] [92 9]) ; tab BOX DRAWINGS LIGHT QUADRUPLE DASH VERTICAL 「┊」
     ;;(tab-mark 9 [9500 9472 9472 9472]) ; tab BOX DRAWINGS LIGHT VERTICAL AND RIGHT 「├」 BOX DRAWINGS LIGHT HORIZONTAL 「─」
     ;;(tab-mark 9 (vconcat [9500] (make-vector tab-width 9472)))
     )))

(use-package wgrep
  :load-path "~/.config/emacs/site-lisp/Emacs-wgrep")

;;; init.el ends here
