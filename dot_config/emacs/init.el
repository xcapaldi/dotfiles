;;; init.el

;;; Commentary:

;; My personal emacs configuration.

;;; Code:

;;;; general configuration
(use-package emacs
  :config
  ;; set variable defaults
  (setq-default
   coding-system-for-read 'utf-8     ; Use UTF-8 by default
   coding-system-for-write 'utf-8
   cursor-in-non-selected-windows t  ; Don't hide the cursor in inactive windows
   help-window-select t              ; Focus new help windows when opened
   indent-tabs-mode nil              ; Use spaces by default instead of tabs
   tab-width 4                       ; Set width for tabs
   indicate-empty-lines t            ; Display bitmap in left fringe on empty lines
   indicate-buffer-boundaries 'left  ; Indicate last newline in buffer
   require-final-newline t           ; Always insert final newline on save
   show-trailing-whitespace t        ; Highlight trailing whitespace at end of line
   inhibit-startup-screen t          ; Remove default start screen
   select-enable-clipboard t         ; Merge Emacs and system clipboard
   view-read-only t                  ; Always open read-only buffers in view-mode
   visible-bell t                    ; Use a visual bell
   vc-follow-symlinks t              ; Don't ask for confirmation following symlinked files
   sentence-end-double-space nil     ; Sentences end with punctuation and a single space
   show-paren-delay 0                ; No delay on highlighting matching paren
   backup-by-copying t               ; Do not move current file while creating backup
   create-lockfiles nil              ; Disable lockfiles
   enable-recursive-minibuffers t)   ; Don't allow recursive minibuffers

  ;; enable/disable modes
  (column-number-mode 1)                   ; Show the column number in modeline
  (context-menu-mode 1)                    ; Replace standard mouse-3 actions with context menu
  (global-auto-revert-mode 1)              ; If file changes on disk, update the buffer automatically
  (pixel-scroll-precision-mode 1)          ; Smooth scrolling
  (display-fill-column-indicator-mode -1)  ; Don't display indicator for the fill line
  (global-hl-line-mode -1)                 ; Don't highlight current line globally
  (show-paren-mode 1)                      ; Show matching parens
  (blink-cursor-mode 1)                    ; Blink the cursor
  (tooltip-mode -1)                        ; Hide mouse hover tooltips
  (global-visual-line-mode -1)             ; Wrap lines instead of extending past view
  (auto-fill-mode -1)                      ; Don't auto-wrap lines
  (minibuffer-depth-indicate-mode 1)       ; Indicate minibuffer recursive depth when recursion enabled

  ;; other settings
  (fset 'yes-or-no-p 'y-or-n-p)  ; Replace yes/no prompts with y/n
  (load-theme 'modus-operandi t) ; Use prot's modus themes

  ;; only enable font if available on system
  (when (member "Unifont" (font-family-list))
    (if (equal system-type 'darwin)
        (progn (set-frame-font "Unifont-15:regular" nil t)
               (add-to-list 'initial-frame-alist '(font . "Unifont-15:regular"))
               (add-to-list 'default-frame-alist '(font . "Unifont-15:regular")))
      (progn (set-frame-font "Unifont-12:regular" nil t)
             (add-to-list 'initial-frame-alist '(font . "Unifont-12:regular"))
             (add-to-list 'default-frame-alist '(font . "Unifont-12:regular")))))
  ;; set fallback fonts for symbols and emoji
  (set-fontset-font t 'symbol (font-spec :family "Apple Symbols") nil 'prepend)
  (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend)
  ;; set line spacing (0.1 == 1x)
  (setq-default line-spacing 0.1)
  (when (equal system-type 'darwin)
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier 'super)))

;;;; package management and configuration
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize) ;; re-enable package.el (disabled in early-init.el)

(require 'use-package)

;; install package to enable version-controlled package management in use-package
(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))

;;;; configure backups and keep config dir clean
(use-package no-littering
  ;; https://github.com/emacscollective/no-littering
  ;; Automagically keep config directory clean.
  :ensure t
  :config
  ;; store backups and autosaves in centralized tmp directories
  (no-littering-theme-backups))


;;;; packages configuration

;; ace-window

(use-package age
  ;; https://github.com/anticomputer/age.el
  ;; Add support for Age encryption/decryption.
  :ensure t
  :demand t
  :custom
  (age-default-identity '("~/Documents/org/age_general"
			  "~/Documents/org/age_chezmoi"))
  (age-default-recipient '("~/Documents/org/age_general.pub"
			   "~/Documents/org/age_chezmoi.pub"))
  (auth-sources '("~/.authinfo" "~/.authinfo.gpg" "~/.authinfo.age" "~/.netrc"))
  :config
  (age-file-enable))

(use-package anzu
  ;; https://github.com/emacsorphanage/anzu
  ;; Improve UX of isearch and query replace (M-%) in isearch.
  :ensure t
  :bind (([remap isearch-query-replace] . anzu-isearch-query-replace)
         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :init (global-anzu-mode 1)
  :custom ((anzu-deactivate-region t) ; deactivate region when anzu overlay starts for constrast
           (anzu-search-threshold 1000)
           (anzu-replace-threshold 50)
           (anzu-replace-to-string-separator " => ")))

;; apheleia

(use-package asdf
  ;; https://github.com/tabfugnic/asdf.el
  ;; asdf is a version manager used at my work to control developer tool
  ;; versions. This package makes tools installed by asdf accessible to
  ;; emacs.
  :vc (:fetcher github :repo tabfugnic/asdf.el)
  :if (eq system-type 'darwin)
  :config (asdf-enable))

;; (use-package avy
;;   ;; https://github.com/abo-abo/avy
;;   :ensure t
;;   :bind (("C-c s" . avy-goto-char-timer)
;;          ("C-z" . avy-goto-char-timer) ;; replace suspend frame
;;          ([remap goto-line] . avy-goto-line) ;; M-g g or M-g M-g
;;          :map isearch-mode-map
;;          ("C-c s" . avy-isearch))
;;   :custom
;;   (avy-style 'at-full)
;;   (avy-timeout-seconds 0.25)
;;   (avy-case-fold-search nil)
;;   (avy-background nil))

;; browse-at-remote
;; https://github.com/rmuslimov/browse-at-remote

(use-package chezmoi
  ;; https://github.com/tuh8888/chezmoi.el
  ;; Chezmoi is a dotfiles manager. This package exposes some chezmoi
  ;; convenience functions inside emacs.
  :ensure t)

(use-package copilot
  ;; https://github.com/zerolfx/copilot.el
  ;; Integrate Github Copilot with emacs. The licence is provided at my work.
  :vc (:fetcher github :repo zerolfx/copilot.el)
  :if (eq system-type 'darwin)
  :hook (prog-mode . copilot-mode)
  :config
  (transient-define-prefix copilot-prefix ()
    "Prefix that allow control of copilot suggestions"
    [("<return>" "accept completion" copilot-accept-completion)
     ("n" "next completion" copilot-next-completion :transient t)
     ("p" "prev completion" copilot-previous-completion :transient t)
     ("<tab>" "accept completion by line" copilot-accept-completion-by-line :transient t)
     ("<SPC>" "accept completion by word" copilot-accept-completion-by-word :transient t)
     ("<backspace>" "clear overlay" copilot-clear-overlay)])
  :bind (:map copilot-mode-map
              ("S-<tab>" . copilot-accept-completion)
              ("S-<SPC>" . copilot-accept-completion-by-word)
              ("C-c c" . copilot-prefix)))

(use-package denote
  ;; https://github.com/protesilaos/denote
  :ensure t)

;; denote-menu

;; dimmer

(use-package dired
  ;; Native file explorer
  :ensure nil
  :custom (dired-listing-switches "-alh")
  ;; allow navigating directories in current buffer with 'a'
  :config (put 'dired-find-alternate-file 'disabled nil))

(use-package display-line-numbers
  ;; Native method of displaying line numbers.
  :ensure nil)
  ;;:hook (prog-mode . display-line-numbers-mode))

(use-package dumb-jump
  ;; https://github.com/jacktasia/dumb-jump
  ;; Use grep, ag or ripgrep with some baked language heuristics for fast goto
  ;; definition command as a fallback when LSP doesn't perform.
  :ensure t
  :after xref
  :custom (dumb-jump-force-searcher 'rg)
  :config (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package editorconfig
  ;; https://github.com/editorconfig/editorconfig-emacs
  ;; Make emacs respect EditorConfig.
  :ensure t
  :config (editorconfig-mode 1))

(use-package eglot
  ;; Native LSP client
  :ensure nil
  :commands eglot
  :config
  (transient-define-prefix eglot-prefix ()
    "Prefix that allows control of eglot LSP interface"
    [("f" "format" eglot-format)
     ("s" "shutdown" eglot-shutdown)
     ("S" "shutdown all" eglot-shutdown-all)
     ("R" "reconnect" eglot-reconnect)
     ("a" "code actions" eglot-code-actions)
     ("r" "rename" eglot-rename)])
  :bind (:map eglot-mode-map
              ("C-c l" . eglot-prefix)))

;; elfeed

(use-package eshell
  :ensure nil
  :config
  ;; this breaks on MacOS
  (if (eq system-type 'gnu/linux)
      (add-to-list 'eshell-modules-list 'eshell-smart))
  :custom
  ;; commands which should run in a dedicated terminal
  (eshell-visual-commands '("vi" "vim" "screen" "tmux" "top" "htop" "less" "more" "lynx" "links" "ncftp" "mutt" "pine" "tin" "trn" "elm"))
  ;; command options which need to run in a dedicated terminal
  (eshell-visual-options '(("git" "--help" "--paginate")))
  ;; subcommands which need to run in a dedicated terminal
  (eshell-visual-subcommands '(("git" "log" "diff" "show"))))

(use-package em-smart
  :ensure nil
  :if (eq system-type 'gnu/linux)
  :after eshell
  :custom
  (eshell-where-to-jump 'begin)
  (eshell-review-quick-commands t)
  (eshell-smart-space-goes-to-end nil))

(use-package exec-path-from-shell
  ;; https://github.com/purcell/exec-path-from-shell
  ;; emacs on MacOS cannot access path properly. This package allows emacs to
  ;; access the same environment variables as in the shell.
  :ensure t
  :if (memq window-system '(mac ns x))
  :config (exec-path-from-shell-initialize))

;; focus

;; email configuration -- stored in separate encrypted file
(if (eq system-type 'gnu/linux)
  (load-file (concat user-emacs-directory "gnus.el")))

;; (use-package go-dlv
;;   ;; https://github.com/benma/go-dlv.el
;;   ;; GDB doesn't understand Go well. Instead you should use Delve. This package
;;   ;; adds emacs support for Delve on top of GUD.
;;   :vc (:fetcher github :repo benma/go-dlv.el))

(use-package go-mode
  ;; https://github.com/dominikh/go-mode.el
  ;; Standard Go language support with integrations to tools like gofmt and Go
  ;; Playground.
  :ensure t
  :mode (("\\.go\\'" . go-mode)
         ("\\.mod\\'" . go-dot-mod-mode)))

(use-package grep
  ;; Native grep interface
  :ensure nil
  :config
  ;; replace grepping commands with ripgrep if available
  ;; https://stegosaurusdormant.com/emacs-ripgrep/
  (if (executable-find "rg")
      (grep-apply-setting
       'grep-find-command
       '("rg -n -H --no-heading -e '' $(git rev-parse --show-toplevel || pwd)" . 27))))

;; helpful

(use-package hippie-exp
  :ensure nil
  :bind ([remap dabbrev-expand] . hippie-expand)) ;; M-/ and C-M-/

(use-package ibuffer
  ;; Native nice replacement for buffer-menu.
  :ensure nil
  :bind ([remap list-buffers] . ibuffer)) ;; C-x C-b

(use-package icomplete
  ;; Native completion interface
  :ensure nil
  :init (icomplete-mode)
  ;; M-TAB is the normal keybind but often conflicts with window managers
  :bind (:map icomplete-minibuffer-map
			  ("C-k" . icomplete-force-complete))
  :custom
  (icomplete-prospects-height 1)
  (completion-styles '(flex basic)))

(use-package js
  ;; Native javascript mode.
  :ensure nil
  :mode "\\.js[x]\\'")

(use-package magit
  ;; https://magit.vc/
  ;; Git porcelain.
  :ensure t)

(use-package magit-todos
  ;; https://github.com/alphapapa/magit-todos
  ;; Highlight TODOs in magit interface.
  :ensure t
  :hook magit-mode)

(use-package markdown-mode
  ;; https://jblevins.org/projects/markdown-mode/
  ;; Mode for markdown markup format.
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode))

(use-package minions
  ;; https://github.com/tarsius/minions
  ;; Hide minor mode markers in one menu in modeline.
  :ensure t
  :config (minions-mode 1)
  :custom (minions-prominent-modes '(flymake-mode)))

(use-package ob-go
  ;; https://github.com/pope/ob-go
  ;; Support for Go in org mode code blocks.
  :ensure t
  :after org-mode)

(use-package org
  :ensure nil
  :config
  ;; Org-babel supported languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (python . t)))
  :custom
  (org-adapt-indentation nil)
  (org-edit-src-content-indentation 0))

(use-package prism
  ;; https://github.com/alphapapa/prism.el
  ;; Highlight by depth. Very helpful for data modes or those those lacking
  ;; their own highlighting.
  :ensure t
  :mode (("\\.proto\\'" . prism-mode)
         ("^Tiltfile\\'" . prism-whitespace-mode)))

(use-package protobuf-mode
  ;; https://github.com/protocolbuffers/protobuf/blob/main/editors/protobuf-mode.el
  ;; Suuport for Protocol Buffer (protobuf) serialized structured data syntax.
  :ensure t
  :if (eq system-type 'darwin)
  :mode ("\\.proto\\'"))

(use-package pulsar
  ;; https://protesilaos.com/emacs/pulsar
  ;; Extends pulse.el to pulse line on move or command.
  :ensure t
  :init (pulsar-global-mode 1)
  ;; :bind (("C-c h p" . pulsar-pulse-line)
  ;;       ("C-c h h" . pulsar-highlight-dwim))
  :commands (recenter-top-bottom
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
             outline-up-heading)
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
                            outline-up-heading)))

;; puni

;; (use-package rainbow-delimiters
;;   ;; https://github.com/Fanael/rainbow-delimiters
;;   :ensure t
;;   :hook (prog-mode. rainbow-delimiters-mode))

;; rainbow-mode

(use-package shell
  ;; Native dumb shell. It's non-interactive but retains emacs keybindings. For
  ;; an interactive terminal, use ansi-term instead.
  :ensure nil
  :init (setq comint-process-echoes t)) ; remove zsh echoing
  ;; :config
  ;; (defun xcc/shell-cur-dir ()
  ;;   (interactive)
  ;;   (shell (concat "*shell" default-directory "*")))
  ;; (transient-define-prefix spawn-shell-prefix ()
  ;;   "Prefix that allows spawning shells"
  ;;   [("<return>" "current directory" xcc/shell-cur-dir)])
  ;; :bind ("C-c s" . spawn-shell-prefix))

(use-package subword
  ;; Treat camel-cased subwords as words. So thisFunction is two words now.
  :ensure nil
  :hook (prog-mode . subword-mode))

(use-package terraform-mode
  ;; https://github.com/hcl-emacs/terraform-mode
  ;; Support for Terraform configuration files.
  :ensure t
  :if (eq system-type 'darwin)
  :mode ("\\.tf\\'" . terraform-mode))

(use-package typescript-mode
  ;; https://github.com/emacs-typescript/typescript.el
  ;; Support for Typescript.
  :ensure t
  :mode "\\.ts[x]\\'")

;; undo-hl

(use-package vundo
  ;; https://github.com/casouri/vundo
  ;; Undo tree visualization for the native emacs undo system.
  :ensure t
  :commands (vundo)
  :bind ("C-c u" . vundo))

(use-package whitespace
  ;; Show whitespace characters
  :ensure nil
  ;; :init (global-whitespace-mode 1)
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
  ;; https://github.com/mhayashi1120/Emacs-wgrep
  ;; Adds ability to edit grep buffers in same way as wdired allows editing of
  ;; files and directories.
  :vc (:fetcher github :repo mhayashi1120/Emacs-wgrep))

;; load customization file
(load custom-file)

;;; init.el ends here
