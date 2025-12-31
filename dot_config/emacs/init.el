;;; init.el

;;; Commentary:

;; My personal emacs configuration.

;;; Code:

;;;; general configuration
(use-package emacs
  :custom
  ;; Hide commands in M-x which do not work in the current mode
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; list of directories or files which we trust the contents for execution
  (trusted-content nil)
  ;; make buffers holding files of the same name unique by preprending directory
  ;; structure or the project info
  (uniquify-dirname-transform 'project-uniquify-dirname-transform)
  ;; ask for gpg passphrase directly in emacs
  (epg-pinentry-mode 'loopback)
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
   enable-recursive-minibuffers nil) ; Don't allow recursive minibuffers

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
  (which-function-mode 1)                  ; Display the

  ;; other settings
  (fset 'yes-or-no-p 'y-or-n-p)  ; Replace yes/no prompts with y/n
  (load-theme 'modus-operandi t) ; Use prot's modus themes

  ;; open fullscreen by default
  ;; only relevant when using emacs as the window manager
  ;; (set-frame-parameter nil 'fullscreen 'fullboth)

  ;; only enable font if available on system
  (if (equal system-type 'darwin)
      (when (member "Maple Mono" (font-family-list))
        (progn (set-frame-font "Maple Mono-15:regular" nil t)
               (add-to-list 'initial-frame-alist '(font . "Maple Mono-15:regular"))
               (add-to-list 'default-frame-alist '(font . "Maple Mono-15:regular"))))
    (when (member "Unifont" (font-family-list))
      (progn (set-frame-font "Unifont-12:regular" nil t)
             (add-to-list 'initial-frame-alist '(font . "Unifont-12:regular"))
             (add-to-list 'default-frame-alist '(font . "Unifont-12:regular")))))
  ;; set fallback fonts for symbols and emoji
  (if (equal system-type 'darwin)
      (progn (set-fontset-font t 'symbol (font-spec :family "Apple Symbols") nil 'prepend)
             (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend)))
  ;; set line spacing (0.1 == 1x)
  (setq-default line-spacing 0.1)
  (when (equal system-type 'darwin)
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier 'super))

  ;; disable C-scrolling to change font size on Mac.
  ;; The trackpad scrolls very fast and scales the font size too much.)
  (when (equal system-type 'darwin)
    (global-set-key (kbd "C-<wheel-up>") nil)
    (global-set-key (kbd "C-<wheel-down>") nil)))

;;;; package management and configuration
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize) ;; re-enable package.el (disabled in early-init.el)

(require 'use-package)

;;;; packages configuration

;; ace-window

(use-package alabaster-theme
  ;; https://github.com/uzhne/alabaster-emacs
  :vc (modus-themes :url "https://github.com/xcapaldi/alabaster-emacs"
                    :branch "remove-reference-to-espresso"))
  
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
  :vc (:url "https://github.com/tabfugnic/asdf.el.git")
  :if (eq system-type 'darwin)
  :config (asdf-enable))

(use-package avy
  ;; https://github.com/abo-abo/avy
  :ensure t
  :bind (("C-c s" . avy-goto-char-timer)
         ("C-z" . avy-goto-char-timer) ;; replace suspend frame
         ([remap goto-line] . avy-goto-line) ;; M-g g or M-g M-g
         :map isearch-mode-map
         ("C-c s" . avy-isearch))
  :custom
  (avy-style 'at-full)
  (avy-timeout-seconds 0.25)
  (avy-case-fold-search nil)
  (avy-background nil))

(use-package beancount
  ;; https://github.com/beancount/beancount-mode
  ;; Beancount plain-text accounting support.
  :vc (:url "https://github.com/beancount/beancount-mode.git")
  :if (eq system-type 'gnu/linux)
  :mode ("\\.beancount\\'" . beancount-mode)
  :hook ((beancount-mode . outline-minor-mode)
         (beancount-mode . flymake-bean-check-enable)))

;; (use-package browse-at-remote
;;   ;; https://github.com/rmuslimov/browse-at-remote
;;   ;; Jump to current line on VC remote repository.
;;   :ensure t
;;   :demand t
;;   :after project
;;   :config
;;   (transient-append-suffix 'version-control-transient '(0 -1)
;;      '("r" "browse at remote" browse-at-remote)))

(use-package chezmoi
  ;; https://github.com/tuh8888/chezmoi.el
  ;; Chezmoi is a dotfiles manager. This package exposes some chezmoi
  ;; convenience functions inside emacs.
  :ensure t)

;; (use-package completion-preview
;;   ;; Native emacs minor mode for viewing completions in a buffer with live update.
;;   :ensure nil
;;   :config (global-completion-preview-mode))

(use-package cook-mode
  :vc (:url "https://github.com/cooklang/cook-mode"
            :rev :newest
            :branch "master")
  :if (eq system-type 'gnu/linux)
  :mode ("\\.cook\\'" . cook-mode))

(use-package cua-rect
  ;; Native emulation for CUA keybindings (standard copy/paste/cut).
  ;; I use only for the enhanced rectangle selection.
  :ensure nil
  :init (cua-selection-mode 1))

;; (use-package diff-hl
;;   ;; Highlight git diffs
;;   ;; https://github.com/dgutov/diff-hl
;;   :ensure t
;;   :init (global-diff-hl-mode)
;;   :config (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package dired
  ;; Native file explorer
  :ensure nil
  :custom
  (dired-listing-switches "-alh")
  (dired-movement-style 'cycle)
  ;; allow navigating directories in current buffer with 'a'
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  (transient-define-prefix dired-prefix ()
    "Prefix with useful dired commands"
    :transient-non-suffix 'transient--do-stay
    [["Dired"
      ("m" "mark" dired-mark :transient t)
      ("u" "unmark" dired-unmark :transient t)]
     ["Image Dired"]])
  :bind (:map dired-mode-map
              ("C-c <SPC>" . dired-prefix)))

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
  ;; Native EditorConfig support.
  :config (editorconfig-mode 1))

(use-package eglot
  ;; Native LSP client
  :ensure nil
  :commands eglot
  :config
  (defun my-eglot-organize-imports () (interactive)
         (eglot-code-actions nil nil "source.organizeImports" t))
  (add-hook 'before-save-hook 'my-eglot-organize-imports nil t)
  (add-hook 'before-save-hook 'eglot-format-buffer)
  (transient-define-prefix eglot-transient ()
    "Eglot Prefix"
    :transient-non-suffix 'transient--do-leave
    [("f" "format" eglot-format)
     ("i" "organize imports" eglot-code-action-organize-imports)
     ("S" "shutdown" eglot-shutdown)
     ;; eglot-shutdown-all
     ("R" "reconnect" eglot-reconnect)
     ("a" "code actions" eglot-code-actions)
     ("r" "rename" eglot-rename)])
  (transient-append-suffix 'project-transient '(0 -1 -1) ;; in the last group
     '("l" "lsp" eglot-transient)))

;; elfeed

(use-package embark
  ;; https://github.com/oantolin/embark
  ;; Contextual actions and capture of minibuffer listings.
  :ensure t
  :bind
  (("<f5>" . embark-act)         ;; pick some comfortable binding
   ("<f6>" . embark-dwim))       ;; good alternative: M-.
  :config
  ;; Use embark as keybind completion helper
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  ;; Add target for go tests
  (defun xcc/embark-go-test ()
    "Target a go test at point of the form func Test___(_ *testing.T) { ___ }."
    (when (and (derived-mode-p 'go-mode)
               (thing-at-point-looking-at "^func \\(Test[A-Za-z0-9_]+\\)"))
      (list 'go-test (match-string 1))))

  (defun xcc/embark-run-go-test (test-name)
    "Run Go test with the given name."
    (let ((command (format "go test -run \"^%s$\"" test-name)))
      (compile command)))

  (add-to-list 'embark-target-finders 'xcc/embark-go-test)

  (add-to-list 'embark-keymap-alist '(go-test . xcc/embark-go-test-map))
  (defvar-keymap xcc/embark-go-test-map
    :doc "Keymap for go test actions."
    "t" #'xcc/embark-run-go-test)
  )

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

;; emacs web wowser -- native, text-based browser using shr
(use-package eww
  :ensure nil)

(use-package exec-path-from-shell
  ;; https://github.com/purcell/exec-path-from-shell
  ;; emacs on MacOS cannot access path properly. This package allows emacs to
  ;; access the same environment variables as in the shell.
  :ensure t
  :demand t
  :if (memq window-system '(mac ns x))
  :config (exec-path-from-shell-initialize))

;; focus

(use-package flymake
  ;; Native mode for on-the-fly syntax checking.
  :ensure nil
  :bind
  (:map flymake-mode-map
        ("C-c f n" . flymake-goto-next-error)
        ("C-c f p" . flymake-goto-prev-error)
        ("C-c f b" . flymake-show-buffer-diagnostics)
        ("C-c f r" . flymake-show-project-diagnostics))
  :custom
  ;; can also be set to short which only shows the most severe diagnostics
  (flymake-show-diagnostics-at-end-of-line nil)
  :config
  (advice-add 'flymake-goto-next-error :after #'flymake-transient)
  (advice-add 'flymake-goto-prev-error :after #'flymake-transient)
  (transient-define-prefix flymake-transient ()
    "Flymake Transient"
    :transient-non-suffix 'transient--do-leave
    [("n" "next error" flymake-goto-next-error :transient t)
     ("p" "prev error" flymake-goto-prev-error :transient t)
     ("b" "show buffer diagnostics" flymake-show-buffer-diagnostics)
     ("r" "show project diagnostics" flymake-show-project-diagnostics)])
  (transient-append-suffix 'project-transient '(0 -1 -1) ;; in the last group
    '("F" "flymake" flymake-transient)))

(use-package gnus
  ;; Native email/feed client.
  :ensure nil
  :custom
  ;; disable default select method
  (gnus-select-method '(nnnil nil))

  ;; speed gnus up
  (gnus-asynchronous t)
  (gnus-use-cache t)
  (gnus-use-header-prefetch t)

  ;; use native smtpmail to send emails
  (message-send-mail-function 'message-use-send-mail-function
                              send-mail-function 'smtpmail-send-it)

  ;; when exiting a group, process messages marked as expired
  (gnus-exit-group-hook 'gnus-summary-expire-articles)

  (gnus-directory (concat user-emacs-directory "/etc/gnus/news/"))
  :config
  (if (equal system-type 'gnu/linux)
      (load-file (concat user-emacs-directory "gnus.el.gpg"))))

;;(use-package go-dlv
;;   ;; https://github.com/benma/go-dlv.el
;;   ;; GDB doesn't understand Go well. Instead you should use Delve. This package
;;   ;; adds emacs support for Delve on top of GUD.
;;   :vc (:url "https://github.com/benma/go-dlv.el.git"))

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
  :bind (:map icomplete-minibuffer-map
              ("C-n" . icomplete-forward-completions)
              ("C-p" . icomplete-backward-completions)
              ("RET" . icomplete-force-complete-and-exit))
  ;;(setq icomplete-delay-completions-threshold 0)
  ;;(setq icomplete-compute-delay 0)
  ;;(setq icomplete-show-matches-on-no-input t)
  ;;(setq icomplete-hide-common-prefix nil)
  ;;(setq icomplete-prospects-height 10)
  ;;(setq icomplete-separator " . ")
  ;;(setq icomplete-max-delay-chars 0)
  ;;(setq icomplete-scroll t)
  ;;(advice-add 'completion-at-point
  ;;            :after #'minibuffer-hide-completions)
  :custom
  (icomplete-mode 1)
  (icomplete-prospects-height 1)
  ;; Inline completions using icomplete. This is a really nice interface and
  ;; can probably be made even nicer with icomplete-vertical; however it
  ;; doesn't seem to work with completions provided by eglot (lsp). 
  ;;(icomplete-in-buffer t)
  (icomplete-with-completion-tables t))

(use-package howm
  ;; https://github.com/kaorahi/howm
  ;; Opinionated note-taking and planning tool implemented in Emacs.
  :ensure t
  ;;:vc (:url "https://github.com/kaorahi/howm.git"
  ;;          :rev :newest)
  :init
  ;;(require 'howm-org)
  ;; fix for bug in howm-org.el
  ;;(setq howm-keyword-body-regexp "[^\r\n]+")
  (setq howm-file-name-format "%Y%m%dT%H%M%S.txt")
  ;; Use Windows host filesystem on WSL
  (if (equal system-name "capaldi-phampc")
    (setq xcc/howm-directory "/mnt/c/Users/xavie/org")
  (setq xcc/howm-directory "~/Notes/"))
  :custom
  (howm-directory xcc/howm-directory)
  (howm-keyword-file (expand-file-name ".howm-keys" xcc/howm-directory))
  (howm-history-file (expand-file-name ".howm-history" xcc/howm-directory))
  ;; Keep one window after "1" key in the summary buffer.
  (howm-view-keep-one-window t)
  ;; Use ripgrep as grep
  (howm-view-use-grep t)
  (howm-view-grep-command "rg")
  (howm-view-grep-option "-nH --no-heading --color never")
  (howm-view-grep-extended-option nil)
  (howm-view-grep-fixed-option "-F")
  (howm-view-grep-expr-option nil)
  (howm-view-grep-file-stdin-option nil)
  ;; Search optimisations
  (howm menu-refresh-after-save nil)
  (howm-menu-expiry-hours 2)  ;; cache menu N hours
  (howm-menu-file "00000000T000000.txt"))

(use-package indent-aux
  ;; Native minor mode to deindent text which is saved to kill ring.
  :ensure nil
  :custom (kill-ring-deindent-mode))

(use-package js
  ;; Native javascript mode.
  :ensure nil
  :mode "\\.js[x]\\'")

(use-package kubel
  ;; https://github.com/abrochard/kubel
  ;; Interact with Kubernetes from emacs. Mostly I still interact via the cli.
  :ensure t
  :if (eq system-type 'darwin))

;; This package may still be useful, but vc-annotate (C-x v g) serves
;; my current needs. I otherwise interact with git via the cli.
(use-package magit
  ;; https://magit.vc/
  ;; git porcelain
  :ensure t)

;;(use-package magit-todos
  ;; https://github.com/alphapapa/magit-todos
  ;; Highlight TODOs in magit interface.
;;  :ensure t
;;  :hook magit-mode)

;;(use-package marginalia
;;  ;; https://github.com/minad/marginalia
;;  ;; Enable rich annotations using the Marginalia package
;;  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
;;  ;; available in the *Completions* buffer, add it to the
;;  ;; `completion-list-mode-map'.
;;  :ensure t
;;  :bind (:map minibuffer-local-map
;;         ("M-A" . marginalia-cycle))
;;  :init (marginalia-mode))

(use-package markdown-mode
  ;; https://jblevins.org/projects/markdown-mode/
  ;; Mode for markdown markup format.
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode))

(use-package minibuffer
  ;; Native minibuffer and completion
  :ensure nil
  :custom
  (completion 'always)
  (completion-auto-help 'visible)
  (completion-auto-select 'second-tab)
  (completions-max-height 12))

(use-package minions
  ;; https://github.com/tarsius/minions
  ;; Hide minor mode markers in one menu in modeline.
  :ensure t
  :config (minions-mode 1)
  :custom (minions-prominent-modes '(flymake-mode)))

(use-package no-littering
  ;; https://github.com/emacscollective/no-littering
  ;; Automagically keep config directory clean.
  :ensure t
  :demand t
  :config
  ;; store backups and autosaves in centralized tmp directories
  (no-littering-theme-backups))

(use-package ob-go
  ;; https://github.com/pope/ob-go
  ;; Support for Go in org mode code blocks.
  :ensure t
  :after org-mode)

(use-package hotfuzz
  ;; https://github.com/axelf4/hotfuzz
  ;; Similarly to built-in flex completion but faster and better scoring algorithm.
  :ensure t
  :custom (completion-styles '(hotfuzz basic)))

(use-package org
  :ensure nil
  :config
  ;; Org-babel supported languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (python . t)))
  (add-to-list 'org-modules 'org-habit)
  ;; Use Windows host filesystem on WSL
  (if (equal system-name "capaldi-phampc")
      (setq org-agenda-files (quote ("/mnt/c/Users/xavie/org/obtf.org")))
    (setq org-agenda-files (quote ("~/Notes/"))))
  :custom
  (org-adapt-indentation nil)
  (org-edit-src-content-indentation 0)
  (org-tags-column 0) ;; don't right align tags
  ;; Capture
  (org-capture-templates
   '(("t" "Todo" entry (file+headline "/mnt/c/Users/xavie/org/obtf.org" "Inbox")
         "* TODO %?\n  %i\n  %a")))
  ;; Agenda configuration
  (org-agenda-span 'week)
  ;; Hide tasks that are scheduled in the future.
  (org-agenda-todo-ignore-scheduled 'future)
  ;; Use "second" instead of "day" for time comparison.
  ;; It hides tasks with a scheduled time like "<2020-11-15 Sun 11:30>"
  (org-agenda-todo-ignore-time-comparison-use-seconds t)
  ;; Hide the deadline prewarning prior to scheduled date.
  (org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled))

(use-package package-vc
  ;; Native ability of package.el to manage vc sources.
  ;; Look at ~package-isolate~ for testing packages in controlled environment.
  :ensure nil
  :demand t
  :custom
  ;; vc packages are registered as projects and can navigated with project.el
  (package-vc-register-as-project t))

(use-package prism
  ;; https://github.com/alphapapa/prism.el
  ;; Highlight by depth. Very helpful for data modes or those those lacking
  ;; their own highlighting.
  :ensure t
  :mode (("\\.proto\\'" . prism-mode)
         ("\\Tiltfile\\'" . prism-whitespace-mode)))

(use-package project
  ;; Native project management
  :ensure nil
  :bind ("C-c p" . project-transient)
  ;; display the current poject in the mode line
  :custom (project-mode-line t)
  :config
  (executable-find "gh")
  (defun xcc/browse-at-remote ()
    (interactive)
    (shell-command
     (concat "gh browse "
             (file-name-nondirectory (buffer-file-name))
             ":"
             (number-to-string (line-number-at-pos)))))
  (transient-define-prefix version-control-transient()
    "Version control Prefix"
    :transient-non-suffix 'transient--do-leave
    [("s" "status" project-vc-dir)
     ("b" "browse at remote" xcc/browse-at-remote)])
  (transient-define-prefix project-transient ()
    "Project Prefix"
    :transient-non-suffix 'transient--do-leave
    [["Project"
      ("p" "switch project" project-switch-project)
      ("X" "forget project" project-forget-project)]
     ["Files and buffers"
      ("f" "find file" project-find-file)
      ("d" "dired" project-dired)
      ("b" "switch buffer" project-switch-to-buffer)
      ("B" "list buffers" project-list-buffers)
      ("k" "kill buffers" project-kill-buffers)]
     ["Search and replace"
      ("g" "regexp search" project-find-regexp)
      ("G" "interactive search" project-search)
      ("r" "query replace" project-query-replace-regexp)]
     ["Shell and compilation"
      ("s" "shell" project-shell)
      ("e" "eshell" project-eshell)
      ("c" "compile" project-compile)
      ("!" "shell command" project-shell-command)
      ("&" "async shell command" project-async-shell-command)]
     ["Extensions"
      ("v" "version control" version-control-transient)]]))

(use-package protobuf-mode
  ;; https://github.com/protocolbuffers/protobuf/blob/main/editors/protobuf-mode.el
  ;; Suuport for Protocol Buffer (protobuf) serialized structured data syntax.
  :ensure t
  :if (eq system-type 'darwin)
  :mode ("\\.proto\\'"))

;; puni

(use-package pyvenv
  ;; Activate python virtual environments so Emacs respects them.
  :ensure t
  :hook (python-mode . pyvenv-mode))

;; rainbow-mode

;; using CUA's rectangle mode instead
;;(use-package rect
;;  ;; Native rectangle mark commands
;;  :ensure nil
;;  :bind ("C-S-SPC" . rectangle-mark-mode))

;;(use-package repeat
;;  ;; Native transient keybinding mode allowing repeating terminal keys.
;;  :ensure nil
;;  :init (repeat-mode 1))

(use-package shell
  ;; Native dumb shell. It's non-interactive but retains emacs keybindings. For
  ;; an interactive terminal, use ansi-term instead.
  :ensure nil
  :init (setq comint-process-echoes nil) ; remove zsh echoing
  :custom (show-trailing-whitespace nil))

(use-package shr
  ;; Native HTML renderer.
  :ensure nil
  :custom
  (shr-max-image-proportion 0.9)
  (shr-discard-aria-hidden t))

(use-package simple
  ;; Native package which contains undo commands
  :ensure nil
  :config
  (advice-add 'undo :after #'undo-transient)
  (transient-define-prefix undo-transient ()
    "Undo Prefix"
    :transient-non-suffix 'transient--do-leave
    [("u" "undo" undo :transient t)]))

(use-package sql
  ;; Native SQL interaction mode.
  :ensure nil)
;;  :bind
;;  (:repeat-map sql-interactive-mode-repeat-map
;;   ("n" . comint-next-prompt)
;;   ("p" . comint-previous-prompt)))

(use-package subword
  ;; Treat camel-cased subwords as words. So thisFunction is two words now.
  :ensure nil
  :hook (prog-mode . subword-mode))

(use-package term
  ;; Native terminal emulator
  :ensure nil
  :custom (show-trailing-whitespace nil))

(use-package terraform-mode
  ;; https://github.com/hcl-emacs/terraform-mode
  ;; Support for Terraform configuration files.
  :ensure t
  :if (eq system-type 'darwin)
  :mode ("\\.tf\\'" . terraform-mode))

(use-package transient
  ;; Support for transient commands/menus.
  ;; Use transient-get-suffix to help append suffixes.
  :ensure nil
  :custom
  ;; error if there are key conflicts in transient menus
  ;; helpful since I append to transient menus in different use-package blocks
  (transient-detect-key-conflicts t))

(use-package typescript-mode
  ;; https://github.com/emacs-typescript/typescript.el
  ;; Support for Typescript.
  :ensure t
  :mode "\\.ts[x]\\'")

;; undo-hl

(use-package vc-annotate
  ;; Native support for display annotations from version-control system.
  :ensure nil
  :commands (vc-annotate)
  :init
  (transient-append-suffix 'version-control-transient '(0 -1)
    '("b" "blame" vc-annotate)))

(use-package vterm
  ;; https://github.com/akermu/emacs-libvterm
  ;; Full terminal emulator running inside Emacs.
  ;; Requires external dependencies:
  ;; brew install cmake libtool libvterm
  :ensure nil
  :if (eq system-type 'darwin)
  :bind (:map vterm-mode-map
              ("C-y" . vterm-yank))
  :config
  (defun xcc/vterm-project (arg)
    "Start vterm in current project root. Buffer is associated with project.
With prefix ARG, create new vterm with unique suffix."
    (interactive "P")
    (let* ((default-directory (project-root (project-current t)))
           (project (project-name (project-current t)))
           (base-name (format "*vterm[%s]*" project))
           (buffer-name (if arg
                            (let ((n 2))
                              (while (get-buffer (format "*vterm[%s]<%d>*" project n))
                                (setq n (1+ n)))
                              (format "*vterm[%s]<%d>*" project n))
                          base-name)))
      (if (and (not arg) (get-buffer buffer-name))
          (switch-to-buffer buffer-name)
        (vterm buffer-name))))
  (transient-append-suffix 'project-transient '(0 3 -1) ;; after eshell in shell group
    '("t" "vterm" xcc/vterm-project)))

(use-package vundo
  ;; https://github.com/casouri/vundo
  ;; Undo tree visualization for the native emacs undo system.
  :ensure t
  :commands (vundo)
  :init
  (transient-append-suffix 'undo-transient '(0) ;; after the last group
    [("v" "visual undo" vundo)]))

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

(use-package window
   ;; Native window management
   :ensure nil
   :config
   (define-advice other-window (:after (&rest _))
     (pulse-momentary-highlight-one-line (point))))
   ;; (define-advice other-window (:after (&rest _))
   ;;   (window-transient))
   ;; (transient-define-prefix window-transient ()
   ;;   "Window Prefix"
   ;;   :transient-non-suffix 'transient--do-leave
   ;;   ["All"
   ;;    ("o" "other window" other-window :transient t)]))

(use-package wgrep
  ;; https://github.com/mhayashi1120/Emacs-wgrep
  ;; Adds ability to edit grep buffers in same way as wdired allows editing of
  ;; files and directories.
  :vc (:url "https://github.com/mhayashi1120/Emacs-wgrep.git"))

(use-package yaml-mode
  ;; https://github.com/yoshiki/yaml-mode
  :ensure t
  :mode ("\\.yml\\'" . yaml-mode))

;; load customization file
(load custom-file)

;;; init.el ends here
