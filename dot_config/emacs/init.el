;;; init.el  -*- lexical-binding: t; -*-

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
   visible-bell nil                  ; Don't use a visual bell
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
  (repeat-mode 1)                          ; Quickly repeat certain commands by pressing the last key
  (which-function-mode 1)                  ; Display the
  ;; Pin the server socket to a TMPDIR-independent path so emacsclient run
  ;; from sandboxed shells (e.g. Claude Code) finds the same socket as the GUI.
  ;; server-start refuses dirs with group/other access, so enforce 0700.
  (let ((dir (expand-file-name "~/.cache/emacs")))
    (unless (file-directory-p dir) (make-directory dir t))
    (set-file-modes dir #o700)
    (setq server-socket-dir dir))
  (server-start)                           ; Start Emacs server so emacsclient can attach

  ;; other settings
  (set-default-coding-systems 'utf-8) ; UTF8 by default
  (fset 'yes-or-no-p 'y-or-n-p)       ; Replace yes/no prompts with y/n
  (load-theme 'modus-operandi t)      ; Use prot's modus themes

  ;; open fullscreen by default
  ;; only relevant when using emacs as the window manager
  ;; (set-frame-parameter nil 'fullscreen 'fullboth)

  ;; only enable font if available on system
  (when (member "Comic Code" (font-family-list))
    (progn (set-frame-font "Comic Code-13:regular" nil t)
           (add-to-list 'initial-frame-alist '(font . "Comic Code-13:regular"))
           (add-to-list 'default-frame-alist '(font . "Comic Code-13:regular"))))
  ;; set fallback fonts for symbols and emoji
  (set-fontset-font t 'symbol (font-spec :family "Noto Emoji") nil 'prepend)
  (if (equal system-type 'darwin)
      (progn (set-fontset-font t 'symbol (font-spec :family "Apple Symbols") nil 'prepend)
             (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend)))
  ;; set line spacing (0.1 == 1x)
  (setq-default line-spacing 0.0)
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

(use-package chezmoi
  ;; https://github.com/tuh8888/chezmoi.el
  ;; Chezmoi is a dotfiles manager. This package exposes some chezmoi
  ;; convenience functions inside emacs.
  :ensure t)

(use-package completion-preview
  ;; Native emacs minor mode for viewing completions in a buffer with live update.
  :ensure nil
  :init (global-completion-preview-mode)
  :bind
  (:map completion-preview-active-mode-map
        ("M-n" . completion-preview-next-candidate)
        ("M-p" . completion-preview-prev-candidate))
  :custom
  (completion-preview-minimum-symbol-length 2) ; Show the preview already after two symbol characters
  (completion-preview-exact-match-only nil) ; If t, only show suggestion if there is only one candidate
  (completion-preview-idle-delay 0.3)) ; If non-nil, wait this many idle seconds before displaying preview

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
  (put 'dired-find-alternate-file 'disabled nil))

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
  :bind (:map eglot-mode-map
              :prefix-map my/eglot-prefix-map
              :prefix "C-c l"
              ("f" . eglot-format)
              ("i" . eglot-code-action-organize-imports)
              ("a" . eglot-code-actions)
              ("r" . eglot-rename)
              ("R" . eglot-reconnect)
              ("S" . eglot-shutdown))
  :config
  (defun my-eglot-organize-imports () (interactive)
         (eglot-code-actions nil nil "source.organizeImports" t))
  (add-hook 'before-save-hook 'my-eglot-organize-imports nil t)
  (add-hook 'before-save-hook 'eglot-format-buffer))

(use-package eink-theme
  ;; Minimal colorscheme
  ;; https://github.com/maio/eink-emacs
  :ensure t
  :init (load-theme 'eink t)
  :config
  ;; this theme reduces the mode line size
  (custom-set-faces
   '(mode-line ((t (:height 1.0))))
   '(mode-line-inactive ((t (:height 1.0))))))

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
  (defun my/embark-go-test ()
    "Target a go test at point of the form func Test___(_ *testing.T) { ___ }."
    (when (and (derived-mode-p 'go-mode)
               (thing-at-point-looking-at "^func \\(Test[A-Za-z0-9_]+\\)"))
      (list 'go-test (match-string 1))))

  (defun my/embark-run-go-test (test-name)
    "Run Go test with the given name."
    (let ((command (format "go test -run \"^%s$\"" test-name)))
      (compile command)))

  (add-to-list 'embark-target-finders 'my/embark-go-test)

  (add-to-list 'embark-keymap-alist '(go-test . my/embark-go-test-map))
  (defvar-keymap my/embark-go-test-map
    :doc "Keymap for go test actions."
    "t" #'my/embark-run-go-test))

(use-package eshell
  :ensure nil
  :config
  ;; this breaks on MacOS
  (if (eq system-type 'gnu/linux)
      (add-to-list 'eshell-modules-list 'eshell-smart))
  :custom
  ;; commands which should run in a dedicated terminal
  (eshell-visual-commands '("claude" "vi" "vim" "screen" "tmux" "top" "htop" "less" "more" "lynx" "links" "ncftp" "mutt" "pine" "tin" "trn" "elm"))
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
        ("M-n" . flymake-goto-next-error)
        ("M-p" . flymake-goto-prev-error))
        ;;("C-c f b" . flymake-show-buffer-diagnostics)
        ;;("C-c f r" . flymake-show-project-diagnostics))
  :custom
  ;; can also be set to short which only shows the most severe diagnostics
  ;; or fancy for detailed info below
  (flymake-show-diagnostics-at-end-of-line 'fancy))

(use-package ghostel
  ;; Terminal emulation powered by libghostty.
  :if (eq system-type 'darwin)
  :ensure t
  :custom (ghostel-eshell-visual-command-mode 't))

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

(use-package howm
  ;; https://github.com/kaorahi/howm
  ;; Opinionated note-taking and planning tool implemented in Emacs.
  :ensure t
  :init
  (require 'howm-markdown)
  ;; Use Windows host filesystem on WSL
  (if (equal system-name "capaldi-phampc")
    (setq my/howm-directory "/mnt/c/Users/xavie/notes")
  (setq my/howm-directory "/Users/xavier.capaldi/Library/CloudStorage/GoogleDrive-xavier.capaldi@optable.co/My Drive/notes"))
  :custom
  (howm-directory my/howm-directory)
  (howm-follow-theme t)
  (howm-keyword-file (expand-file-name ".howm-keys" my/howm-directory))
  (howm-history-file (expand-file-name ".howm-history" my/howm-directory))
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
  ;; (howm-file-name-format "%Y%m%dT%H%M%S.md") ;; might need thisto be in init
  ;;(howm-menu-file (expand-file-name "00000000T000000.md" my/howm-directory))
  :config
  ;; Howm's menu entry point only generates the skeleton template when
  ;; `howm-menu-file' is nil; with it set, opening the menu would just
  ;; find-file an empty buffer. So generate the skeleton into that path
  ;; ourselves the first time the menu is opened and the file is missing.
  ;; `howm-menu-copy-skel' targets `(or howm-menu-file ...)', so it writes
  ;; straight to our chosen name.
  (defun my/howm-ensure-menu-file (&rest _)
    "Generate the howm menu skeleton at `howm-menu-file' if it is missing."
    (when (and howm-menu-file (not (file-exists-p howm-menu-file)))
      (let ((menu-sym (howm-get-symbol nil "howm-menu-" howm-menu-lang)))
        (require menu-sym)
        (howm-menu-copy-skel (symbol-value menu-sym))
        (ignore-errors (howm-view-kill-buffer)))))
  (advice-add 'howm-menu :before #'my/howm-ensure-menu-file))

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

(use-package idle-highlight-mode
  ;; https://github.com/nonsequitur/idle-highlight-mode
  ;; Highlight thing under point in buffer if idle.
  :ensure t
  :hook (prog-mode . idle-highlight-mode))

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

(use-package magit-gh
  ;; https://github.com/jonathanchu/magit-gh
  ;; Interact with Github CLI from magit
  :ensure t
  :after magit)

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

(use-package mise
  ;; https://github.com/eki3z/mise.el
  ;; mise is a version manager used at my work to control developer tool
  ;; versions. This package makes tools installed by mise accessible to
  ;; emacs.
  :ensure t
  :if (eq system-type 'darwin)
  :init (global-mise-mode 1))

(use-package no-littering
  ;; https://github.com/emacscollective/no-littering
  ;; Automagically keep config directory clean.
  :ensure t
  :demand t
  :config
  ;; store backups and autosaves in centralized tmp directories
  (no-littering-theme-backups))

(use-package ob-d2
  ;; https://github.com/xcapaldi/ob-d2
  ;; Support for D2 in org mode code blocks
  :ensure t)
;;  :after org-mode)

(use-package ob-go
  ;; https://github.com/pope/ob-go
  ;; Support for Go in org mode code blocks.
  :ensure t)
;;  :after org-mode)

(use-package org
  :ensure nil
  :after (ob-d2 ob-go)
  :config
  ;; Org-babel supported languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (python . t)
     (sql . t)
     (d2 . t)
     (go . t)))
  (add-to-list 'org-modules 'org-habit)
  ;; Use Windows host filesystem on WSL
  (if (equal system-name "capaldi-phampc")
      (setq org-agenda-files (quote ("/mnt/c/Users/xavie/org/obtf.org")))
    (setq org-agenda-files (quote ("/Users/xavier.capaldi/Library/CloudStorage/GoogleDrive-xavier.capaldi@optable.co/My Drive/notes/"))))
  :custom
  (org-adapt-indentation nil)
  (org-edit-src-content-indentation 0)
  (org-tags-column 0) ;; don't right align tags
  ;; Capture
  ;; (org-capture-templates
  ;;  '(("t" "Todo" entry (file+headline " " "Tasks")
  ;;     "* TODO %?\n  %i\n  %a")
  ;;    ("j" "Journal" entry (file+olp+datetree "~/org/journal.org")
  ;;        "* %?\nEntered on %U\n  %i\n  %a")))
  ;; Task management
  (org-todo-keywords
   '((sequence "TODO(t)" "PROG(p)" "DONE(d)")))
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

(use-package prescient
  ;; https://github.com/radian-software/prescient.el
  ;; Improved completions with history
  :ensure t
  :custom (completion-styles '(prescient basic)))

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
  :bind (:map project-prefix-map
              ("b" . my/browse-at-remote)
              ("h" . ghostel-project))
  ;; display the current project in the mode line
  :custom (project-mode-line t)
  :config
  ;;(executable-find "gh") this needs to be bound to block the command below if not present
  (defun my/browse-at-remote ()
    (interactive)
    (shell-command
     (concat "gh browse "
             (file-name-nondirectory (buffer-file-name))
             ":"
             (number-to-string (line-number-at-pos)))))
  (which-key-add-keymap-based-replacements project-prefix-map
    "C-b" "list buffers"
    "!" "shell command"
    "&" "async shell command"
    "D" "dired"
    "c" "compile"
    "d" "find-dir"
    "e" "eshell"
    "f" "find-file"
    "g" "find-regexp"
    "k" "kill-buffers"
    "p" "switch-project"
    "r" "query-replace-regexp"
    "s" "shell"
    "v" "vc-dir"
    "h" "ghostel"
    "b" "browse at remote"))

(use-package protobuf-mode
  ;; https://github.com/protocolbuffers/protobuf/blob/main/editors/protobuf-mode.el
  ;; Support for Protocol Buffer (protobuf) serialized structured data syntax.
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

(use-package repeat
  ;; Native transient keybinding mode allowing repeating terminal keys.
  :ensure nil
  :init (repeat-mode 1))

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
  :ensure nil)

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
  :commands (vc-annotate))

(use-package vundo
  ;; https://github.com/casouri/vundo
  ;; Undo tree visualization for the native emacs undo system.
  :ensure t
  :commands (vundo))

(use-package which-key
  ;; Native keybind popup hints.
  :ensure nil
  :custom (which-key-mode t))

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

(use-package wgrep
  ;; https://github.com/mhayashi1120/Emacs-wgrep
  ;; Adds ability to edit grep buffers in same way as wdired allows editing of
  ;; files and directories.
  :vc (:url "https://github.com/mhayashi1120/Emacs-wgrep.git"))

(use-package yaml-mode
  ;; https://github.com/yoshiki/yaml-mode
  :ensure t
  :mode ("\\.yml\\'" . yaml-mode))

;; Load work configuration. This repository is public, so that code is kept in
;; Google Drive instead. Skip it when the drive is not mounted.
(let ((directory "~/Google Drive/My Drive/dotfiles/emacs/"))
  (when (file-directory-p directory)
    (add-to-list 'load-path directory)
    (dolist (file (directory-files directory t "\\.el\\'"))
      (load file nil t))))

;; load customization file
(load custom-file)

;;; init.el ends here
