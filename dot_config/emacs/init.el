(defvar elpaca-installer-version 0.5)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
;; Enable :elpaca use-package keyword.
(elpaca-use-package-mode))
;; Assume :elpaca t unless otherwise specified.
;;(setq elpaca-use-package-by-default t))

(elpaca-wait)

(use-package emacs
  :elpaca nil
  :config
  <<LOAD_THEME>>
  (when (equal system-type 'darwin)
    <<REBIND_MAC_KEYS>>
    <<MAC_FONTS>>))

(use-package asdf
  :if (eq system-type 'darwin)
  :elpaca (asdf :host github
		:repo "tabfugnic/asdf.el"
		:branch "main")
  :config (asdf-enable))

(use-package consult
  ;; Enable automatic preview at point in the *Completions* buffer.
  ;; This is relevant when you use the default completion UI,
  ;; and not necessary for Vertico, Selectrum, etc.
  :elpaca t
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind (:map isearch-mode-map
  ("C-c l" . consult-line)))

(use-package copilot
  :elpaca (copilot :host github
		   :repo "zerolfx/copilot.el"
		   :branch "main"
		   :files ("dist" "*.el"))
  :if (eq system-type 'darwin)
  :hook (prog-mode . copilot-mode))

(use-package corfu
  :elpaca t
  :init
  (global-corfu-mode))

(use-package dumb-jump
  :elpaca t
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package editorconfig
  :elpaca t
  :config (editorconfig-mode 1))

(use-package eglot
  :elpaca t
  :after (project flymake xref)
  :commands eglot)

(use-package exec-path-from-shell
  :elpaca t
  :if (memq window-system '(mac ns x))
  :init (exec-path-from-shell-initialize))

(use-package go-mode
  :elpaca t
  :mode (("\\.go\\'" . go-mode)
         ("\\.mod\\'" . go-dot-mod-mode)))

(use-package js
  :elpaca nil
  :mode "\\.js[x]\\'")

(use-package magit
  :elpaca t)

(use-package marginalia
  :elpaca t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init (marginalia-mode 1))

(use-package orderless
  :elpaca t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package protobuf-mode
  :elpaca t
  :if (eq system-type 'darwin)
  :mode ("\\.proto\\'"))

(use-package terraform-mode
  :elpaca t
  :if (eq system-type 'darwin)
  :mode ("\\.tf\\'" . terraform-mode))

(use-package typescript-mode
  :elpaca t
  :mode "\\.ts[x]\\'")

(use-package vertico
  :elpaca t
  :init (vertico-mode 1))

(use-package vundo
  :elpaca t
  :commands (vundo)
  :bind ("C-c u" . vundo))
