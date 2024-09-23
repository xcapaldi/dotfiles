;;; early-init.el

;;; Commentary:

;; The early init file is loaded before the graphical or package systems.
;; I use to temporarily disable the package system and to disable any
;; unnecessary graphical components.

;;; Code:...

;; disable package loading initially
(setq package-enable-at-startup nil)

;; set the customize file so it doesn't pollute init.el
(setq custom-file (concat user-emacs-directory "custom.el"))

;; disable start screen, scratch message and tool bar
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; disable unused graphical components
(tool-bar-mode -1)
(menu-bar-mode -1)
(set-scroll-bar-mode 'right)
(scroll-bar-mode -1)

;;; early-init.el ends here
