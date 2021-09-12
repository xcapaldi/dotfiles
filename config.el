(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq straight-use-package-by-default 1)

(global-visual-line-mode 1)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq ring-bell-function 1)
(setq visible-bell t)
(blink-cursor-mode 0)

(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

(setq inhibit-startup-screen t)

(setq vc-follow-symlinks t)

(setq backup-directory-alist '(("." . "~/.config/emacs/backup"))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 20
      kept-old-versions 5)

(setq-default frame-title-format '("%b [%m]"))

(use-package hydra
  :bind ("C-c SPC" . hydra-master/body))

(defhydra hydra-master (:color blue
                               :columns 4)
  "Master"
  ("f" hydra-file/body "file")
  ("c" xcc/hydra-launcher "mode"))

(defhydra hydra-file (:color blue
                             :columns 4)
  "File"
  ("f" find-file "find file"))

(defun xcc/hydra-launcher ()
  "A launcher for code-assisting hydras based on current major mode."
  (interactive)
  (cl-case major-mode
    ('python-mode (hydra-python/body))
    (t (message "No hydra for this major mode: %s" major-mode))))

(use-package org
  :straight (:type built-in)
  :bind (:map org-mode-map
	      ;; These commands would normally add current org file to agenda.
	      ;; Better do assign them manually with org-agenda-files
	      ("C-c [" . nil)
	      ("C-c ]" . nil))
  ;; Use virtual identation
  :hook ((org-mode . org-indent-mode))
  :config
  ;; Don't indent text by default
  ;; Technically this is disabled automatically with org-indent-mode
  (setq org-adapt-indentation nil)
  ;; Define all project files or files that contain dates
  (setq org-agenda-files
	'("/home/xavier/Dropbox/org/personal.org"
	  "/home/xavier/Dropbox/org/archive.org"
	  "/home/xavier/Dropbox/org/phd.org"
	  "/home/xavier/Dropbox/org/birthdays.org"))
  ;; 
  (setq org-agenda-start-with-log-mode t)
  (setq org-deadline-warning-days 14)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-todo-keywords
	'((sequence "TODO(t)" "|" "DONE(d!)")
	  (sequence "EMAIL(e)" "WAIT(w@/!)" "HOLD(h@/!)" "MEETING(m)" "|" "CANCELLED(c@/!)")))
  ;; select a todo from any in the above list quickly
  (setq org-use-fast-todo-selection t)

  ;; Enforce todo dependencies
  ;; Parent nodes can only be finished if all children are finished
  (setq org-enforce-todo-dependencies t)
  (setq org-enforce-todo-checkbox-dependencies t)
  ;; Tasks which have unfulfilled dependencies (children or those linked by org-edna) will remain invisible on the agenda
  (setq org-agenda-dim-blocked-tasks 'invisible)

  ;; Habits
  ;;(require 'org-habit)
  (add-to-list 'org-modules 'org-habit)

  (setq org-file-apps '((auto-mode . emacs)
			("\\.mm\\'" . default)
			("\\.x?html?\\'" . default)
			("\\.pdf\\'" . "evince %s")))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t))))

  ;; Custom agenda view
  ;; Many of my tasks are blocked in my projects but some still have deadlines
  ;; that I will want a warning for
  ;;  https://stackoverflow.com/questions/29846732/make-emacs-org-mode-deadlines-and-scheduled-blocked-tasks-visible-in-agenda-view
  ;; (setq org-agenda-custom-commands
  ;; 	'(("c"
  ;; 	   "Agenda to show deadlines and hide blocked"
  ;; 	   (
  ;; 	    (agenda ""
  ;; 		    ((org-agenda-entry-types '(:deadline :scheduled))))
  ;; 	    (tags-todo "-TODO=\"DONE\""
  ;; 		       ((org-agenda-skip-entry-if 'deadline 'scheduled)
  ;; 			(org-agenda-dim-blocked-tasks 'invisible)))
  ;; 	    ))))

  ;; setup org-capture
  ;; Use franklin for capture for now
  ;; Might add this back later for notmuch integration
  ;; default directory where captures are stored
  ;; (setq org-default-notes-file "/home/xavier/Dropbox/inbox.org")

  ;; ;; Define capture templates
  ;; (setq org-capture-templates
  ;; 	  (quote (("t" "todo" entry (file "/home/xavier/Dropbox/inbox.org")
  ;; 		   "* TODO %?\n%U\n")
  ;; 		  ;;("e" "email" entry (file "/home/xavier/Dropbox/inbox.org")
  ;; 		  ;; "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :immediate-finish t)
  ;; 		  ("m" "meeting" entry (file "/home/xavier/Dropbox/inbox.org")
  ;; 		   "* MEETING with %? \n%U")))))

(use-package ivy)
  ;;:general
  ;;(:keymaps 'ivy-minibuffer-map
  ;;          "H-<return>" 'ivy-dispatching-done)
  ;;:config)
  ;;(ivy-mode 1))

(use-package counsel)
  ;;:general
  ;; This option gives some nice information about the file location and type
  ;; ("C-x b" 'counsel-ibuffer)
  ;; But this option gives a file preview
  ;;("C-x b" 'counsel-switch-buffer)
  ;;:config
  ;;(counsel-mode 1))

(use-package swiper)

(use-package ivy-bibtex
  :config
  (setq bibtex-completion-bibliography '("/home/xavier/Dropbox/library/references.bib"))
  (setq bibtex-completion-library-path '("/home/xavier/Dropbox/library"))
  (setq bibtex-completion-notes-path "/home/xavier/Dropbox/notes")
  (setq bibtex-completion-find-additional-pdfs t))
  ;; Use evince to open pdfs
  ;;(setq bibtex-completion-pdf-open-function
  ;;      (lambda (fpath)
  ;;        (call-process "evince" nil 0 nil fpath))))

(use-package org-ref
  :init
  (setq org-ref-completion-library 'org-ref-ivy-cite)
  :config
  (setq reftex-default-bibliography '("/home/xavier/Dropbox/library/references.bib"))
  (setq org-ref-bibliography-notes "/home/xavier/Dropbox/notes")
  (setq org-ref-notes-function 'org-ref-notes-function-many-files)
  (setq org-ref-default-bibliography '("/home/xavier/Dropbox/library/references.bib"))
  (setq org-ref-pdf-directory "/home/xavier/Dropbox/library")
  ;; since we use ivy-bibtex
  (setq bibtex-completion-bibliography "/home/xavier/Dropbox/library/references.bib"
	bibtex-completion-library-path "/home/xavier/Dropbox/library"
	bibtex-completion-notes-path "/home/xavier/Dropbox/notes")
  ;; format how we generate keys
  (setq bibtex-autokey-year-length 4
	bibtex-autokey-name-year-separator "-"
	bibtex-autokey-year-title-separator "-"
	bibtex-autokey-titleword-separator "-"
	bibtex-autokey-titlewords 2
	bibtex-autokey-titlewords-stretch 1
	bibtex-autokey-titleword-length 5))

(use-package pdf-tools)

(use-package elfeed
  ;;:init
  ;;(evil-set-initial-state 'elfeed-show-mode 'normal)
  ;;:general
  ;;(:keymaps 'elfeed-search-mode-map
  ;;          :states '(normal)
  ;;          "RET" 'elfeed-search-show-entry
  ;;          "S-<return>" 'elfeed-search-browse-url
  ;;          "q" 'elfeed-search-quit-window)
  ;;(:keymaps 'elfeed-show-mode-map
  ;;          :states '(normal)
  ;;          "q" 'elfeed-kill-buffer)
  ;; Tag hooks
  ;:hook ((elfeed-new-entry . (elfeed-make-tagger :feed-url "youtube\\.com"
  ;						    :add '(video)))
  ;	   (elfeed-new-entry . (elfeed-make-tagger :before "2 weeks ago"
  ;						   :remove 'unread)))

  :config
  (setq elfeed-db-directory "~/.config/emacs/elfeed")
  ;; Tag hooks
  ;;(add-hook 'elfeed-new-entry-hook
  ;;          (elfeed-make-tagger :feed-url "youtube\\.com"
  ;;                              :add '(video)))
  ;;(add-hook 'elfeed-new-entry-hook
  ;;          (elfeed-make-tagger :before "2 weeks ago"
  ;;                              :remove 'unread))

  ;; use mpv to watch youtube videos
  (setq browse-url-browser-function
	'(("https:\\/\\/www\\.youtu\\.*be." . xcc/browse-url-mpv)
	  ("." . browse-url-default-browser)))

  (defun xcc/browse-url-mpv (url &optional single)
    ;;(async-shell-command (format "mpv %s" url)))
    (start-process "mpv" nil "mpv" url))

  ;; list of feeds with autotags
  (setq elfeed-feeds
	'(;; news
	  ("https://rss.nytimes.com/services/xml/rss/nyt/HomePage.xml" news)
	  ("https://rss.nytimes.com/services/xml/rss/nyt/World.xml" news)
	  ("https://rss.nytimes.com/services/xml/rss/nyt/YourMoney.xml" news finance)
	  ("https://rss.nytimes.com/services/xml/rss/nyt/Business.xml" news finance)
	  ("https://rss.nytimes.com/services/xml/rss/nyt/EnergyEnvironment.xml" news)
	  ("https://rss.nytimes.com/services/xml/rss/nyt/Economy.xml" news finance)
	  ("https://rss.nytimes.com/services/xml/rss/nyt/Technology.xml" news)
	  ("https://rss.nytimes.com/services/xml/rss/nyt/Science.xml" news)
	  ("https://www.mcgill.ca/newsroom/channels_item/19/rss" news mcgill)
	  ;; scientific journals
	  ("http://feeds.rsc.org/rss/sm" literature)
	  ("https://feeds.feedburner.com/acs/mamobx" literature)
	  ("https://feeds.feedburner.com/acs/nalefd" literature)
	  ("http://feeds.aps.org/rss/recent/pre.xml" literature)
	  ("http://feeds.aps.org/rss/recent/prl.xml" literature)
	  ("http://feeds.aps.org/rss/recent/physics.xml" literature)
	  ("http://feeds.aps.org/rss/presuggestions.xml" literature)
	  ("http://feeds.aps.org/rss/recent/prlsuggestions.xml" literature)
	  ("http://feeds.nature.com/ncomms/rss/current" literature)
	  ("http://feeds.nature.com/nature/rss/current" literature)
	  ("https://science.sciencemag.org/rss/current.xml" literature)
	  ("https://science.sciencemag.org/rss/ec.xml" literature)
	  ;; project updates
	  ("https://github.com/xcapaldi.private.atom?token=AJIXGKSORO2P4YZ7IJ37VB56CRCUO" github)
	  ("https://suckless.org/atom.xml")
	  ;; videos on gaming
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UC21uZkfXpT8rPY-gPgMiCwA" gaming)
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCfSVMX8vs7xA_hqFcuFqgwQ" gaming)
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UC2eEGT06FrWFU6VBnPOR9lg" gaming)
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UC3wxqeB1gIxdw6YKueea5Jg" gaming)
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCD6VugMZKRhSyzWEWA9W2fg" gaming)
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCKlUrYO3i9MDlL45Ia6j5EA" gaming)
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCqJ-Xo29CKyLTjn6z2XwYAw" gaming)
	  ;; videos on coding
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCKTehwyGCKF-b2wo0RKwrcg"  programming)
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCsUalyRg43M8D60mtHe6YcA"  programming)
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UC9-y-6csu5WGm29I7JiwpnA"  programming)
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCW6TXMZ5Pq6yL6_k5NZ2e0Q"  programming)
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCvjgXvBlbQiydffZU7m1_aw"  programming)
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UC-yuWVUplUJZvieEligKBkA"  programming)
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCRLEADhMcb8WUdnQ5_Alk7g" programming)
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCRLEADhMcb8WUdnQ5_Alk7g" programming)
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCyrF_lsKS9kQ3OUKQkgYh3A" programming)
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCYNrBrBOgTfHswcz2DdZQFA" programming python)
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCaoqVlqPTH78_xjTjTOMcmQ" programming)
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCD6ArU-AYbfIj5sx2L4SZAQ" programming)
	  ;; videos on electronics
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UC8uT9cgJorJPWu7ITLGo9Ww" programming electronics)
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UC6mIxFTvXkWQVEHPsEdflzQ" electronics)
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UC5I2hjZYiW9gZPVkvzM8_Cw" electronics)
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCS0N5baNlQWJCUrhCEo8WlA" electronics)
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCUW49KGPezggFi0PGyDvcvg" electronics)
	  ;; videos on emacs
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCxkMDXQ5qzYOgXPRnOBrp1w" emacs)
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UC0uTPqBCFIpZxlz_Lv1tk_g" emacs)
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCDEtZ7AKmwS0_GNJog01D2g" emacs)
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCAiiOTio8Yu69c3XnR7nQBQ" emacs)
	  ;; videos on vim
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCUR1pFG_3XoZn3JNKjulqZg" vim)
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UC8ENHE5xdFSwx71u3fDH5Xw" vim)
	  ;; videos on history
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCWnlQMQ-ACfhpD68yWRsnJw" history)
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCMjlDOf0UO9wSijFqPE9wBw" history)
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCsaGKqPZnGp_7N80hcHySGQ" history cooking)
	  ;; cooking
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCYDLmV1b0kvF8jY491dtyHg" cooking)
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCMmZEL8jV1B61NKAXcyW87A" cooking)
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCmXkJ9ReY5hjvYPcnmBwing" cooking)
	  ;; music
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCG7AaCh_CiG6pq_rRDNw72A" music)
	  ;; blogs
	  ("https://feeds.feedburner.com/TheKitchinResearchGroup" emacs)
	  ("https://karthinks.com/index.xml" emacs)
	  ("https://nullprogram.com/feed/" programming)
	  ("http://pragmaticemacs.com/feed/" emacs)
	  ("http://esr.ibiblio.org/?feed=rss2")
	  ("https://www.calnewport.com/blog/feed/")
	  ;; other
	  ("https://www.nngroup.com/feed/rss/")
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UC7mu94v1zFZU8pgNX13dHsQ" vietnamese)
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCipg-xAE_rNtL8kaG4ezFAQ" nanopore)
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCDXTQ8nWmx_EhZ2v-kp7QxA" finance)
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCtg1eIVmfwXnO0ipN84-a6g" ergonomics)
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCbfYPyITQ-7l4upoX8nvctg")
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCFtOX-21N1earf-K58C7HjQ" keyboard)
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCD0y51PJfvkZNe3y3FR5riw" keyboard)
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UC2eYFnH61tmytImy1mTYvhA")
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCyRhIGDUKdIOw07Pd8pHxCw")
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCpnkp_D4FLPCiXOmDhoAeYA")
	  ("https://www.youtube.com/feeds/videos.xml?channel_id=UCw03U5DZGLqvv5elJvXvR0Q")
	  )))

(add-to-list 'default-frame-alist
             '(font . "Fira Mono-9"))

(use-package modus-themes
  :init
  ;; add all customizations before loading theme

  ;; load theme files before enabling
  (modus-themes-load-themes)
  :config
  ;; load theme of choice
  (modus-themes-load-operandi))

(use-package python 
  :straight (:type built-in)
  ;;:mode ("\\.py\\" . python-mode)
  ;;:interpreter ("python" . python-mode)
  :config
  (defhydra hydra-python (:color blue
                               :columns 4)
    "Coding"
    ("q" hydra-master/body "backlick")))

(use-package notmuch
  :straight (:type built-in))

(setq mail-specify-envelope-from t)
(setq message-sendmail-envelope-from 'header)
(setq mail-envelope-from 'header)
(setq message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "/bin/msmtp"
      user-full-name "Xavier Capaldi")

(use-package gnus-alias
  :hook ('message-setup . gnus-alias-determine-identity)
  :config
  (setq gnus-alias-use-buttonized-from nil)
  (setq gnus-alias-identity-alist
        '(("physics"
           nil ;; refer to any identity
           "Xavier Capaldi <capaldix@physics.mcgill.ca>"
           nil ;; organization header
           (("Fcc" . "/physics.mcgill/Sent")) ;; extra headers to save outgoing mail
           nil ;; extra body text
           nil) ;; signature
           ("scribo"
           nil ;; refer to other identity
           "Xavier Capaldi <xcapaldi@scribo.biz>"
           nil ;; organization header
           (("Fcc" . "/scribo/Sent")) ;; extra headers to save outgoing mail
           nil ;; extra body text
           nil) ;; signature
           ("giftedfleece"
            nil ;; refer to other identity
            "Xavier Capaldi <beeboy@giftedfleece.com>"
            nil ;; organization header
            (("Fcc" . "/giftedfleece/Sent")) ;; extra headers to save outgoing mail
            nil ;; extra body text
            nil) ;; signature
            ("gmail"
             nil ;; refer to other identity
             "Xavier Capaldi <xavier.capaldi@gmail.com>"
             nil ;; organization header
             (("Fcc" . "/gmail/Sent")) ;; extra headers to save outgoing mail
             nil ;; extra body text
             nil))) ;; signature
  ;; use "physics" identity by default
  (setq gnus-alias-default-identity "physics")
  ;; define rules to match other identities
  (setq gnus-alias-identity-rules
        '(("scribo" ("any"
                     "xcapaldi@scribo.biz"
                     both)
           "scribo")
          ("giftedfleece" ("any"
                           "beeboy@giftedfleece.com"
                           both)
           "giftedfleece")
          ("gmail" ("any"
                    "xavier.capaldi@gmail.com"
                    both)
           "gmail"))))
