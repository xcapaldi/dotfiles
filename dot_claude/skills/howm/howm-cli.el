;;; howm-cli.el --- Batch wrappers around howm for CLI use -*- lexical-binding: t -*-

;; Load with:
;;   emacs --batch \
;;     -l ~/.config/emacs/early-init.el \
;;     -l ~/.config/emacs/init.el \
;;     -l ~/.claude/skills/howm/howm-cli.el \
;;     --eval '(howm-cli-todo)'
;;
;; Every command prints a single JSON document to stdout and nothing else
;; (errors and warnings still go to stderr).  The user's howm-directory and
;; related settings are honored because the user's full init has already run.

(require 'howm)
(require 'howm-reminder)
(require 'howm-view)
(require 'json)
(require 'cl-lib)

(defun howm-cli--item->alist (item &optional include-priority)
  "Convert a howm item to a JSON-friendly alist.
When INCLUDE-PRIORITY is non-nil, attach the computed todo priority."
  (let* ((name (howm-item-name item))
         (page (howm-item-page item))
         (summary (howm-item-summary item))
         (place (howm-item-place item))
         (base (list (cons 'file (or name ""))
                     (cons 'basename (and name (file-name-nondirectory name)))
                     (cons 'summary (or summary ""))
                     (cons 'place place))))
    (when (and include-priority page)
      (push (cons 'priority (howm-todo-priority item)) base))
    (nreverse base)))

(defun howm-cli--emit (data)
  "Print DATA as JSON and a trailing newline."
  (let ((json-encoding-pretty-print nil))
    (princ (json-encode data)))
  (princ "\n"))

(defun howm-cli--task-items-with-priority ()
  "Return reminder items sorted by computed howm priority (high first).
Mirrors `howm-list-todo' for `+', `-', `~' (and inherited types)."
  (let ((items (howm-list-todo-sub-setup-items nil)))
    ;; Drop separator items (page is nil) for clean machine output.
    (cl-remove-if-not #'howm-item-page items)))

(defun howm-cli--items-for-types (types)
  "Run a reminder search restricted to TYPES (a string of marker chars)."
  (howm-reminder-search types))

;;; ----------------------------------------------------------------- listing

(defun howm-cli-todo ()
  "Open todo/reminder/defer items with howm's priority."
  (let ((items (howm-cli--task-items-with-priority)))
    (howm-cli--emit
     (mapcar (lambda (it) (howm-cli--item->alist it t)) items))))

(defun howm-cli-schedule ()
  "Schedule events and deadlines (`@' and `!')."
  (let ((items (howm-cli--items-for-types howm-schedule-types)))
    (howm-cli--emit
     (mapcar (lambda (it) (howm-cli--item->alist it nil)) items))))

(defun howm-cli-all-tasks ()
  "Every reminder of any marker, sorted by date inside the file."
  (let ((items (howm-cli--items-for-types howm-reminder-types)))
    (howm-cli--emit
     (mapcar (lambda (it) (howm-cli--item->alist it nil)) items))))

(defun howm-cli-all ()
  "Every note in the howm directory (one entry per file)."
  (let ((items (howm-folder-items howm-directory t)))
    (howm-cli--emit
     (mapcar (lambda (it) (howm-cli--item->alist it nil)) items))))

(defun howm-cli-recent (&optional days)
  "Notes modified in the last DAYS days (default `howm-list-recent-days')."
  (let* ((d (or days howm-list-recent-days))
         (now (current-time))
         (from (howm-days-before now d))
         (item-list (howm-recent-items-filter
                     (howm-folder-items howm-directory t)))
         (items (howm-filter-items-by-mtime item-list from now)))
    (howm-cli--emit
     (mapcar (lambda (it) (howm-cli--item->alist it nil)) items))))

(defun howm-cli-today ()
  "Notes whose content contains today's date (links, tasks, timestamps)."
  (howm-cli-search-date (format-time-string "%Y-%m-%d")))

(defun howm-cli-yesterday ()
  "Notes whose content contains yesterday's date."
  (howm-cli-search-date
   (format-time-string "%Y-%m-%d" (howm-days-before (current-time) 1))))

(defun howm-cli-search-date (date-string)
  "Notes whose content contains DATE-STRING (YYYY-MM-DD)."
  (let* ((trio (howm-call-view-search-internal date-string nil nil))
         (items (cl-caddr trio)))
    (howm-cli--emit
     (mapcar (lambda (it) (howm-cli--item->alist it nil)) items))))

;;; ----------------------------------------------------------------- search

(defun howm-cli-search (pattern &optional fixed-p)
  "Grep the wiki for PATTERN.
PATTERN is a fixed string by default; pass FIXED-P=nil for regex."
  (let* ((trio (howm-call-view-search-internal pattern (or fixed-p t) nil))
         (items (cl-caddr trio)))
    (howm-cli--emit
     (mapcar (lambda (it) (howm-cli--item->alist it nil)) items))))

(defun howm-cli-search-regexp (pattern)
  "Grep the wiki using PATTERN as a regular expression."
  (howm-cli-search pattern nil))

(defun howm-cli-backlinks (target)
  "Notes that reference TARGET (filename or absolute path)."
  (let ((pat (file-name-nondirectory target)))
    (howm-cli-search pat t)))

;;; ----------------------------------------------------------------- keywords

(defun howm-cli-keywords ()
  "Every `<<<' keyword registered in `.howm-keys'."
  (let ((keys (howm-keyword-list)))
    (howm-cli--emit
     (mapcar (lambda (k)
               ;; A keyword entry can be a string or (canonical alias ...).
               (cond
                ((stringp k) (list (cons 'keyword k) (cons 'aliases [])))
                ((listp k)
                 (list (cons 'keyword (car k))
                       (cons 'aliases (vconcat (cdr k)))))))
             keys))))

(defun howm-cli-keyword-add (keyword)
  "Append KEYWORD to `.howm-keys' if missing."
  (let* ((before (howm-keyword-list))
         (already (cl-find keyword before
                           :key (lambda (e) (if (stringp e) e (car e)))
                           :test #'string=)))
    (unless already
      (let ((file (howm-keyword-file)))
        (with-temp-buffer
          (when (file-exists-p file) (insert-file-contents file))
          (goto-char (point-max))
          (unless (or (bobp) (bolp)) (insert "\n"))
          (insert keyword "\n")
          (write-region (point-min) (point-max) file))))
    (howm-cli--emit
     (list (cons 'keyword keyword)
           (cons 'added (not already))))))

(defun howm-cli-keyword-remove (keyword)
  "Remove KEYWORD from `.howm-keys' if present.
Rewrites the file by hand to avoid howm's read-only buffer protection."
  (let* ((file (howm-keyword-file))
         (removed nil))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward (concat "^" (regexp-quote keyword) "$") nil t)
          (delete-region (line-beginning-position)
                         (min (point-max) (1+ (line-end-position))))
          (setq removed t))
        (write-region (point-min) (point-max) file)))
    (howm-cli--emit
     (list (cons 'keyword keyword) (cons 'removed removed)))))

;;; ----------------------------------------------------------------- write

(defun howm-cli-create (title &optional body)
  "Create a new note with TITLE and optional BODY.
The title-header character comes from `howm-view-title-header'
(e.g. `=' for txt notes, `#' for markdown).
Returns the new file's path as JSON."
  (let* ((ts (format-time-string howm-file-name-format))
         (path (expand-file-name ts howm-directory))
         (dir (file-name-directory path))
         (header (or howm-view-title-header "=")))
    (unless (file-directory-p dir) (make-directory dir t))
    (with-temp-file path
      (insert (format "%s %s\n[%s]\n\n" header title
                      (format-time-string "%Y-%m-%d %H:%M")))
      (when body (insert body) (unless (string-suffix-p "\n" body) (insert "\n"))))
    (howm-cli--emit
     (list (cons 'file path)
           (cons 'title title)))))

;;; ----------------------------------------------------------------- meta

(defun howm-cli-config ()
  "Echo the active howm configuration as JSON."
  (howm-cli--emit
   (list (cons 'howm-version howm-version)
         (cons 'howm-directory (expand-file-name howm-directory))
         (cons 'howm-keyword-file (expand-file-name howm-keyword-file))
         (cons 'howm-history-file (expand-file-name howm-history-file))
         (cons 'howm-file-name-format howm-file-name-format)
         (cons 'howm-list-recent-days howm-list-recent-days))))

(provide 'howm-cli)
;;; howm-cli.el ends here
