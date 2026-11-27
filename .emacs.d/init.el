;; -*- lexical-binding: nil; -*-

;; Set default window size
(setopt initial-frame-alist '((width . 81) (height . 32)))

(setq font-name
      (if (eq system-type 'windows-nt)
          "Consolas"
        "Adwaita Mono"))
(setq font-height 110)
(set-face-attribute 'default nil :font font-name :height font-height)

(load "~/.emacs.d/basic.el" t)

(setopt org-agenda-files '("~/todo.org" "~/tasks.org"))
(setopt org-clock-mode-line-total 'today)
(setopt org-duration-format 'h:mm)
(setopt org-startup-truncated nil)
(setopt org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "SOMEDAY(s)" "DONE(d)")))
(setq org-default-notes-file "~/todo.org")
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(setq mymediawiki-highlights
      '(("<ref[^>/]*>?[^<]*\\(</ref>\\|/>\\)" . font-lock-constant-face)))
(define-derived-mode mymediawiki-mode text-mode "mymediawiki"
  "Major mode for editing MediaWiki files"
  (setq font-lock-defaults '(mymediawiki-highlights)))
(add-to-list 'auto-mode-alist '("\\.mediawiki\\'" . mymediawiki-mode))
(add-to-list 'auto-mode-alist '("\\.wikipedia\\.org" . mymediawiki-mode))
(add-hook 'mymediawiki-mode-hook
          '(lambda ()
             (setq-local sentence-end-without-space
                   (concat sentence-end-without-space "<"))))

(load "~/.emacs.d/spaced-inbox.el" t)

;; Automatically switch light/dark themes based on the system theme.
(defun my/apply-system-theme (appearance)
  (mapc #'disable-theme custom-enabled-themes)
  (when (eq appearance 'dark)
    (load-theme 'tango-dark t)))
(add-hook 'toolkit-theme-set-functions #'my/apply-system-theme)
;; Match the theme at startup too, in case the hook already ran
(when toolkit-theme
  (my/apply-system-theme toolkit-theme))

;; Stuff to do on startup
(let ((inbox-file "/home/issa/Issa/projects/notes/inbox.txt"))
    (when (file-exists-p inbox-file)
        (find-file inbox-file)
        (end-of-buffer)
        (recenter-top-bottom)
        (roll)
        (org-agenda-list)))
