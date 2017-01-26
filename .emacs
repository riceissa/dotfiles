; No startup screen
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

; For installing packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(require 'evil)
(evil-mode 1)
(define-key evil-insert-state-map (kbd "C-a") nil)
(define-key evil-insert-state-map (kbd "C-d") nil)
(define-key evil-insert-state-map (kbd "C-e") nil)

(add-hook 'find-file-hooks 'turn-on-flyspell) ; turn on flyspell in most files

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(global-linum-mode t)
 '(indent-tabs-mode nil)
 '(ispell-program-name "/usr/bin/hunspell")
 '(make-backup-files nil)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (3 ((shift) . 1) ((control)))))
 '(org-agenda-files (quote ("~/todo.txt")))
 '(org-capture-templates
   (quote
    (("m" "Mood" entry
      (file+headline "~/todo.txt" "Mood tracking")
      "* %T\n  happiness: %^{Rate your happiness from 0-10}\n  energy level: %^{Rate your energy level from 0-10}\n  frustration: %^{Rate your frustration from 0-10}\n  current task: %^{What are you doing now?|%k}\n  %i%?")
     ("t" "TODO item" entry
      (file+headline "~/todo.txt" "Tasks")
      "* TODO %?\n  %i"))))
 '(org-file-apps
   (quote
    ((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . "atril %s"))))
 '(org-startup-truncated nil)
 '(org-todo-keywords
   (quote
    ((sequence "TODO(t)" "WAITING(w)" "SOMEDAY(s)" "DONE(d)"))))
 '(save-interprogram-paste-before-kill t)
 '(sentence-end-double-space nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(vc-follow-symlinks t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "MS  " :family "Consolas")))))

; MediaWiki setup
(setq sentence-end-without-space (concat sentence-end-without-space "<"))
(setq mymediawiki-highlights
      '(("<ref[^>/]*>?[^<]*\\(</ref>\\|/>\\)" . font-lock-constant-face)))
(define-derived-mode mymediawiki-mode text-mode "mymediawiki"
  "Major mode for editing MediaWiki files"
  (setq font-lock-defaults '(mymediawiki-highlights)))
(add-to-list 'auto-mode-alist '("\\.mediawiki\\'" . mymediawiki-mode))
(add-to-list 'auto-mode-alist '("\\.wikipedia\\.org" . mymediawiki-mode))

; (evil-set-initial-state 'org-mode 'emacs)
(setq evil-default-state 'emacs)

(setq org-default-notes-file "~/todo.txt")
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c b") 'org-iswitchb)

; Quickly make a Git snapshot of the file on disk
(global-set-key
 (kbd "C-x w")
 (lambda ()
   (interactive)
   (shell-command
    (concat "git add "
            buffer-file-name
            "; git commit -m 'snapshot'"))))

(ido-mode t)
