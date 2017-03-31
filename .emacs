; No startup screen
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

; For installing packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(add-hook 'find-file-hooks 'turn-on-flyspell) ; turn on flyspell in most files

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor-type (quote bar))
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
      "* Mood tracking entry
  %T
  :PROPERTIES:
  :happiness: %^{Rate your happiness 1-very unhappy, 2-unhappy, 3-neutral, 4-happy, 5-very happy}
  :energy level: %^{Rate your energy level 1-very tired, 2-tired, 3-neutral, 4-alert, 5-very alert}
  :frustration: %^{Rate your frustration 1-serene, 2-calm, 3-neutral, 4-frustrated, 5-very frustrated}
  :current task: %^{What are you doing now?|%k}
  :END:
  %i%?")
     ("t" "TODO item" entry
      (file+headline "~/todo.txt" "Tasks")
      "* TODO %?
  %i"))))
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
 '(scroll-conservatively 1000)
 '(sentence-end-double-space nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(vc-follow-symlinks t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#ffffff" :foreground "#000000" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :family "Source Code Pro")))))

; MediaWiki setup
(setq sentence-end-without-space (concat sentence-end-without-space "<"))
(setq mymediawiki-highlights
      '(("<ref[^>/]*>?[^<]*\\(</ref>\\|/>\\)" . font-lock-constant-face)))
(define-derived-mode mymediawiki-mode text-mode "mymediawiki"
  "Major mode for editing MediaWiki files"
  (setq font-lock-defaults '(mymediawiki-highlights)))
(add-to-list 'auto-mode-alist '("\\.mediawiki\\'" . mymediawiki-mode))
(add-to-list 'auto-mode-alist '("\\.wikipedia\\.org" . mymediawiki-mode))

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
(add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "C-c f") 'company-complete)

(global-set-key (kbd "C-x g") 'magit-status)

;; From <https://github.com/nonsequitur/smex/>
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(setq c-default-style "linux")
(add-hook 'c-mode-hook '(lambda ()
                         (setq indent-tabs-mode t)
                         (setq tab-width 8)))
