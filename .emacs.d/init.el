;; No startup screen
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; I like Emacs to take up about half of the screen, but this depends
;; on the screen in use so might need to be adjusted depending on the
;; computer.
(setq initial-frame-alist
          '((width . 84) (height . 40)))

;; For installing packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; For Japanese input. I like to install this from the Ubuntu
;; repository rather than melpa. The package is called emacs-mozc.
(require 'mozc nil 'noerror)

;; Use IPAexGothic for Japanese text. From
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Fontsets.html
(when (fboundp 'set-fontset-font)
  (set-fontset-font t 'japanese-jisx0208 (font-spec :family "IPAexGothic")))

;; Turn on flyspell in most files
;; (add-hook 'find-file-hooks 'turn-on-flyspell)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; This is already bound to C-; which only works in the GUI version of
;; Emacs, so provide a binding for terminal Emacs.
(global-set-key (kbd "C-c s") 'flyspell-auto-correct-previous-word)

;; hunspell provides better spelling suggestions in my opinion.
(when (file-exists-p "/usr/bin/hunspell")
  (setq ispell-program-name "/usr/bin/hunspell"))

;; Override some colors that the MATE theme sets
(set-face-attribute 'region nil :background "#eeeeee")
(set-face-attribute 'default nil
                    :font "Ubuntu Mono"
                    :height 110
                    :background "#ffffff"
                    :foreground "#333333")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(indent-tabs-mode nil)
 '(magit-diff-refine-hunk (quote all))
 '(make-backup-files nil)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (3 ((shift) . 1) ((control)))))
 '(org-agenda-files (quote ("~/todo.org")))
 '(org-capture-templates
   (quote
    (("i" "Idea" entry
      (file "~/org/notes.org")
      "* %T %?")
     ("t" "TODO item" entry
      (file+headline "~/todo.org" "Tasks")
      "* TODO %?
  %i"))))
 '(org-startup-truncated nil)
 '(org-todo-keywords
   (quote
    ((sequence "TODO(t)" "WAITING(w)" "SOMEDAY(s)" "DONE(d)"))))
 '(package-selected-packages (quote (php-mode lua-mode markdown-mode jedi intero magit)))
 '(require-final-newline t)
 '(save-interprogram-paste-before-kill t)
 '(scroll-conservatively 1000)
 '(sentence-end-double-space nil)
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(tool-bar-mode nil)
 '(vc-follow-symlinks t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; MediaWiki setup
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

(setq org-default-notes-file "~/todo.org")
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c b") 'org-iswitchb)

;; Make C-w work as in Bash.
;; See https://www.emacswiki.org/emacs/ZapUpToChar for zap-up-to-char.
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." 'interactive)
(global-set-key (kbd "C-w")
                '(lambda () (interactive)
                   (if (region-active-p)
                       (kill-region (region-beginning) (region-end))
                     (zap-up-to-char -1 ?\s))))

(when (fboundp 'magit-diff-buffer-file)
  ;; This is like ":Git diff %" in fugitive.vim
  (global-set-key (kbd "C-x C-d")
                  '(lambda () (interactive)
                     (magit-diff-buffer-file)
                     (setq truncate-lines nil)
                     (diff-refine-hunk)
                     (delete-other-windows))))

(add-hook 'magit-diff-mode-hook
          '(lambda () (setq-local truncate-lines nil)))

(add-hook 'haskell-mode-hook 'intero-mode)

;; This works in terminal Emacs as well, and is like C-y
(defun paste-clipboard ()
  "Use xsel to paste content of clipboard at point"
  (interactive)
  (insert (shell-command-to-string "xsel -ob")))

;; This works in terminal Emacs as well, and is like M-w
(defun copy-clipboard ()
  "Use xsel to copy region into clipboard"
  (interactive)
  (shell-command-on-region
   (region-beginning)
   (region-end)
   "xsel -ib"))

(defun insert-today ()
  "Insert today's date in YYYY-mm-dd format"
  (interactive)
  (insert (substring (shell-command-to-string "date -Idate")
                     0
                     -1)))

(ido-mode t)

(when (fboundp 'magit-status)
  (global-set-key (kbd "C-x g") 'magit-status))

(setq c-default-style "linux")
(add-hook 'c-mode-hook '(lambda ()
                         (setq indent-tabs-mode t)
                         (setq tab-width 8)))

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

(server-start)
