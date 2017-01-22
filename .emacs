; No startup screen
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

; For installing packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

; (add-to-list 'auto-mode-alist '("\\.mediawiki\\'" . mediawiki-mode))

; magit settings
(global-set-key (kbd "C-x g") 'magit-status)
; magit status should wrap lines
; from https://emacs.stackexchange.com/questions/2890/how-to-make-truncate-lines-nil-and-auto-fill-mode-off-in-magit-buffers
(add-hook 'magit-status-mode-hook
          (lambda ()
            (setq truncate-lines nil)))
(add-hook 'find-file-hooks 'turn-on-flyspell) ; turn on flyspell in most files

; markdown mode
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.page\\'" . markdown-mode))
(setq markdown-command "pandoc -f markdown -t html5 --mathjax -Ss")

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
 '(org-file-apps
   (quote
    ((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . "atril %s"))))
 '(org-startup-truncated nil)
 '(save-interprogram-paste-before-kill t)
 '(sentence-end-double-space nil)
 '(show-paren-mode t)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(vc-follow-symlinks t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "MS  " :family "Consolas")))))

(ido-mode t)
