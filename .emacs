; for installing packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

; use evil mode
;(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)

; Equivalent of
;     nnoremap j gj
;     nnoremap k gk
;     nnoremap gj j
;     nnoremap gk k
(define-key evil-normal-state-map "j" 'evil-next-visual-line)
(define-key evil-normal-state-map "k" 'evil-previous-visual-line)
(define-key evil-normal-state-map "gj" 'evil-next-line)
(define-key evil-normal-state-map "gk" 'evil-previous-line)

; Change L, M, H to use screen lines all the time instead of hard
; lines, just like how gj and gk do screen lines.
(define-key evil-normal-state-map (kbd "L") (lambda () (interactive) (move-to-window-line-top-bottom -1)))
(define-key evil-normal-state-map (kbd "H") (lambda () (interactive) (move-to-window-line-top-bottom 0)))
(define-key evil-normal-state-map (kbd "M") (lambda () (interactive) (move-to-window-line-top-bottom)))

; enable line numbers all the time
(global-linum-mode t)

; disable tool bar
(tool-bar-mode -1)

; use 4 spaces instead of tabs
(setq-default tab-width 4 indent-tabs-mode nil)

; indent when returning
(define-key global-map (kbd "RET") 'newline-and-indent)

; show matching paren
(show-paren-mode t)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "monospace" :slant normal :height 98 :width normal)))))

; I got this from somewhere - I no longer remember where - but
; I think it's supposed to make previewing easier when writing
; LaTeX documents.
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

; load markdown mode by placing it in path
(add-to-list 'load-path "~/projects/markdown-mode")

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.page\\'" . markdown-mode))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (solarized-light)))
 '(custom-safe-themes (quote ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default))))
