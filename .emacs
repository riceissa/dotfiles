; for installing packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

; use evil mode
(require 'evil)
(evil-mode 1)

; make emacs state the default
(setq evil-default-state 'emacs)

; don't even use evil insert state; instead, just use regular emacs
; state
; from https://stackoverflow.com/questions/25542097/emacs-evil-mode-how-to-change-insert-state-to-emacs-state-automaticly
(defalias 'evil-insert-state 'evil-emacs-state)

; Equivalent of
;     nnoremap j gj
;     nnoremap k gk
;     nnoremap gj j
;     nnoremap gk k
; TODO make this work with visual state as well
(define-key evil-normal-state-map "j" 'evil-next-visual-line)
(define-key evil-normal-state-map "k" 'evil-previous-visual-line)
(define-key evil-normal-state-map "gj" 'evil-next-line)
(define-key evil-normal-state-map "gk" 'evil-previous-line)

(define-key evil-normal-state-map (kbd "K") (lambda () (interactive) (evil-buffer nil)))

; Change L, M, H to use screen lines all the time instead of hard
; lines, just like how gj and gk do screen lines.
;; TODO: make this work in visual mode as well
(define-key evil-normal-state-map (kbd "L") (lambda () (interactive) (move-to-window-line-top-bottom -1)))
(define-key evil-normal-state-map (kbd "H") (lambda () (interactive) (move-to-window-line-top-bottom 0)))
(define-key evil-normal-state-map (kbd "M") (lambda () (interactive) (move-to-window-line-top-bottom)))

; let evil insert use emacs keybindings instead of vim insert keybindings
;(setcdr evil-insert-state-map nil)
;(define-key evil-insert-state-map
;            (read-kbd-macro evil-toggle-key) 'evil-normal-state)
;(define-key evil-insert-state-map [escape] 'evil-normal-state)
; i don't think this one is valid:
;(define-key evil-normal-state-map
;            (read-kbd-macro evil-insert-toggle-key) 'evil-emacs-state)

; FIXME make this paste to the current spot the formatted link of the
; URL in the clipboard, like in my Vim configuration; it would
; actually be handy too if this coul be added for emacs in general and
; not just evil insert state.
;(define-key evil-insert-state-map (kbd "C-b") (lambda () (interactive) (shell-command "autolink.py ")))

; enable line numbers all the time
(global-linum-mode t)

; nnoremap Y y$
(setq evil-want-Y-yank-to-eol t)

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
 '(default ((t (:family "monospace" :slant normal :height 102 :width normal)))))

; I got this from somewhere - I no longer remember where - but
; I think it's supposed to make previewing easier when writing
; LaTeX documents.
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

; wrap lines in org mode
(setq org-startup-truncated nil)

; from https://tex.stackexchange.com/questions/27241/entering-math-mode-in-auctex-using-and
(add-hook 'LaTeX-mode-hook
  '(lambda ()
    (define-key TeX-mode-map "\C-cm" 'TeX-insert-inline-math)
    (defun TeX-insert-inline-math (arg)
      "Like TeX-insert-brackes but for \(...\)" (interactive "P")
      (if (TeX-active-mark)
        (progn
          (if (< (point) (mark)) (exchange-point-and-mark))
          (insert "\\)")
          (save-excursion (goto-char (mark)) (insert "\\(")))
          (insert "\\(")
          (save-excursion
            (if arg (forward-sexp (prefix-numeric-value arg)))
            (insert "\\)"))))))

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
