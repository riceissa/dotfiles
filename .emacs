; for installing packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

; another thing to do when first setting up emacs:
; see https://stackoverflow.com/questions/22710964/emacs24-not-rendering-fonts-properly
; in essence: in ~/.Xresources , add:
;     Xft.hintstyle:  hintfull
;     Xft.lcdfilter:  lcddefault
; this will improve font rendering; in particular, Source Code Pro will
; look much nicer.

; no startup screen
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

; use evil mode
(require 'evil)
(evil-mode 1)

; make emacs state the default
(setq evil-default-state 'emacs)

; don't even use evil insert state; instead, just use regular emacs
; state
; from https://stackoverflow.com/questions/25542097/emacs-evil-mode-how-to-change-insert-state-to-emacs-state-automaticly
; this causes some problems because doing 'I' after multi-line visual block
; will not insert in each row, but that's the only problem I've had so far.
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

; nnoremap K <C-^>
(define-key evil-normal-state-map (kbd "K") (lambda () (interactive) (evil-buffer nil)))

; Change H, M, L to use screen lines all the time instead of logical
; lines, just like how gj and gk do screen lines.
;; TODO: make this work in visual mode as well
(define-key evil-normal-state-map (kbd "H") (lambda () (interactive) (move-to-window-line-top-bottom 0)))
(define-key evil-normal-state-map (kbd "M") (lambda () (interactive) (move-to-window-line-top-bottom)))
(define-key evil-normal-state-map (kbd "L") (lambda () (interactive) (move-to-window-line-top-bottom -1)))

; TODO: make this paste to the current spot the formatted link of the
; URL in the clipboard, like in my Vim configuration; it would
; actually be handy too if this could be added for emacs in general
; and not just evil insert state.
; UPDATE: evil insert no longer exists in my setup, so now just get
; this working for emacs proper
;(define-key evil-insert-state-map (kbd "C-b") (lambda () (interactive) (shell-command "autolink.py ")))

; nnoremap Y y$
(setq evil-want-Y-yank-to-eol t)

; More regular emacs configuration (not Evil)
(global-linum-mode t)   ; enable line numbers all the time
(tool-bar-mode -1)   ; disable tool bar
(setq-default tab-width 4 indent-tabs-mode nil)  ; use 4 spaces instead of tabs
(define-key global-map (kbd "RET") 'newline-and-indent)   ; indent when returning
(show-paren-mode t)   ; show matching paren
; enable flyspell mode by default; see https://stackoverflow.com/questions/15891808/emacs-how-to-enable-automatic-spell-check-by-default
;(add-hook 'text-mode-hook 'flyspell-mode)
;(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'find-file-hooks 'turn-on-flyspell)

; Settings from Custom
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :slant normal :height 98 :width normal)))))

; I got this from somewhere - I no longer remember where - but I think
; it's supposed to make previewing easier when writing LaTeX
; documents. It might not even be necessary, and I should at some
; point document what this does...
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(setq org-startup-truncated nil)   ; wrap lines in org mode

; from https://tex.stackexchange.com/questions/27241/entering-math-mode-in-auctex-using-and
; basically, allow C-c m to enter math in LaTeX with delimiters
; \(...\) instead of the TeX $...$ (you'd think AUCTeX was smart
; enough to do this, but alas...)
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

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.page\\'" . markdown-mode))

; more settings from Custom
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (solarized-light)))
 '(custom-safe-themes (quote ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default))))
