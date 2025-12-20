(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Set default window size
(setopt initial-frame-alist '((width . 81) (height . 32)))

(setq font-name
      (if (eq system-type 'windows-nt)
          "Consolas"
        "Adwaita Mono"))
(setq font-height 110)
(set-face-attribute 'default nil :font font-name :height font-height)

(load "~/.emacs.d/basic.el" t)

;; hunspell provides better spelling suggestions in my opinion.
;; To get hunspell on Windows, first install
;; msys2 (https://www.msys2.org/), then follow the instructions
;; in this answer https://emacs.stackexchange.com/a/45752/31094
;; to get hunspell installed via msys2 (the instructions
;; are for aspell but it's very similar). Once hunspell is
;; installed, the configuration below should automatically work.
;; Also it is kind of bizarre that Emacs (which in all other areas
;; eschews the Unix philosophy) is that one that needs
;; an external program to do spell checking, whereas Vim comes
;; with its own (very excellent -- way better than hunspell)
;; spell checker.
(let ((spell-program-location (if (eq system-type 'windows-nt)
                                  "C:/msys64/mingw64/bin/hunspell.exe"
                                  "/usr/bin/hunspell")))
    (when (file-exists-p spell-program-location)
        (setopt ispell-program-name spell-program-location)
        (setenv "DICTIONARY" "en_US")))

;; Turn on flyspell in most files
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; Emacs already binds C-; to flyspell-auto-correct-previous-word.
;; However, in my experience this command has a couple of bugs:
;; 1. Sometimes it shifts the screen as if I had typed C-l or something.
;; 2. It often misses misspelled words (that actually have a red squiggly
;;    underline) that are closer to point and tries to fix words farther
;;    away from point.
;; 3. Possibly related to the bug in (1), if I have multiple copies of
;;    the same buffer (at different locations) open in different panes,
;;    then the pane that I am not in will also be shifted.
;; I may eventually need to roll my own elisp code to get the behavior I
;; want (basically Vim's insert-mode C-x C-s), but I did notice while
;; playing around that flyspell mode has this other command to look
;; backwards to fix a spelling mistake, so I'll be trying it for now.
(eval-after-load "flyspell"
    '(define-key flyspell-mode-map (kbd "C-;")
         'flyspell-check-previous-highlighted-word))

(setopt org-agenda-files '("~/todo.org"))
(setopt org-capture-templates
      '(("i" "Idea" entry (file "~/notes.org") "* %T %?")
           ("t" "TODO item" entry (file+headline "~/todo.org" "Tasks")
               "* TODO %?\12  %i\12")))
(setopt org-clock-mode-line-total 'today)
(setopt org-duration-format 'h:mm)
(setopt org-format-latex-options
      '(:foreground default :background default :scale 1.3 :html-foreground
           "Black" :html-background "Transparent" :html-scale 1.0
           :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))
(setopt org-startup-truncated nil)
(setopt org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "SOMEDAY(s)" "DONE(d)")))
(setopt preview-scale-function 1.2)
(setq org-default-notes-file "~/todo.org")
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
      '("ff0d5f6adf3182829eee9ace64d2e32bc0f0e00890eb54879cac364be9dc243f"
           default))
 '(package-selected-packages '(auctex auto-dark magit markdown-mode s)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-code-face ((t nil)))
 '(markdown-inline-code-face ((t nil))))

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

(setopt magit-diff-refine-hunk 'all)
(when (fboundp 'magit-diff-buffer-file)
  ;; This is like ":Git diff %" in fugitive.vim
  (global-set-key (kbd "C-x C-d")
                  '(lambda () (interactive)
                     (magit-diff-buffer-file)
                     (setq truncate-lines nil)
                     (diff-refine-hunk)
                     (delete-other-windows))))
(add-hook 'magit-mode-hook
    #'(lambda () (setq-local truncate-lines nil)))
(when (fboundp 'magit-status)
  (global-set-key (kbd "C-x g") 'magit-status))
;; For pushing with git on windows
(when (eq system-type 'windows-nt)
    (setenv "SSH_ASKPASS" "git-gui--askpass"))

;; from https://tex.stackexchange.com/a/392038/18026
(add-hook 'LaTeX-mode-hook
          (lambda () (set (make-variable-buffer-local 'TeX-electric-math)
                          (cons "\\(" "\\)"))))

;; How to create the "default"/issa-test theme:
;; 1. Disable all theme-related emacs configs (including auto-dark), except the following:
;; Override some colors that the MATE theme sets
;; (set-face-attribute 'region nil :background "LightGoldenrod2") ;; equivalent to #eedc82
;; (set-face-attribute 'default nil
;;                     :font font-name
;;                     :height font-height
;;                     :background "#ffffff"
;;                     :foreground "#333333"
;;                     )
;; 2. reopen emacs
;; 3. M-x customize-create-theme
;; 4. Save the theme and give it some name; I've called it issa-test
;; 5. M-x customize-themes
;; 6. Pick the theme you just created. This will trigger a dialogue box that
;;    asks you whether to mark it as safe, so say "yes" to it. This gets saved as
;;    a hash in custom-safe-themes.
;; 7. Now you have the default emacs theme as a named theme that you can use
;;    with auto-dark!

;; auto-dark must be enabled after setting the auto-dark-themes
;; option; otherwise it starts to load random themes like leuven and
;; wombat that I don't want. See
;; https://github.com/LionyxML/auto-dark-emacs/issues/51
(setopt auto-dark-themes '((tango-dark) (issa-test)))
(require 'auto-dark)
(auto-dark-mode t)

;; Stuff to do on startup
(let ((inbox-file "/home/issa/projects/notes/inbox.txt"))
    (when (file-exists-p inbox-file)
        (find-file inbox-file)
        (end-of-buffer)
        (recenter-top-bottom)
        (roll)))
