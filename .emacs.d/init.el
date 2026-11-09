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

(setopt org-agenda-files '("~/todo.org" "~/tasks.org"))
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
 )

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

;; This block is written by Claude Fable 5.2.
(with-eval-after-load 'markdown-mode
  ;; 1. Don't treat footnote definitions ("[^label]: text") as reference
  ;;    link definitions ("[label]: url"): forbid a label starting with ^.
  (let ((old markdown-regex-reference-definition)
        (new "^ \\{0,3\\}\\(?1:\\[\\)\\(?2:[^]^\n][^]\n]*?\\)\\(?3:\\]\\)\\(?4::\\)\\s *\\(?5:.*?\\)\\s *\\(?6: \"[^\"]*\"$\\|$\\)"))
    (dolist (kw markdown-mode-font-lock-keywords)
      (when (equal (car-safe kw) old)
        (setcar kw new)))
    (setq markdown-regex-reference-definition new))

  ;; 2. Indented continuation lines of a footnote are not code blocks.
  (defun my/markdown-unpre-footnotes (start end)
    "Remove the `markdown-pre' property from footnote bodies in START..END."
    (save-excursion
      (save-match-data
        (let ((pos start))
          (while (and pos (< pos end))
            (if (not (get-text-property pos 'markdown-pre))
                (setq pos (next-single-property-change pos 'markdown-pre nil end))
              (let ((next (or (next-single-property-change pos 'markdown-pre nil end)
                              end)))
                (goto-char pos)
                (when (markdown-footnote-text-positions)
                  (remove-text-properties pos next '(markdown-pre nil)))
                (setq pos next))))))))
  (advice-add 'markdown-syntax-propertize-pre-blocks
              :after #'my/markdown-unpre-footnotes))


;; Stuff to do on startup
(let ((inbox-file "/home/issa/projects/notes/inbox.txt"))
    (when (file-exists-p inbox-file)
        (find-file inbox-file)
        (end-of-buffer)
        (recenter-top-bottom)
        (roll)
        (org-agenda-list)))
