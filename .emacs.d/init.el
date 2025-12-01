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

(setopt inhibit-startup-screen t)
(setopt menu-bar-mode nil)
(setopt tool-bar-mode nil)

(setopt mouse-wheel-progressive-speed nil)
(setopt mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control))))
(setopt scroll-conservatively 1000)

(setopt column-number-mode t)
(setopt global-word-wrap-whitespace-mode t)
(setopt indent-tabs-mode nil)
(setopt make-backup-files nil)
(setopt markdown-enable-math t)
(setopt require-final-newline t)
(setopt ring-bell-function 'ignore)
(setopt save-interprogram-paste-before-kill t)
(setopt sentence-end-double-space nil)
(setopt show-paren-mode t)
(setopt show-trailing-whitespace t)
(setopt vc-follow-symlinks t)

;; I forgot why I needed to put these lines. Probably because Windows
;; is stupid and doesn't use UTF-8 by default.
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; This does not seem to actually obey what's in ~/.editorconfig a lot
;; of the time. I'm not sure why.
(editorconfig-mode 1)

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
(if (eq system-type 'windows-nt)
    (progn
      (setopt ispell-program-name "C:/msys64/mingw64/bin/hunspell.exe")
      (setenv "DICTIONARY" "en_US"))
  (when (file-exists-p "/usr/bin/hunspell")
    (setopt ispell-program-name "/usr/bin/hunspell")))

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

;; Make C-w work as in Bash.
;; This still doesn't work exactly like in bash, since bash seems to
;; care about all whitespace characters, whereas this only cares about
;; the space character.
;; To get the old behavior of Emacs's C-w, you can use C-x C-x C-w
;; (i.e., activate the region between point and the last mark, then
;; kill it).
(global-set-key (kbd "C-w")
                '(lambda () (interactive)
                   (if (region-active-p)
                       (kill-region (region-beginning) (region-end))
                     (my-smart-zap-back-to-whitespace))))

(defun my-smart-zap-back-to-whitespace ()
  "Kill back to, but not including the previous whitespace,
in a smart sort of way like C-w in bash."
  (kill-region (point)
               ;; so this next part needs to find the whitespace location
               (progn
                 (let ((original-line-beginning (line-beginning-position)))
                   (if (equal (string (preceding-char)) " ")
                       ;; the previous char is space
                       (progn
                         (re-search-backward "[^ ]" nil t 1)
                         (search-backward " ")
                         (forward-char)
                         (goto-char (max (point) original-line-beginning)))
                     ;; the previous char is non-space, so just
                     ;; kill back to previous space
                     (progn
                       (search-backward " ")
                       (forward-char)
                       (goto-char (max (point) original-line-beginning))))))))


;; spaced inbox stuff

(defun daily-note-separator ()
  "Insert the daily note separator for spaced inbox."
  (interactive)
  (shell-command-to-string "python3 /home/issa/projects/spaced-inbox/spaced_inbox.py")  ;; I don't need the output but I also don't want to have a buffer pop up showing me the output (which is what would happen if I used shell-command instead), so I store it to a string and just ignore it.
  (insert "=====\n")
  (insert (format-time-string "%Y-%m-%d"))
  (insert "\n\n\n")
  (previous-line 4)
  (recenter-top-bottom 0)
  (next-line 4))

(defun spaced-inbox--navigate-from-string (input-string)
  (if (string-match "^\\(.*\\):\\([0-9]+\\):[0-9]+\\(?::.*\\)?$" input-string)
      (let ((filename (string-trim (match-string 1 input-string)))
            (line-number (string-to-number (match-string 2 input-string))))
        (message "Navigating to %s:%d..." filename line-number)
        (with-current-buffer (window-buffer (selected-window))
          (find-file filename)
          (goto-line line-number)
          (recenter-top-bottom 0))
        t)
    (progn
      (message "Was not able to navigate to %s line %d" filename line-number)
      nil)))

(defun roll ()
  (interactive)
  (let* ((spaced-inbox-executable "python3 /home/issa/projects/spaced-inbox/spaced_inbox.py")
         (flags "-r")
         (spaced-inbox-command (concat spaced-inbox-executable " " flags)))
    (progn
      (let* ((output (string-trim (shell-command-to-string spaced-inbox-command))))
        (if (string-empty-p output)
            (message "Spaced inbox script produced no output.")
          (spaced-inbox--navigate-from-string output))))))

(defun today ()
  (interactive)
  (insert (concat (format-time-string "%Y-%m-%d") ": ")))

(global-set-key (kbd "C-c r") 'roll)
(global-set-key (kbd "C-c t") 'today)
;;(global-set-key (kbd "C-c d") 'daily-note-separator)


(setopt magit-diff-refine-hunk 'all)
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
;;                     :background "white"  ;; equivalent to #ffffff
;;                     :foreground "gray20" ;; equivalent to #333333
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
;; (setopt auto-dark-dark-theme 'tango-dark)
;; (setopt auto-dark-light-theme 'issa-test)
(require 'auto-dark)
(auto-dark-mode t)

;; Stuff to do on startup
(let ((inbox-file "/home/issa/projects/notes/inbox.txt"))
    (when (file-exists-p inbox-file)
        (find-file inbox-file)
        (end-of-buffer)
        (recenter-top-bottom)
        (roll)))
