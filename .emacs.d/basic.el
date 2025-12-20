;; This file is supposed to be for basic editing-related settings
;; that I would like whenever using Emacs on any machine.

(setopt inhibit-startup-screen t)
(setopt menu-bar-mode nil)
(setopt tool-bar-mode nil)

(setopt mouse-wheel-progressive-speed nil)
(setopt mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control))))
;; (setopt scroll-conservatively 1000)

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
