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
