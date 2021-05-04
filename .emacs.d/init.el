;; No startup screen
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; Set default window size
(setq initial-frame-alist
          '((width . 81) (height . 39)))

;; For installing packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Set variables like PATH so that eshell works more like a normal shell
(when (and
        (require 'exec-path-from-shell nil 'noerror)
        (not (eq system-type 'windows-nt)))
  (exec-path-from-shell-initialize))

;; For Japanese input. I like to install this from the Ubuntu
;; repository rather than melpa. The package is called emacs-mozc.
(require 'mozc nil 'noerror)

;; Use IPAexGothic for Japanese text. From
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Fontsets.html
(when (fboundp 'set-fontset-font)
  (set-fontset-font t 'japanese-jisx0208 (font-spec :family "IPAexGothic")))

;; hunspell provides better spelling suggestions in my opinion.
;; To get hunspell on Windows, first install
;; msys2 (https://www.msys2.org/), then follow the instructions
;; in this answer https://emacs.stackexchange.com/a/45752/31094
;; to get hunspell installed via msys2 (the instructions
;; are for aspell but it's very similar). Once hunspell is
;; installed, the configuration below should automatically work.
(if (eq system-type 'windows-nt)
    (progn
      (setq ispell-program-name "C:/msys64/mingw64/bin/hunspell.exe")
      (setenv "DICTIONARY" "en_US"))
  (when (file-exists-p "/usr/bin/hunspell")
    (setq ispell-program-name "/usr/bin/hunspell")))

;; Turn on flyspell in most files
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(setq font-name
      (if (eq system-type 'windows-nt)
          "Consolas"
        "Ubuntu Mono"))
(setq font-height
      (if (eq system-type 'windows-nt)
          110
        140))

;; Override some colors that the MATE theme sets
(set-face-attribute 'region nil :background "LightGoldenrod2") ;; equivalent to #eedc82
(set-face-attribute 'default nil
                    :font font-name
                    :height font-height
                    :background "white"  ;; equivalent to #ffffff
                    :foreground "gray20" ;; equivalent to #333333
                    )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(indent-tabs-mode nil)
 '(magit-diff-refine-hunk (quote all))
 '(make-backup-files nil)
 '(markdown-enable-math t)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (3 ((shift) . 1) ((control)))))
 '(org-agenda-files (quote ("~/todo.org")))
 '(org-capture-templates
   (quote
    (("a" "Anki note" entry
      (file "~/org/anki.org")
      "* %?")
     ("i" "Idea" entry
      (file "~/org/notes.org")
      "* %T %?")
     ("t" "TODO item" entry
      (file+headline "~/todo.org" "Tasks")
      "* TODO %?
  %i
"))))
 '(org-clock-mode-line-total (quote today))
 '(org-duration-format (quote h:mm))
 '(org-format-latex-options
   (quote
    (:foreground default :background default :scale 1.3 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\["))))
 '(org-startup-truncated nil)
 '(org-todo-keywords
   (quote
    ((sequence "TODO(t)" "WAITING(w)" "SOMEDAY(s)" "DONE(d)"))))
 '(package-selected-packages
   (quote
    (exec-path-from-shell auctex php-mode lua-mode markdown-mode jedi intero magit)))
 '(preview-scale-function 1.2)
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

(define-key 'iso-transl-ctl-x-8-map "el" [?…])

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

;; from https://tex.stackexchange.com/a/392038/18026
(add-hook 'LaTeX-mode-hook
          (lambda () (set (make-variable-buffer-local 'TeX-electric-math)
                          (cons "\\(" "\\)"))))

(ido-mode t)

(when (fboundp 'magit-status)
  (global-set-key (kbd "C-x g") 'magit-status))

(setq c-default-style "linux")
(add-hook 'c-mode-hook '(lambda ()
                         (setq indent-tabs-mode t)
                         (setq tab-width 8)))

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)


;; For pushing with git on windows
(when (eq system-type 'windows-nt)
    (setenv "SSH_ASKPASS" "git-gui--askpass"))


(require 'server)
(unless (server-running-p)
  (server-start))
