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
