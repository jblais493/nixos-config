;;; done-refile.el --- Refile items to done.org under today's date -*- lexical-binding: t; -*-

(defun my/move-to-done-org ()
  "Move the current org heading to done.org under today's date."
  (interactive)
  (let* ((done-file (expand-file-name "~/org/done.org"))
         (today-heading (format-time-string "* %Y-%m-%d %A")))

    ;; First, mark the task as DONE if it's not already
    (when (org-entry-is-todo-p)
      (org-todo 'done))

    ;; Add CLOSED property if it doesn't exist
    (unless (org-entry-get nil "CLOSED")
      (org-add-planning-info 'closed (org-current-effective-time)))

    ;; Ensure done.org exists and has today's date heading
    (with-current-buffer (find-file-noselect done-file)
      (goto-char (point-min))
      ;; Find or create today's heading
      (unless (re-search-forward (concat "^" (regexp-quote today-heading) "$") nil t)
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert today-heading "\n")
        (save-buffer)))

    ;; Use org-refile to move the subtree
    (let* ((rfloc (with-current-buffer (find-file-noselect done-file)
                    (goto-char (point-min))
                    (re-search-forward (concat "^" (regexp-quote today-heading) "$"))
                    (list today-heading
                          done-file
                          nil
                          (point)))))
      (org-refile nil nil rfloc))

    (message "Task moved to done.org under %s" today-heading)))

;; Bind to a convenient key
(global-set-key (kbd "C-c d") 'my/move-to-done-org)

(provide 'done-refile)
