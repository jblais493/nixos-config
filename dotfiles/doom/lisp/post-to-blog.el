;;; ../../dotfiles/doom/.config/doom/lisp/post-to-blog.el -*- lexical-binding: t; -*-

(defun publish-post-to-hugo ()
  "Select and publish an org file from the writing directory to Hugo blog."
  (interactive)
  (let* ((writing-dir (expand-file-name "~/org/roam/writing/To Post/"))
         (hugo-posts-dir (expand-file-name "~/Development/Blog2024/content/posts/"))
         ;; Get all org files in the writing directory
         (org-files (directory-files writing-dir t "\\.org$"))
         ;; Create a list of just the file names for selection
         (file-names (mapcar #'file-name-nondirectory org-files))
         ;; Have the user select a file
         (selected-name (completing-read "Select post to publish: " file-names nil t))
         ;; Find the full path of the selected file
         (selected-file (car (seq-filter
                              (lambda (f) (string= (file-name-nondirectory f) selected-name))
                              org-files)))
         ;; Prepare the destination path
         (file-base-name (file-name-sans-extension selected-name))
         (kebab-name (replace-regexp-in-string " " "-" (downcase file-base-name)))
         (post-dir (concat hugo-posts-dir kebab-name "/"))
         (target-file (concat post-dir "index.org")))

    ;; Confirm with the user
    (when (yes-or-no-p (format "Publish %s to %s?" selected-name post-dir))
      ;; Create the post directory if it doesn't exist
      (unless (file-exists-p post-dir)
        (make-directory post-dir t))

      ;; Copy the file to the target location
      (copy-file selected-file target-file t)

      (message "Published %s to %s" selected-name target-file)

      ;; Optionally open the target file
      (when (yes-or-no-p "Open the published file?")
        (find-file target-file)))))
