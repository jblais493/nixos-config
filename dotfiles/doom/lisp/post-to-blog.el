;;; ../../dotfiles/doom/.config/doom/lisp/post-to-blog.el -*- lexical-binding: t; -*-

(defun my/get-org-property (property)
  "Get an org property from the current buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (format "^#\\+%s: \\(.*\\)$" property) nil t)
      (match-string 1))))

(defun my/get-org-filetags ()
  "Get filetags from current buffer as a list."
  (let ((tags-string (my/get-org-property "FILETAGS")))
    (when tags-string
      (split-string tags-string))))

(defun my/generate-astro-frontmatter ()
  "Generate Astro frontmatter from current org buffer."
  (let* ((title (or (my/get-org-property "TITLE")
                    (file-name-base (buffer-file-name))))
         (date (or (my/get-org-property "DATE")
                   (format-time-string "%Y-%m-%d")))
         (lastmod (or (my/get-org-property "LASTMOD") ""))
         (toc (or (my/get-org-property "TOC") "true"))
         (share (or (my/get-org-property "SHARE") "true"))
         (giscus (or (my/get-org-property "GISCUS") "true"))
         (ogimage (or (my/get-org-property "OGIMAGE") "true"))
         (tags (my/get-org-filetags)))
    (format "---\ntitle: %s\npubDate: %s\nlastModDate: '%s'\ntoc: %s\nshare: %s\ngiscus: %s\nogImage: %s\ntags: [%s]\n---\n\n"
            title date lastmod toc share giscus ogimage
            (if tags
                (mapconcat (lambda (tag) (format "\"%s\"" tag)) tags ", ")
              ""))))

(defun my/offset-markdown-headings (text)
  "Offset markdown headings by one level (# becomes ##, etc.)"
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (while (re-search-forward "^\\(#+\\) " nil t)
      (replace-match "#\\1 "))
    (buffer-string)))

(defun publish-post-to-astro ()
  "Select and publish an org file from the writing directory to Astro blog."
  (interactive)
  (let* ((writing-dir (expand-file-name "~/Projects/writing/Editing/"))
         (astro-posts-dir (expand-file-name "~/Development/joshuablais.com/src/content/blog/"))
         (org-files (directory-files writing-dir t "\\.org$"))
         (file-names (mapcar #'file-name-nondirectory org-files))
         (selected-name (completing-read "Select post to publish: " file-names nil t))
         (selected-file (car (seq-filter
                              (lambda (f) (string= (file-name-nondirectory f) selected-name))
                              org-files)))
         (file-base-name (file-name-sans-extension selected-name))
         (kebab-name (replace-regexp-in-string " " "-" (downcase file-base-name)))
         (target-file (concat astro-posts-dir kebab-name ".md")))
    (when (yes-or-no-p (format "Publish %s to %s?" selected-name target-file))
      (with-current-buffer (find-file-noselect selected-file)
        (let ((frontmatter (my/generate-astro-frontmatter))
              ;; Export to markdown and offset headings
              (content (my/offset-markdown-headings
                        (org-export-as 'md nil nil t))))
          (with-temp-file target-file
            (insert frontmatter)
            (insert content))))
      (message "Published %s to %s" selected-name target-file)
      (when (yes-or-no-p "Open the published markdown file?")
        (find-file target-file)))))
