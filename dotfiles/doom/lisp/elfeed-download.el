;;; elfeed-download.el --- Download articles and videos from elfeed

;;; Commentary:
;; This package provides functionality to download YouTube videos and save
;; articles as PDFs directly from elfeed entries.

;;; Code:

(require 'elfeed)

(defvar elfeed-download-base-dir "~/Downloads/Elfeed/"
  "Base directory for downloaded content.")

(defvar elfeed-download-youtube-dir "youtube/"
  "Subdirectory for YouTube videos (relative to base dir).")

(defvar elfeed-download-articles-dir "articles/"
  "Subdirectory for articles (relative to base dir).")

(defvar elfeed-download-node-script-path (expand-file-name "~/.config/scripts/save-article.js")
  "Path to the Node.js Playwright script for saving articles as PDFs.")

;;; Helper Functions
(defun elfeed-download--ensure-directory (dir)
  "Ensure directory DIR exists, creating it if necessary."
  (unless (file-exists-p dir)
    (make-directory dir t)))

(defun elfeed-download--sanitize-filename (filename)
  "Sanitize FILENAME for safe file system use."
  (replace-regexp-in-string "[^a-zA-Z0-9-_. ]" "_" filename))

(defun elfeed-download--get-youtube-dir ()
  "Get the full path to the YouTube download directory."
  (expand-file-name elfeed-download-youtube-dir elfeed-download-base-dir))

(defun elfeed-download--get-articles-dir ()
  "Get the full path to the articles download directory."
  (expand-file-name elfeed-download-articles-dir elfeed-download-base-dir))

;;; Download Functions
(defun elfeed-download-youtube (url title)
  "Download YouTube video from URL with TITLE."
  (let ((download-dir (elfeed-download--get-youtube-dir)))
    (elfeed-download--ensure-directory download-dir)
    (start-process "yt-dlp" "*yt-dlp*" "yt-dlp"
                   "-o" (concat download-dir "%(title)s.%(ext)s")
                   url)))

(defun elfeed-download-article (url title)
  "Save article from URL with TITLE as PDF using Playwright."
  (let* ((download-dir (elfeed-download--get-articles-dir))
         (safe-title (elfeed-download--sanitize-filename title))
         (pdf-file (concat download-dir safe-title ".pdf")))
    (elfeed-download--ensure-directory download-dir)
    (start-process "playwright-pdf" "*pdf-gen*" "node"
                   elfeed-download-node-script-path url pdf-file)))

;;; Main Functions
;;; Main Functions
(defun elfeed-download-current-entry ()
  "Download current elfeed entry, mark as read, and advance to next."
  (interactive)
  (let ((entries (elfeed-search-selected)))
    (when entries
      (let* ((entry (if (listp entries) (car entries) entries))
             (url (elfeed-entry-link entry))
             (title (elfeed-entry-title entry)))

        ;; Download based on URL type
        (cond
         ((string-match-p "youtube\\.com\\|youtu\\.be" url)
          (elfeed-download-youtube url title))
         (t
          (elfeed-download-article url title)))

        ;; Mark as read - remove unread tag and add read tag
        (elfeed-untag entry 'unread)
        (elfeed-tag entry 'read)
        (elfeed-search-update-entry entry)

        ;; Confirmation message
        (message "Downloaded and marked: %s" title)

        ;; Move to next entry
        (forward-line 1)))))

;;; Setup Function
(defun elfeed-download-setup ()
  "Set up elfeed-download keybindings and directories."
  (interactive)
  ;; Ensure base directories exist
  (elfeed-download--ensure-directory elfeed-download-base-dir)
  (elfeed-download--ensure-directory (elfeed-download--get-youtube-dir))
  (elfeed-download--ensure-directory (elfeed-download--get-articles-dir))

  (message "elfeed-download setup complete! Directories created, keybinding set to 'd'"))

;;; Utility Functions
(defun elfeed-download-open-download-dir ()
  "Open the download directory in file manager."
  (interactive)
  (find-file elfeed-download-base-dir))

(defun elfeed-download-check-dependencies ()
  "Check if required dependencies are installed."
  (interactive)
  (let ((missing-deps '()))
    ;; Check for yt-dlp
    (unless (executable-find "yt-dlp")
      (push "yt-dlp" missing-deps))

    ;; Check for node
    (unless (executable-find "node")
      (push "node" missing-deps))

    ;; Check for Playwright script
    (unless (file-exists-p elfeed-download-node-script-path)
      (push "save-article.js script" missing-deps))

    (if missing-deps
        (message "Missing dependencies: %s" (string-join missing-deps ", "))
      (message "All dependencies found!"))))

(provide 'elfeed-download)

;;; elfeed-download.el ends here
