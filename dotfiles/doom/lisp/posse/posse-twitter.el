;;; my-posse.el --- Multi-platform POSSE system -*- lexical-binding: t; -*-

;;; Commentary:
;; POSSE system that posts to both Twitter and Mastodon using authinfo.gpg credentials

;;; Code:

(require 'auth-source)

(defvar my-posse-script-path "~/.config/scripts/posse-social.py"
  "Path to the unified social media posting Python script.")

(defvar my-microblog-json-path "~/Development/joshuablais.com/src/content/notes/notes.json"
  "Path to the microblog JSON file.")

(defvar my-microblog-images-dir "~/Development/joshuablais.com/public/images/microblog/"
  "Directory to store microblog images.")

(defun my-post-tweet ()
  "Compose and post to both Twitter and Mastodon."
  (interactive)

  ;; Create composer buffer
  (let ((buf (get-buffer-create "*Tweet Composer*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "# Compose your post below (500 chars for Twitter compatibility):\n\n")
      (insert "This will be posted to both Twitter and Mastodon.")
      (org-mode)
      (goto-char (point-max))

      (setq-local media-path nil)

      ;; Custom keymap for this buffer
      (use-local-map (copy-keymap org-mode-map))
      (local-set-key (kbd "C-c C-c")
                     (lambda () (interactive) (my-send-tweet-from-buffer)))
      (local-set-key (kbd "C-c C-a")
                     (lambda () (interactive) (my-select-media-for-tweet)))
      (local-set-key (kbd "C-c C-k")
                     (lambda () (interactive)
                       (when (y-or-n-p "Cancel this post? ")
                         (kill-buffer)
                         (message "Post canceled.")))))

    (switch-to-buffer buf)
    (message "Compose your post. C-c C-c to send to all platforms, C-c C-a to attach media, C-c C-k to cancel.")))

(defun my-select-media-for-tweet ()
  "Select media to attach to the post."
  (interactive)
  (let ((file (expand-file-name (read-file-name "Select media file: " nil nil t))))
    (if (and file
             (file-exists-p file)
             (string-match-p "\\(?:png\\|jpg\\|jpeg\\|gif\\|mp4\\)$" file))
        (with-current-buffer "*Tweet Composer*"
          (setq-local media-path file)
          (message "Media selected: %s" (file-name-nondirectory file)))
      (message "Error: Selected file does not exist or is not a supported media type."))))

(defun my-get-auth-secret (host user)
  "Helper to get and clean auth secret from authinfo.gpg."
  (let* ((entry (car (auth-source-search :host host :user user :max 1)))
         (secret-fn (plist-get entry :secret)))
    (when secret-fn
      (string-trim (funcall secret-fn)))))

(defun my-get-twitter-credentials ()
  "Get Twitter credentials from auth-source."
  (let ((consumer-key (my-get-auth-secret "api.twitter.com" "TwitterAPI"))
        (consumer-secret (my-get-auth-secret "api.twitter.com.consumer" "TwitterAPI"))
        (access-token (my-get-auth-secret "api.twitter.com.token" "TwitterAPI"))
        (access-token-secret (my-get-auth-secret "api.twitter.com.secret" "TwitterAPI")))

    (unless (and consumer-key consumer-secret access-token access-token-secret)
      (error "Missing Twitter credentials. Check your ~/.authinfo.gpg file"))

    (list :consumer-key consumer-key
          :consumer-secret consumer-secret
          :access-token access-token
          :access-token-secret access-token-secret)))

(defun my-get-mastodon-credentials ()
  "Get Mastodon credentials from auth-source."
  (let ((instance (my-get-auth-secret "mastodon.instance" "MastodonAPI"))
        (access-token (my-get-auth-secret "mastodon.social" "MastodonAPI")))

    ;; DEBUG
    (message "DEBUG: Mastodon instance value: %s" instance)
    (message "DEBUG: Mastodon token length: %d" (length access-token))

    (unless (and instance access-token)
      (error "Missing Mastodon credentials. Check your ~/.authinfo.gpg file"))

    (list :instance instance
          :access-token access-token)))

(defun my-add-to-microblog (text media-path)
  "Add post to local microblog JSON file with proper JSON structure."
  (let* ((images-dir (expand-file-name my-microblog-images-dir))
         (json-file (expand-file-name my-microblog-json-path))
         (timestamp (format-time-string "%Y-%m-%dT%H:%M:%S"))
         (image-url "")
         (existing-data nil))

    ;; Create images directory if it doesn't exist
    (unless (file-directory-p images-dir)
      (make-directory images-dir t))

    ;; Handle image upload and linking
    (when (and media-path (file-exists-p media-path))
      (let* ((file-ext (file-name-extension media-path))
             (new-filename (format "%s.%s"
                                   (format-time-string "%Y%m%d-%H%M%S")
                                   file-ext))
             (dest-path (expand-file-name new-filename images-dir)))

        (copy-file media-path dest-path)
        (setq image-url (format "/images/microblog/%s" new-filename))
        (message "Image copied: %s -> %s"
                 (file-name-nondirectory media-path)
                 new-filename)))

    ;; Read existing JSON data
    (when (file-exists-p json-file)
      (with-temp-buffer
        (insert-file-contents json-file)
        (setq existing-data (json-parse-buffer :array-type 'list :object-type 'alist))))

    ;; Create new entry as alist (not plist!)
    (let ((new-entry `((text . ,(or text ""))
                       (image . ,image-url)
                       (timestamp . ,timestamp))))

      ;; Prepend new entry (newest first)
      (setq existing-data (cons new-entry existing-data))

      ;; Write updated JSON back to file
      (with-temp-file json-file
        (insert (json-encode existing-data)))

      (message "Microblog entry added: %s | %s | %s"
               timestamp
               (if (string-empty-p image-url) "no image" "with image")
               (if (string-empty-p text) "no text" (format "%.50s..." text))))))

(defun my-send-tweet-from-buffer ()
  "Send the post to Twitter, Mastodon, AND local microblog."
  (interactive)
  (let* ((content (buffer-substring-no-properties
                   (save-excursion
                     (goto-char (point-min))
                     (forward-line 2)
                     (point))
                   (point-max)))
         (tweet-text (string-trim content))
         (media (buffer-local-value 'media-path (current-buffer)))
         (twitter-creds (my-get-twitter-credentials))
         (mastodon-creds (my-get-mastodon-credentials)))

    (cond
     ((and (string-empty-p tweet-text) (not media))
      (message "Post must contain either text or media (or both)."))
     ((and (not (string-empty-p tweet-text)) (> (length tweet-text) 500))
      (message "Post exceeds 500 characters (%d). Please shorten it."
               (length tweet-text)))
     ((and media (not (file-exists-p media)))
      (message "Selected media file does not exist: %s" media))
     (t
      ;; Add to local microblog
      (my-add-to-microblog tweet-text media)

      ;; Prepare environment with credentials
      (let ((temp-file (make-temp-file "post-" nil ".txt"))
            (process-environment
             (append
              (list
               (format "TWITTER_CONSUMER_KEY=%s" (plist-get twitter-creds :consumer-key))
               (format "TWITTER_CONSUMER_SECRET=%s" (plist-get twitter-creds :consumer-secret))
               (format "TWITTER_ACCESS_TOKEN=%s" (plist-get twitter-creds :access-token))
               (format "TWITTER_ACCESS_TOKEN_SECRET=%s" (plist-get twitter-creds :access-token-secret))
               (format "MASTODON_INSTANCE=%s" (plist-get mastodon-creds :instance))
               (format "MASTODON_ACCESS_TOKEN=%s" (plist-get mastodon-creds :access-token)))
              process-environment)))

        (with-temp-file temp-file
          (insert tweet-text))

        (let* ((media-arg (when media (format " --media %s" (shell-quote-argument media))))
               (command (format "%s --text-file %s%s"
                                (shell-quote-argument (expand-file-name my-posse-script-path))
                                (shell-quote-argument temp-file)
                                (or media-arg ""))))

          (let ((result (shell-command-to-string command)))
            (delete-file temp-file)
            (if (string-match-p "Successfully posted" result)
                (progn
                  (message "Posted to microblog + all social platforms!")
                  (kill-buffer))
              (message "Microblog: ✓ | Social: %s" result)))))))))
(map! :leader
      (:prefix ("t" . "Tweet")
       :desc "Post to all platforms" "t" #'my-post-tweet))

(provide 'my-posse)
;;; my-posse.el ends here
