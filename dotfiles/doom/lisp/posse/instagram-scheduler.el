;;; instagram-scheduler.el --- Instagram scheduling via Telegram -*- lexical-binding: t; -*-

;;; Commentary:
;; Schedule Instagram posts, stories, and reels via Telegram bot reminders.
;; This allows for convenient scheduling while keeping the actual posting process manual.

;;; Code:

(require 'auth-source)
(require 'cl-lib)
(require 'json)
(require 'url)
(require 'parse-time)

(defgroup instagram-scheduler nil
  "Instagram scheduling via Telegram."
  :group 'applications
  :prefix "instagram-scheduler-")

(defvar instagram-scheduler-bot-token ""
  "The Telegram bot token for the Instagram scheduler bot.")

(defvar instagram-scheduler-chat-id ""
  "The Telegram chat ID to send notifications to.")

(defcustom instagram-scheduler-media-dir
  (expand-file-name "instagram-scheduled" user-emacs-directory)
  "Directory to store media files for scheduled posts."
  :type 'directory
  :group 'instagram-scheduler)

(defcustom instagram-scheduler-data-file
  (expand-file-name "instagram-scheduled-posts.el" user-emacs-directory)
  "File to store scheduled post data."
  :type 'file
  :group 'instagram-scheduler)

(defvar instagram-scheduler-posts nil
  "List of scheduled Instagram posts.")

(defvar instagram-scheduler-post-types
  '(("feed" . "Regular post to Instagram feed")
    ("carousel" . "Multiple-image carousel post")
    ("story" . "Instagram story")
    ("reels" . "Instagram reels video"))
  "Types of Instagram posts available for scheduling.")

;;; Utility Functions

(defun instagram-scheduler-load-credentials ()
  "Load Telegram bot credentials from auth-source."
  (let* ((bot-entry (car (auth-source-search :host "telegram" :max 1)))
         (chat-entry (car (auth-source-search :host "telegram" :max 1)))
         (bot-token-fn (plist-get bot-entry :secret))
         (chat-id-fn (plist-get chat-entry :secret))
         (bot-token (when bot-token-fn (funcall bot-token-fn)))
         (chat-id (when chat-id-fn (funcall chat-id-fn))))

    ;; For debugging
    (message "Bot token found: %s" (if bot-token "Yes" "No"))
    (message "Chat ID found: %s" (if chat-id "Yes" "No"))

    (when bot-token
      (setq instagram-scheduler-bot-token bot-token))
    (when chat-id
      (setq instagram-scheduler-chat-id chat-id))))

(defun instagram-scheduler-ensure-directories ()
  "Ensure necessary directories exist."
  (unless (file-directory-p instagram-scheduler-media-dir)
    (make-directory instagram-scheduler-media-dir t)))

(defun instagram-scheduler-load-posts ()
  "Load scheduled posts from disk."
  (when (file-exists-p instagram-scheduler-data-file)
    (with-temp-buffer
      (insert-file-contents instagram-scheduler-data-file)
      (goto-char (point-min))
      (condition-case nil
          (setq instagram-scheduler-posts (read (current-buffer)))
        (error
         (message "Error reading scheduled posts file.")
         (setq instagram-scheduler-posts nil))))))

(defun instagram-scheduler-save-posts ()
  "Save scheduled posts to disk."
  (with-temp-file instagram-scheduler-data-file
    (pp instagram-scheduler-posts (current-buffer))))

(defun instagram-scheduler-format-time-difference (time)
  "Format the time difference between TIME and now as a human-readable string."
  (let ((time-diff-secs (float-time (time-subtract time (current-time)))))
    (cond
     ((< time-diff-secs 0)
      "overdue")
     ((< time-diff-secs 60)
      "in less than a minute")
     ((< time-diff-secs 3600)
      (format "in %.0f minutes" (/ time-diff-secs 60)))
     ((< time-diff-secs 86400)
      (format "in %.1f hours" (/ time-diff-secs 3600)))
     (t
      (format "in %.1f days" (/ time-diff-secs 86400))))))

(defun instagram-scheduler-generate-id ()
  "Generate a unique ID for a post."
  (format "%d" (random 1000000)))

;;; Media Management Functions

(defun instagram-scheduler-select-media ()
  "Select media files for an Instagram post."
  (interactive)
  (let ((file (read-file-name "Select media file: " nil nil t)))
    (when (and file (file-exists-p file)
               (string-match-p "\\(?:png\\|jpg\\|jpeg\\|gif\\|mp4\\|mov\\)$" file))
      file)))

(defun instagram-scheduler-select-multiple-media ()
  "Select multiple media files for an Instagram carousel post."
  (interactive)
  (let ((files nil)
        (continue t)
        (count 0))
    (while (and continue (< count 10)) ; Instagram allows up to 10 images in a carousel
      (let ((file (instagram-scheduler-select-media)))
        (if file
            (progn
              (push file files)
              (setq count (1+ count))
              (setq continue (y-or-n-p (format "Add another media file? (%d selected)" count))))
          (setq continue nil))))
    (nreverse files)))

(defun instagram-scheduler-save-media-files (media-files)
  "Save MEDIA-FILES to the storage directory and return the new paths."
  (let ((saved-paths nil))
    (dolist (media-path media-files)
      (let* ((media-filename (file-name-nondirectory media-path))
             (media-id (format "%d-%s" (random 1000000) media-filename))
             (media-dest (expand-file-name media-id instagram-scheduler-media-dir)))
        (copy-file media-path media-dest t)
        (push media-dest saved-paths)))
    (nreverse saved-paths)))

;;; Post Creation Functions

(defun instagram-scheduler-create-post ()
  "Create a new scheduled Instagram post."
  (interactive)

  ;; Load credentials
  (instagram-scheduler-load-credentials)
  (instagram-scheduler-ensure-directories)

  ;; Ensure we have bot credentials
  (unless (and (not (string-empty-p instagram-scheduler-bot-token))
               (not (string-empty-p instagram-scheduler-chat-id)))
    (message "Telegram bot credentials not set. Configure them first.")
    (cl-return-from instagram-scheduler-create-post nil))

  ;; Get post type
  (let* ((post-type-options (mapcar 'cdr instagram-scheduler-post-types))
         (post-type-values (mapcar 'car instagram-scheduler-post-types))
         (selected-type-name (completing-read "Post type: " post-type-options nil t))
         (selected-type (car (rassoc selected-type-name instagram-scheduler-post-types)))
         (buf (get-buffer-create "*Instagram Scheduler*")))

    (with-current-buffer buf
      (erase-buffer)
      (org-mode)

      ;; Set up the buffer content based on post type
      (insert "# Schedule Instagram Post\n\n")
      (insert "## Post Type\n\n")
      (insert selected-type-name)
      (insert "\n\n")
      (insert "## Scheduled Time\n\n")
      (insert (format-time-string "%Y-%m-%d %H:%M"
                                  (time-add (current-time) (seconds-to-time 3600))))
      (insert "\n\n")

      ;; Different sections based on post type
      (cond
       ;; Story has caption + text overlay
       ((string= selected-type "story")
        (insert "## Caption (for Story Details)\n\n")
        (insert "Enter caption for the story details section...\n\n")
        (insert "## Text Overlay (appears on the story)\n\n")
        (insert "Enter text to overlay on the story...\n\n"))

       ;; Reels has caption + audio info
       ((string= selected-type "reels")
        (insert "## Caption\n\n")
        (insert "Enter your caption here...\n\n")
        (insert "## Audio\n\n")
        (insert "Enter audio/music information (optional)...\n\n"))

       ;; Feed/Carousel has caption
       (t
        (insert "## Caption\n\n")
        (insert "Enter your caption here...\n\n")))

      ;; Common hashtags section
      (insert "## Hashtags\n\n")
      (insert "#yourtag #anothertag\n\n")

      ;; Buffer-local variables
      (setq-local post-type selected-type)
      (setq-local media-files nil)
      (setq-local post-id (instagram-scheduler-generate-id))

      ;; Custom keymap
      (use-local-map (copy-keymap org-mode-map))
      (local-set-key (kbd "C-c C-c") #'instagram-scheduler-finalize-post)
      (local-set-key (kbd "C-c C-k") #'instagram-scheduler-cancel-post)

      ;; Media selection keybinding depends on post type
      (if (string= selected-type "carousel")
          (local-set-key (kbd "C-c C-a") #'instagram-scheduler-attach-multiple)
        (local-set-key (kbd "C-c C-a") #'instagram-scheduler-attach-single)))

    ;; Switch to the buffer and position cursor
    (switch-to-buffer buf)
    (goto-char (point-min))
    (search-forward "Enter" nil t)
    (beginning-of-line)
    (kill-line)

    ;; Display help message
    (let ((help-text (cond
                      ((string= selected-type "carousel")
                       "C-c C-a to attach multiple images, C-c C-c to schedule, C-c C-k to cancel")
                      ((string= selected-type "story")
                       "C-c C-a to attach an image/video, C-c C-c to schedule, C-c C-k to cancel")
                      ((string= selected-type "reels")
                       "C-c C-a to attach a video, C-c C-c to schedule, C-c C-k to cancel")
                      (t
                       "C-c C-a to attach media, C-c C-c to schedule, C-c C-k to cancel"))))
      (message "Create your Instagram %s post. %s" selected-type help-text))))

(defun instagram-scheduler-attach-single ()
  "Attach a single media file to the post."
  (interactive)
  (let ((file (instagram-scheduler-select-media)))
    (when file
      (with-current-buffer "*Instagram Scheduler*"
        (setq-local media-files (list file))
        (message "Media attached: %s" (file-name-nondirectory file))))))

(defun instagram-scheduler-attach-multiple ()
  "Attach multiple media files to the post."
  (interactive)
  (let ((files (instagram-scheduler-select-multiple-media)))
    (when files
      (with-current-buffer "*Instagram Scheduler*"
        (setq-local media-files files)
        (message "Attached %d media files." (length files))))))

(defun instagram-scheduler-cancel-post ()
  "Cancel the current Instagram post."
  (interactive)
  (when (y-or-n-p "Cancel this Instagram post? ")
    (kill-buffer)
    (message "Instagram post canceled.")))

(defun instagram-scheduler-extract-sections ()
  "Extract sections from the Instagram scheduler buffer."
  (with-current-buffer "*Instagram Scheduler*"
    (let* ((content (buffer-substring-no-properties (point-min) (point-max)))
           (result (list))
           ;; Extract standard sections
           (time (when (string-match "## Scheduled Time\n\n\\(\\(?:.\\|\n\\)*?\\)\n\n" content)
                   (match-string 1 content)))
           (hashtags (when (string-match "## Hashtags\n\n\\(\\(?:.\\|\n\\)*?\\)\\(?:\n\n\\|$\\)" content)
                       (match-string 1 content)))
           ;; Post-type dependent sections
           (post-type (buffer-local-value 'post-type (current-buffer))))

      ;; Add common fields
      (push (cons 'time time) result)
      (push (cons 'hashtags hashtags) result)

      ;; Extract fields specific to post type
      (cond
       ;; Story has caption + text overlay
       ((string= post-type "story")
        (when (string-match "## Caption[^\n]*\n\n\\(\\(?:.\\|\n\\)*?\\)\n\n" content)
          (push (cons 'caption (match-string 1 content)) result))
        (when (string-match "## Text Overlay[^\n]*\n\n\\(\\(?:.\\|\n\\)*?\\)\n\n" content)
          (push (cons 'overlay (match-string 1 content)) result)))

       ;; Reels has caption + audio
       ((string= post-type "reels")
        (when (string-match "## Caption\n\n\\(\\(?:.\\|\n\\)*?\\)\n\n" content)
          (push (cons 'caption (match-string 1 content)) result))
        (when (string-match "## Audio\n\n\\(\\(?:.\\|\n\\)*?\\)\n\n" content)
          (push (cons 'audio (match-string 1 content)) result)))

       ;; Feed/Carousel has caption
       (t
        (when (string-match "## Caption\n\n\\(\\(?:.\\|\n\\)*?\\)\n\n" content)
          (push (cons 'caption (match-string 1 content)) result))))

      ;; Return the extracted data
      result)))

(defun instagram-scheduler-finalize-post ()
  "Finalize and schedule the Instagram post."
  (interactive)
  (with-current-buffer "*Instagram Scheduler*"
    (let* ((post-type (buffer-local-value 'post-type (current-buffer)))
           (media-files (buffer-local-value 'media-files (current-buffer)))
           (post-id (buffer-local-value 'post-id (current-buffer)))
           (sections (instagram-scheduler-extract-sections))
           (schedule-time-str (cdr (assoc 'time sections)))
           (caption (or (cdr (assoc 'caption sections)) ""))
           (hashtags (or (cdr (assoc 'hashtags sections)) ""))
           ;; Story/reels specific fields
           (overlay (cdr (assoc 'overlay sections)))
           (audio (cdr (assoc 'audio sections))))

      ;; Validate the post
      (cond
       ;; Check schedule time
       ((or (not schedule-time-str) (string-empty-p schedule-time-str))
        (message "Schedule time is required."))

       ;; Check media requirements
       ((not media-files)
        (message "Media is required for all Instagram post types."))

       ;; Type-specific validations
       ((and (string= post-type "carousel") (< (length media-files) 2))
        (message "Carousel posts require at least 2 images."))

       ((and (string= post-type "reels")
             (not (cl-some (lambda (file) (string-match-p "\\.\\(mp4\\|mov\\)$" file)) media-files)))
        (message "Reels require a video file (mp4 or mov)."))

       ;; All validations passed, create the post
       (t
        ;; Parse schedule time
        (condition-case err
            (let* ((parsed-time (parse-time-string schedule-time-str))
                   (schedule-time (apply #'encode-time parsed-time))
                   ;; Combine caption and hashtags
                   (full-caption (if (string-empty-p hashtags)
                                     caption
                                   (concat caption "\n\n" hashtags))))

              ;; Save media files to storage location
              (instagram-scheduler-ensure-directories)
              (let ((saved-media-paths (instagram-scheduler-save-media-files media-files)))

                ;; Create the post data structure
                (let ((post-data `((id . ,post-id)
                                   (type . ,post-type)
                                   (time . ,schedule-time)
                                   (caption . ,full-caption)
                                   (media . ,saved-media-paths))))

                  ;; Add type-specific fields
                  (when (and (string= post-type "story") overlay)
                    (push (cons 'overlay overlay) post-data))

                  (when (and (string= post-type "reels") audio)
                    (push (cons 'audio audio) post-data))

                  ;; Add to scheduled posts
                  (instagram-scheduler-load-posts)
                  (push post-data instagram-scheduler-posts)
                  (instagram-scheduler-save-posts)

                  ;; Schedule the notification
                  (instagram-scheduler-schedule-notification post-data)

                  ;; Show confirmation and close buffer
                  (message "Instagram %s scheduled for %s (%s)"
                           post-type
                           schedule-time-str
                           (instagram-scheduler-format-time-difference schedule-time))
                  (kill-buffer))))

          ;; Handle time parsing errors
          (error
           (message "Invalid schedule time format: %s" (error-message-string err)))))))))

;;; Notification and Scheduling Functions

(defun instagram-scheduler-schedule-notification (post-data)
  "Schedule a notification for POST-DATA."
  (let ((post-time (cdr (assoc 'time post-data))))
    (run-at-time post-time nil #'instagram-scheduler-send-notification post-data)))

(defun instagram-scheduler-send-notification (post-data)
  "Send a notification for POST-DATA via Telegram."
  (let* ((post-id (cdr (assoc 'id post-data)))
         (post-type (cdr (assoc 'type post-data)))
         (caption (cdr (assoc 'caption post-data)))
         (media-paths (cdr (assoc 'media post-data)))
         (overlay (cdr (assoc 'overlay post-data)))
         (audio (cdr (assoc 'audio post-data)))

         ;; Format the notification message based on post type
         (message-text
          (cond
           ;; Story format
           ((string= post-type "story")
            (format "🚨 *Instagram Story Reminder*\n\n*Caption for Story Details:*\n%s\n\n%s\n\n*Instructions:*\n1. Save the media\n2. Open Instagram\n3. Swipe right to create a Story\n4. Upload the saved media\n5. Add the overlay text\n6. Post the Story!"
                    caption
                    (if overlay (format "*Text Overlay:*\n%s" overlay) "")))

           ;; Reels format
           ((string= post-type "reels")
            (format "🚨 *Instagram Reels Reminder*\n\n*Caption:*\n%s\n\n%s\n\n*Instructions:*\n1. Save the video\n2. Open Instagram\n3. Tap + and select Reels\n4. Upload the saved video\n5. Add effects/music as needed\n6. Add the caption\n7. Post the Reel!"
                    caption
                    (if audio (format "*Audio:*\n%s" audio) "")))

           ;; Carousel format
           ((string= post-type "carousel")
            (format "🚨 *Instagram Carousel Reminder*\n\n*Caption:*\n%s\n\n*Instructions:*\n1. Save all %d images\n2. Open Instagram\n3. Tap + to create a new post\n4. Select all saved images in order\n5. Paste the caption\n6. Post the carousel!"
                    caption
                    (length media-paths)))

           ;; Standard post format
           (t
            (format "🚨 *Instagram Post Reminder*\n\n*Caption:*\n%s\n\n*Instructions:*\n1. Save the media\n2. Open Instagram\n3. Tap + to create a new post\n4. Select the saved media\n5. Paste the caption\n6. Post to your feed!"
                    caption)))))

    ;; Send the initial message
    (instagram-scheduler-telegram-send-message instagram-scheduler-chat-id message-text)

    ;; Send each media file
    (dolist (media-path media-paths)
      (instagram-scheduler-telegram-send-media instagram-scheduler-chat-id media-path))

    ;; Send just the caption separately for easy copying
    (instagram-scheduler-telegram-send-message instagram-scheduler-chat-id caption)

    ;; If it's a story with overlay text, send that separately too
    (when (and (string= post-type "story") overlay (not (string-empty-p overlay)))
      (instagram-scheduler-telegram-send-message instagram-scheduler-chat-id overlay))

    ;; Remove this post from the scheduled list
    (setq instagram-scheduler-posts
          (cl-remove-if (lambda (p) (string= (cdr (assoc 'id p)) post-id))
                        instagram-scheduler-posts))

    ;; Save updated list
    (instagram-scheduler-save-posts)))

(defun instagram-scheduler-telegram-send-message (chat-id text)
  "Send TEXT to CHAT-ID using the Telegram bot API."
  (let ((url-request-method "POST")
        (url-request-extra-headers '(("Content-Type" . "application/json")))
        (url (format "https://api.telegram.org/bot%s/sendMessage"
                     instagram-scheduler-bot-token))
        (url-request-data (json-encode `(("chat_id" . ,chat-id)
                                         ("text" . ,text)
                                         ("parse_mode" . "Markdown")))))
    (url-retrieve url (lambda (_) nil))))

(defun instagram-scheduler-telegram-send-media (chat-id media-path)
  "Send media at MEDIA-PATH to CHAT-ID using the Telegram bot API."
  (let* ((file-ext (downcase (file-name-extension media-path)))
         (is-video (member file-ext '("mp4" "mov")))
         (endpoint (if is-video "sendVideo" "sendPhoto"))
         (boundary (format "-------------------------%d" (random 1000000000)))
         (url (format "https://api.telegram.org/bot%s/%s"
                      instagram-scheduler-bot-token endpoint))

         ;; Prepare multipart form data
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . ,(format "multipart/form-data; boundary=%s" boundary))))

         ;; Read file content
         (file-data (with-temp-buffer
                      (condition-case nil
                          (insert-file-contents-literally media-path)
                        (error
                         (insert-file-contents media-path)))
                      (buffer-string)))

         ;; Build multipart form body
         (url-request-data
          (with-temp-buffer
            ;; Add chat_id field
            (insert (format "--%s\r\n" boundary))
            (insert "Content-Disposition: form-data; name=\"chat_id\"\r\n\r\n")
            (insert chat-id)
            (insert "\r\n")

            ;; Add media file
            (insert (format "--%s\r\n" boundary))
            (insert (format "Content-Disposition: form-data; name=\"%s\"; filename=\"%s\"\r\n"
                            (if is-video "video" "photo")
                            (file-name-nondirectory media-path)))
            (insert (format "Content-Type: %s\r\n\r\n"
                            (if is-video "video/mp4" "image/jpeg")))
            (insert file-data)
            (insert "\r\n")

            ;; End boundary
            (insert (format "--%s--\r\n" boundary))
            (buffer-string))))

    ;; Send request
    (url-retrieve url (lambda (_) nil))))

;;; Post Management Functions

(defun instagram-scheduler-view-posts ()
  "View all scheduled Instagram posts."
  (interactive)
  (instagram-scheduler-load-posts)
  (let ((buf (get-buffer-create "*Instagram Schedule*")))
    (with-current-buffer buf
      (erase-buffer)
      (org-mode)
      (insert "# Scheduled Instagram Posts\n\n")

      (if (null instagram-scheduler-posts)
          (insert "No posts currently scheduled.\n")
        (cl-loop for post in (sort (copy-sequence instagram-scheduler-posts)
                                   (lambda (a b)
                                     (time-less-p (cdr (assq 'time a))
                                                  (cdr (assq 'time b)))))
                 do (let* ((post-id (cdr (assq 'id post)))
                           (post-type (cdr (assq 'type post)))
                           (time (cdr (assq 'time post)))
                           (time-str (format-time-string "%Y-%m-%d %H:%M" time))
                           (caption (cdr (assq 'caption post)))
                           (media-paths (cdr (assq 'media post)))
                           (time-diff (instagram-scheduler-format-time-difference time)))

                      (insert (format "## %s Post: %s (%s)\n\n"
                                      (capitalize post-type) time-str time-diff))
                      (insert (format "- **Media:** %d file(s)\n" (length media-paths)))
                      (insert (format "- **Caption sample:** %s\n\n"
                                      (truncate-string-to-width caption 50 nil nil "...")))

                      ;; Add buttons for actions
                      (insert "[[elisp:(instagram-scheduler-delete-post \"" post-id "\")][Delete]] | ")
                      (insert "[[elisp:(instagram-scheduler-view-post-details \"" post-id "\")][View Details]] | ")
                      (insert "[[elisp:(instagram-scheduler-send-now \"" post-id "\")][Send Now]]\n\n")))))

    (switch-to-buffer buf)
    (goto-char (point-min))
    (message "View and manage scheduled Instagram posts.")))

(defun instagram-scheduler-delete-post (post-id)
  "Delete the scheduled post with POST-ID."
  (interactive "sPost ID to delete: ")
  (when (yes-or-no-p (format "Delete scheduled Instagram post %s? " post-id))
    (instagram-scheduler-load-posts)
    (setq instagram-scheduler-posts
          (cl-remove-if (lambda (p) (string= (cdr (assq 'id p)) post-id))
                        instagram-scheduler-posts))
    (instagram-scheduler-save-posts)
    (message "Post %s deleted." post-id)
    (instagram-scheduler-view-posts)))

(defun instagram-scheduler-view-post-details (post-id)
  "View detailed information about the post with POST-ID."
  (interactive "sPost ID to view: ")
  (instagram-scheduler-load-posts)
  (let ((post (cl-find-if (lambda (p) (string= (cdr (assq 'id p)) post-id))
                          instagram-scheduler-posts)))
    (if (not post)
        (message "Post %s not found." post-id)
      (let ((buf (get-buffer-create "*Instagram Post Details*")))
        (with-current-buffer buf
          (erase-buffer)
          (org-mode)

          (let ((post-type (cdr (assq 'type post)))
                (time (cdr (assq 'time post)))
                (time-str (format-time-string "%Y-%m-%d %H:%M" (cdr (assq 'time post))))
                (caption (cdr (assq 'caption post)))
                (media-paths (cdr (assq 'media post)))
                (overlay (cdr (assq 'overlay post)))
                (audio (cdr (assq 'audio post))))

            (insert (format "# Instagram %s Post Details\n\n" (capitalize post-type)))
            (insert (format "**Scheduled Time:** %s (%s)\n\n"
                            time-str
                            (instagram-scheduler-format-time-difference time)))

            (insert "## Caption\n\n")
            (insert (format "%s\n\n" caption))

            (when overlay
              (insert "## Text Overlay\n\n")
              (insert (format "%s\n\n" overlay)))

            (when audio
              (insert "## Audio Information\n\n")
              (insert (format "%s\n\n" audio)))

            (insert "## Media Files\n\n")
            (dolist (path media-paths)
              (insert (format "- %s\n" path)))

            (insert "\n")
            (insert "[[elisp:(instagram-scheduler-delete-post \"" post-id "\")][Delete Post]] | ")
            (insert "[[elisp:(instagram-scheduler-send-now \"" post-id "\")][Send Now]] | ")
            (insert "[[elisp:(instagram-scheduler-view-posts)][Back to List]]\n")))

        (switch-to-buffer buf)
        (goto-char (point-min))))))

(defun instagram-scheduler-send-now (post-id)
  "Send the scheduled post with POST-ID immediately."
  (interactive "sPost ID to send now: ")
  (instagram-scheduler-load-posts)
  (let ((post (cl-find-if (lambda (p) (string= (cdr (assq 'id p)) post-id))
                          instagram-scheduler-posts)))
    (if (not post)
        (message "Post %s not found." post-id)
      (when (yes-or-n-p (format "Send Instagram %s post now? " (cdr (assq 'type post))))
        (instagram-scheduler-send-notification post)
        (message "Instagram post %s sent." post-id)
        (instagram-scheduler-view-posts)))))

;;; Initialization

(defun instagram-scheduler-initialize ()
  "Initialize the Instagram scheduler system."
  (instagram-scheduler-load-credentials)
  (instagram-scheduler-ensure-directories)
  (instagram-scheduler-load-posts)

  ;; Schedule all future posts
  (dolist (post instagram-scheduler-posts)
    (let* ((post-time (cdr (assq 'time post)))
           (now (current-time))
           (time-diff (float-time (time-subtract post-time now))))
      (when (> time-diff 0)  ; Only schedule posts in the future
        (run-at-time post-time nil #'instagram-scheduler-send-notification post)))))

;; Run initialization when the module loads
(instagram-scheduler-initialize)

;; Keybindings
(map! :leader
      (:prefix ("i" . "Instagram")
       :desc "Schedule post" "p" #'instagram-scheduler-create-post
       :desc "View scheduled posts" "v" #'instagram-scheduler-view-posts))

(provide 'instagram-scheduler)
;;; instagram-scheduler.el ends here
