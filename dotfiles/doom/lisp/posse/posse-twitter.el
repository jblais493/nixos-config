;;; my-posse.el --- Multi-platform POSSE system -*- lexical-binding: t; -*-

;;; Commentary:
;; POSSE system that posts to both Twitter and Mastodon using authinfo.gpg credentials

;;; Code:

(require 'auth-source)

(defvar my-posse-script-path "~/.config/scripts/posse-social.py"
  "Path to the unified social media posting Python script.")

(defun my-post-tweet ()
  "Compose and post to both Twitter and Mastodon."
  (interactive)

  ;; Check if the script directory exists, create if needed
  (let ((script-dir (file-name-directory my-posse-script-path)))
    (unless (file-directory-p script-dir)
      (make-directory script-dir t)))

  ;; Create composer buffer
  (let ((buf (get-buffer-create "*Tweet Composer*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "# Compose your post below (280 chars for Twitter compatibility):\n\n")
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

    (unless (and instance access-token)
      (error "Missing Mastodon credentials. Check your ~/.authinfo.gpg file"))

    (list :instance instance
          :access-token access-token)))

(defun my-generate-script-from-template ()
  "Generate the Python script with aggressive Python 3.13 tweepy fix."
  (let* ((twitter-creds (my-get-twitter-credentials))
         (mastodon-creds (my-get-mastodon-credentials))
         (script-template "#!/usr/bin/env python3
import tempfile
import subprocess
import tweepy
import argparse
from pathlib import Path
from typing import Optional
from mastodon import Mastodon
import mimetypes
import os
import sys

# Aggressive fix for Python 3.13 imghdr removal
# We need to patch tweepy before it gets imported
def patch_tweepy_for_python313():
    \"\"\"Patch tweepy's imghdr usage for Python 3.13 compatibility\"\"\"

    # Create fake imghdr module
    class FakeImghdr:
        @staticmethod
        def what(filename, h=None):
            \"\"\"Replacement for imghdr.what that handles the 'h' parameter\"\"\"
            # Ignore the 'h' parameter that's causing the issue
            mime_type, _ = mimetypes.guess_type(filename)
            if mime_type and mime_type.startswith('image/'):
                return mime_type.split('/')[-1]

            # Fallback to extension
            ext = os.path.splitext(filename)[1].lower()
            if ext in ['.jpg', '.jpeg']:
                return 'jpeg'
            elif ext == '.png':
                return 'png'
            elif ext == '.gif':
                return 'gif'
            elif ext == '.webp':
                return 'webp'
            return None

    # Replace imghdr in sys.modules
    sys.modules['imghdr'] = FakeImghdr()

    # Also patch tweepy.api if it's already imported
    try:
        import tweepy.api
        if hasattr(tweepy.api, 'imghdr'):
            tweepy.api.imghdr = FakeImghdr()
    except:
        pass

# Apply the patch before using tweepy
patch_tweepy_for_python313()

# Twitter API credentials
TWITTER_CONSUMER_KEY = \"%s\"
TWITTER_CONSUMER_SECRET = \"%s\"
TWITTER_ACCESS_TOKEN = \"%s\"
TWITTER_ACCESS_TOKEN_SECRET = \"%s\"

# Mastodon API credentials
MASTODON_INSTANCE = \"%s\"
MASTODON_ACCESS_TOKEN = \"%s\"

class TwitterClient:
    def __init__(self):
        # Set up Twitter API client with our credentials
        self.client = tweepy.Client(
            consumer_key=TWITTER_CONSUMER_KEY,
            consumer_secret=TWITTER_CONSUMER_SECRET,
            access_token=TWITTER_ACCESS_TOKEN,
            access_token_secret=TWITTER_ACCESS_TOKEN_SECRET,
        )
        # Set up auth for media uploads (needed specifically for images)
        auth = tweepy.OAuthHandler(TWITTER_CONSUMER_KEY, TWITTER_CONSUMER_SECRET)
        auth.set_access_token(TWITTER_ACCESS_TOKEN, TWITTER_ACCESS_TOKEN_SECRET)
        self.api = tweepy.API(auth)

    def upload_media(self, photo_path: str) -> int:
        \"\"\"Uploads media with Python 3.13 compatibility.\"\"\"
        try:
            print(f\"Attempting Twitter media upload: {photo_path}\")

            # Method 1: Try with file handle (bypasses some imghdr calls)
            with open(photo_path, 'rb') as f:
                # Read file content
                file_content = f.read()
                f.seek(0)

                # Try upload with file object
                media = self.api.media_upload(filename=os.path.basename(photo_path), file=f)
                print(f\"Twitter upload successful via file handle\")
                return media.media_id

        except Exception as e1:
            print(f\"File handle method failed: {e1}\")

            try:
                # Method 2: Direct filename approach
                media = self.api.media_upload(photo_path)
                print(f\"Twitter upload successful via filename\")
                return media.media_id

            except Exception as e2:
                print(f\"Filename method failed: {e2}\")

                try:
                    # Method 3: Use simple_upload directly
                    media = self.api.simple_upload(photo_path)
                    print(f\"Twitter upload successful via simple_upload\")
                    return media.media_id

                except Exception as e3:
                    raise Exception(f\"All upload methods failed. Last error: {e3}\")

    def create_tweet(self, text: str = None, media_ids: Optional[list] = None) -> None:
        \"\"\"Creates a tweet with optional media attachments.\"\"\"
        # Handle empty text for media-only tweets
        if not text or text.strip() == '':
            text = None
        self.client.create_tweet(text=text, media_ids=media_ids)

class MastodonClient:
    def __init__(self):
        \"\"\"Initialize Mastodon client.\"\"\"
        self.client = Mastodon(
            access_token=MASTODON_ACCESS_TOKEN,
            api_base_url=MASTODON_INSTANCE
        )

    def upload_media(self, media_path: str) -> dict:
        \"\"\"Uploads media and returns media dict for attachment to toots.\"\"\"
        return self.client.media_post(media_path)

    def create_toot(self, text: str = None, media_attachments: Optional[list] = None) -> None:
        \"\"\"Creates a toot with optional media attachments.\"\"\"
        # Mastodon allows empty text with media
        status_text = text if text and text.strip() else ''
        self.client.status_post(status_text, media_ids=media_attachments)

def validate_tweet(text: str) -> bool:
    \"\"\"Checks if tweet text meets Twitter's length requirements.\"\"\"
    if len(text) > 280:
        print(f\"Tweet exceeds 280 characters ({len(text)}). Please shorten your tweet.\")
        return False
    return True

def post_to_twitter(text: str, media_path: Optional[str] = None) -> tuple[bool, str]:
    \"\"\"Post to Twitter using your original working code.\"\"\"
    try:
        twitter = TwitterClient()

        # Handle media attachment if provided
        media_ids = None
        if media_path:
            media_ids = [twitter.upload_media(media_path)]
            print(f\"Media attached to Twitter: {media_path}\")

        # Post the tweet
        twitter.create_tweet(text, media_ids)
        return True, \"Twitter: Posted successfully\"

    except tweepy.errors.TweepyException as e:
        return False, f\"Twitter API error: {e}\"
    except Exception as e:
        return False, f\"Twitter unexpected error: {e}\"

def post_to_mastodon(text: str, media_path: Optional[str] = None) -> tuple[bool, str]:
    \"\"\"Post to Mastodon.\"\"\"
    try:
        mastodon = MastodonClient()

        # Handle media attachment if provided
        media_attachments = None
        if media_path:
            media_attachments = [mastodon.upload_media(media_path)]
            print(f\"Media attached to Mastodon: {media_path}\")

        # Post the toot
        mastodon.create_toot(text, media_attachments)
        return True, \"Mastodon: Posted successfully\"

    except Exception as e:
        return False, f\"Mastodon error: {e}\"

def main():
    print(f\"Python version: {sys.version}\")
    print(f\"Tweepy version: {tweepy.__version__}\")

    # Set up command-line argument parsing (same as your original)
    parser = argparse.ArgumentParser(description=\"Post to Twitter and Mastodon from Emacs\")
    parser.add_argument(
        \"--text-file\", type=str, required=True, help=\"File containing post text\"
    )
    parser.add_argument(\"--media\", type=str, help=\"Path to media file to attach\")
    args = parser.parse_args()

    try:
        # Read post text from the file provided by Elisp
        with open(args.text_file, \"r\") as f:
            post_text = f.read().strip()

        # Handle empty text (media-only posts)
        if not post_text and not args.media:
            print(\"Error: Post must contain either text or media (or both).\")
            return

        # Validate content (using your original validation)
        if post_text and not validate_tweet(post_text):
            return

        # Track results
        results = []
        overall_success = True

        # Post to Twitter (using your original working method)
        twitter_success, twitter_msg = post_to_twitter(post_text, args.media)
        results.append(twitter_msg)
        if not twitter_success:
            overall_success = False

        # Post to Mastodon
        mastodon_success, mastodon_msg = post_to_mastodon(post_text, args.media)
        results.append(mastodon_msg)
        if not mastodon_success:
            overall_success = False

        # Print results
        for result in results:
            print(result)

        if args.media:
            print(f\"Media attached: {args.media}\")

        # Summary
        if overall_success:
            print(\"✓ Successfully posted to all platforms!\")
        else:
            print(\"⚠ Some posts failed. Check messages above.\")

    except FileNotFoundError:
        print(f\"Error: Could not find text file: {args.text_file}\")
    except Exception as e:
        print(f\"An unexpected error occurred: {e}\")
        import traceback
        traceback.print_exc()

if __name__ == \"__main__\":
    main()
"))

    ;; Fill in the template with actual credentials
    (with-temp-file my-posse-script-path
      (insert (format script-template
                      (plist-get twitter-creds :consumer-key)
                      (plist-get twitter-creds :consumer-secret)
                      (plist-get twitter-creds :access-token)
                      (plist-get twitter-creds :access-token-secret)
                      (plist-get mastodon-creds :instance)
                      (plist-get mastodon-creds :access-token))))

    ;; Make the script executable
    (set-file-modes my-posse-script-path #o755)
    (message "Script generated with aggressive Python 3.13 fix!")))

(defun my-send-tweet-from-buffer ()
  "Send the post to both platforms using the generated script."
  (interactive)
  (let* ((content (buffer-substring-no-properties
                   (save-excursion
                     (goto-char (point-min))
                     (forward-line 2)
                     (point))
                   (point-max)))
         (tweet-text (string-trim content))
         (media (buffer-local-value 'media-path (current-buffer))))

    (cond
     ((and (string-empty-p tweet-text) (not media))
      (message "Post must contain either text or media (or both)."))
     ((and (not (string-empty-p tweet-text)) (> (length tweet-text) 280))
      (message "Post exceeds 280 characters (%d). Please shorten it."
               (length tweet-text)))
     ((and media (not (file-exists-p media)))
      (message "Selected media file does not exist: %s" media))
     (t
      ;; Generate script with credentials
      (my-generate-script-from-template)

      ;; Create temp file for text
      (let ((temp-file (make-temp-file "post-" nil ".txt")))
        (with-temp-file temp-file
          (insert tweet-text))

        ;; Build and execute command (same as your original)
        (let* ((media-arg (when media (format " --media %s" (shell-quote-argument media))))
               (command (format "python %s --text-file %s%s"
                                my-posse-script-path
                                (shell-quote-argument temp-file)
                                (or media-arg ""))))

          (let ((result (shell-command-to-string command)))
            (delete-file temp-file)
            (if (string-match-p "Successfully posted" result)
                (progn
                  (message "Posted to all platforms successfully!")
                  (kill-buffer))
              (message "Result: %s" result)))))))))

;; Keep your existing key binding
(map! :leader
      (:prefix ("t" . "Tweet")
       :desc "Post to all platforms" "t" #'my-post-tweet))

(provide 'my-posse)
;;; my-posse.el ends here
