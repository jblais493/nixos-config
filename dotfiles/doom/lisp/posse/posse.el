;;; posse.el --- POSSE content syndication system -*- lexical-binding: t; -*-

;;; Commentary:
;; Top-level module for POSSE (Publish Own Site, Syndicate Elsewhere) system.
;; Integrates multiple publishing platforms into Doom Emacs.

;;; Code:

(require 'cl-lib)

(defgroup posse nil
  "POSSE content syndication system."
  :group 'applications
  :prefix "posse-")

;; Platform configuration
(defcustom posse-platforms
  '((twitter . t)
    (instagram . nil)
    (mastodon . nil)
    (linkedin . nil)
    (youtube . nil))
  "Enabled publishing platforms."
  :type '(alist :key-type symbol :value-type boolean)
  :group 'posse)

;; Main entry point
(defun posse-create-post ()
  "Start creating a new post for syndication."
  (interactive)
  (let* ((platforms (cl-remove-if-not #'cdr posse-platforms))
         (choices (mapcar (lambda (p) (symbol-name (car p))) platforms))
         (selected (completing-read-multiple "Select platforms: " choices)))
    (cond
     ((member "twitter" selected)
      (require 'posse-twitter)
      (posse-twitter-compose))
     (t
      (message "No platform selected or platforms not yet implemented")))))

;; API key management
(defvar posse-api-keys (make-hash-table :test 'equal)
  "Hash table of API keys for various platforms.")

(defun posse-set-api-key (platform key)
  "Store API KEY for PLATFORM."
  (puthash platform key posse-api-keys))

(defun posse-get-api-key (platform)
  "Get the API key for PLATFORM."
  (gethash platform posse-api-keys))

;; Show README/Documentation
(defun posse-show-readme ()
  "Show the POSSE system README."
  (interactive)
  (with-current-buffer (get-buffer-create "*POSSE README*")
    (erase-buffer)
    (org-mode)
    (insert "#+TITLE: POSSE System Documentation\n\n")
    (insert "* Overview\n\n")
    (insert "The POSSE (Publish Own Site, Syndicate Elsewhere) system allows you to:\n\n")
    (insert "- Create content in Org mode\n")
    (insert "- Publish to your Hugo blog as the primary destination\n")
    (insert "- Syndicate content to various social platforms\n\n")
    (insert "* Usage\n\n")
    (insert "- M-x posse-create-post :: Start a new post\n")
    (insert "- M-x posse-twitter-compose :: Compose a tweet\n\n")
    (insert "* Platforms\n\n")
    (insert "Currently implemented:\n")
    (insert "- Twitter\n\n")
    (insert "Coming soon:\n")
    (insert "- Instagram/Facebook\n")
    (insert "- Mastodon\n")
    (insert "- LinkedIn\n")
    (insert "- YouTube\n"))
  (switch-to-buffer "*POSSE README*")
  (goto-char (point-min)))

(provide 'posse)
;;; posse.el ends here
