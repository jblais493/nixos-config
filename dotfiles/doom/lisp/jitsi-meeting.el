;;; ../../nixos-config/dotfiles/doom/lisp/jitsi-meeting.el -*- lexical-binding: t; -*-

;; Define jitsi base URL
(defvar jitsi-base-url "https://meet.jit.si/"
  "Base URL for Jitsi meetings.")

(defun my/jitsi-generate-room-name ()
  "Generate room name using scheduled time if available, current time otherwise."
  (let* ((scheduled-time (when (and (derived-mode-p 'org-mode)
                                    (org-at-heading-p))
                           (org-get-scheduled-time nil)))
         (use-time (or scheduled-time (current-time)))
         (date-part (format-time-string "%Y%m%d-%H%M" use-time))
         (random-part (format "%04x" (random 65536))))
    (format "meeting-%s-%s" date-part random-part)))

(defun my/jitsi-create-room ()
  "Create a Jitsi meeting room and copy URL to clipboard."
  (interactive)
  (let* ((room-name (my/jitsi-generate-room-name))
         (full-url (concat jitsi-base-url room-name)))
    ;; Copy to clipboard
    (kill-new full-url)
    ;; Insert in current buffer
    (insert full-url)
    ;; Show user what happened
    (message "Jitsi room created and copied to clipboard: %s" full-url)
    ;; Return the URL in case other functions need it
    full-url))

;; Keybinding
(map! :leader
      (:prefix ("j" . "jitsi")
       :desc "Create jitsi room" "c" #'my/jitsi-create-room))
