;;; org-meeting-notify.el --- Simple meeting notifications for org-agenda -*- lexical-binding: t; -*-

;;; Commentary:
;; This package provides simple notification functionality for meetings in org-agenda.
;; It sends notifications at 30 minutes and 5 minutes before meetings,
;; and a clickable notification 1 minute before that opens the meeting link.

;;; Code:

(require 'org)
(require 'org-agenda)
(require 'browse-url)

(defgroup org-meeting-notify nil
  "Customization options for org-meeting-notify."
  :group 'org)

(defcustom org-meeting-notify-times '(30 5 1)
  "List of times (in minutes) before meetings to send notifications."
  :type '(repeat integer)
  :group 'org-meeting-notify)

(defvar org-meeting-notify-timers nil
  "List of active meeting notification timers.")

;; Core function to extract meeting information from org entries
(defun org-meeting-notify-extract-info ()
  "Extract upcoming meetings with :MEETING: tag from org-agenda files."
  (let ((meetings nil))
    (org-map-entries
     (lambda ()
       (let* ((tags (org-get-tags))
              (has-meeting-tag (member "MEETING" tags))
              (scheduled-time (org-get-scheduled-time (point)))
              (deadline-time (org-get-deadline-time (point)))
              (timestamp (or scheduled-time deadline-time))
              (heading (org-get-heading t t t t))
              (properties (org-entry-properties))
              (meeting-link (or (cdr (assoc "ZOOM_LINK" properties))
                                (cdr (assoc "TEAMS_LINK" properties))
                                (cdr (assoc "MEET_LINK" properties))
                                (cdr (assoc "JITSI_LINK" properties))
                                (cdr (assoc "MEETING_LINK" properties))
                                (cdr (assoc "URL" properties))
                                (cdr (assoc "LINK" properties))))
              (time-string (and timestamp (format-time-string "%Y-%m-%d %H:%M" timestamp)))
              (current-file (buffer-file-name))
              (marker (point-marker)))

         ;; Only include entries with the MEETING tag and a timestamp in the future
         (when (and has-meeting-tag
                    timestamp
                    (time-less-p (current-time) timestamp))
           (push (list :heading heading
                       :time timestamp
                       :time-string time-string
                       :link meeting-link
                       :file current-file
                       :marker marker)
                 meetings))))
     "+MEETING" 'agenda)

    ;; Sort meetings by time
    (sort meetings (lambda (a b)
                     (time-less-p (plist-get a :time) (plist-get b :time))))))

;; Function to send a notification
(defun org-meeting-notify-send (title message &optional link)
  "Send a desktop notification with TITLE and MESSAGE.
If LINK is provided, create a clickable notification to open that link."
  (if link
      ;; For clickable notifications that open the meeting link
      (let* ((notification-id (format "org-meeting-%d" (random 10000)))
             ;; Create a more reliable script for opening the link
             (temp-script (make-temp-file "meeting-notify" nil ".sh")))

        ;; Create a shell script that will open the link in the browser
        (with-temp-file temp-script
          (insert "#!/bin/bash\n\n")
          ;; Try multiple browsers in order of preference
          (insert "# Try opening with zen-browser first\n")
          (insert (format "zen-browser \"%s\" || \\\n" link))
          (insert "# Fall back to xdg-open\n")
          (insert (format "xdg-open \"%s\" || \\\n" link))
          (insert "# Last resort: try firefox, chrome, etc.\n")
          (insert (format "firefox \"%s\" || google-chrome \"%s\" || chromium \"%s\"\n"
                          link link link)))

        ;; Make it executable
        (shell-command (format "chmod +x %s" temp-script))

        ;; Send notification with action that calls our script
        (start-process "notify-send" nil "notify-send"
                       (format "--hint=string:x-canonical-private-synchronous:%s" notification-id)
                       (format "--action=default=Join:%s" temp-script)
                       "--app-name=Org Meeting"
                       "--icon=appointment-soon"
                       title
                       (concat message "\n\nClick to join meeting"))

        ;; As a fallback, display a prompt in Emacs after a short delay
        (run-with-timer 2 nil
                        (lambda ()
                          (when (y-or-n-p "Join meeting now? ")
                            (browse-url link)))))

    ;; Regular notification without clickable action
    (start-process "notify-send" nil "notify-send"
                   "--app-name=Org Meeting"
                   "--icon=appointment-soon"
                   title
                   message)))

;; Function to set up notifications for a single meeting
(defun org-meeting-notify-setup-for-meeting (meeting)
  "Set up notifications for MEETING at specified intervals."
  (let* ((heading (plist-get meeting :heading))
         (timestamp (plist-get meeting :time))
         (link (plist-get meeting :link))
         (file (plist-get meeting :file))
         (marker (plist-get meeting :marker)))

    ;; For each notification time
    (dolist (minutes-before org-meeting-notify-times)
      (let* ((notification-time (time-subtract timestamp
                                               (seconds-to-time (* 60 minutes-before))))
             ;; Don't schedule notifications in the past
             (now (current-time))
             (seconds-from-now (float-time (time-subtract notification-time now))))

        ;; Only schedule if it's in the future
        (when (> seconds-from-now 0)
          (let ((timer
                 (run-at-time notification-time nil
                              (lambda ()
                                (let ((notif-title
                                       (format "Meeting in %d minute%s"
                                               minutes-before
                                               (if (= minutes-before 1) "" "s")))
                                      (notif-message
                                       (format "%s\n\nTime: %s"
                                               heading
                                               (plist-get meeting :time-string))))

                                  ;; If this is the 1-minute warning, make it clickable with the meeting link
                                  (if (and (= minutes-before 1) link)
                                      (org-meeting-notify-send notif-title notif-message link)
                                    (org-meeting-notify-send notif-title notif-message)))))))

            ;; Store the timer so we can cancel it later if needed
            (push timer org-meeting-notify-timers)))))))

;; Main function to set up all meeting notifications
(defun org-meeting-notify-setup ()
  "Set up notifications for all upcoming meetings with the MEETING tag."
  (interactive)

  ;; Cancel any existing notification timers
  (when org-meeting-notify-timers
    (dolist (timer org-meeting-notify-timers)
      (when (timerp timer)
        (cancel-timer timer)))
    (setq org-meeting-notify-timers nil))

  ;; Get upcoming meetings
  (let ((meetings (org-meeting-notify-extract-info)))
    (if meetings
        (progn
          (dolist (meeting meetings)
            (org-meeting-notify-setup-for-meeting meeting))
          (message "Set up notifications for %d upcoming meetings" (length meetings)))
      (message "No upcoming meetings found with the MEETING tag"))))

;; Test function with a dummy Jitsi link
(defun org-meeting-notify-test ()
  "Send a test notification with a clickable Jitsi link."
  (interactive)
  (let ((jitsi-link "https://meet.jit.si/TestMeeting"))
    (org-meeting-notify-send
     "Test Meeting Notification"
     "This is a test of the meeting notification system.\n\nMeeting: Test Meeting\nTime: In 1 minute"
     jitsi-link)

    (message "Sent test notification with Jitsi link: %s" jitsi-link)))

;; Enable automatic notification setup when Emacs starts
(defun org-meeting-notify-enable ()
  "Enable automatic meeting notifications."
  (interactive)
  ;; Setup notifications initially
  (org-meeting-notify-setup)

  ;; Set up periodic refresh (hourly)
  (run-at-time "1 hour" 3600 'org-meeting-notify-setup)

  (message "Meeting notifications enabled. Will check hourly for new meetings."))

(provide 'org-meeting-notify)
;;; org-meeting-notify.el ends here
