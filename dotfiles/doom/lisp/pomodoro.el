;;; pomodoro.el --- A simple pomodoro timer -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025
;;
;; Author: Joshua Blais <josh@joshblais.com>
;; Maintainer: Joshua Blais <josh@joshblais.com>
;; Created: February 04, 2025
;; Modified: February 04, 2025
;; Version: 0.0.1
;; Keywords: tools
;; Homepage: https://github.com/jblais493/pomodoro
;; Package-Requires: ((emacs "29.1") (alert "1.2"))
;;
;;; Commentary:
;;
;;  A simple pomodoro timer implementation with countdown display and org logging
;;
;;; Code:

;;(require 'alert)

;; Core variables for timer functionality
(defvar pomodoro-work-minutes 25  ; Set to 1 for testing
  "Work period length in minutes.")

(defvar pomodoro-break-minutes 5  ; Set to 1 for testing
  "Break period length in minutes.")

(defvar pomodoro-task ""
  "Current task being worked on.")

(defvar pomodoro-timer nil
  "Timer object for the main pomodoro countdown.")

(defvar pomodoro-mode-line ""
  "String to display in mode line.")

(defvar pomodoro-end-time nil
  "When the current period ends.")

(defvar pomodoro-update-timer nil
  "Timer object for updating the display.")

(defvar pomodoro-start-time nil
  "When the current work period started.")

(defvar pomodoro-break-p nil
  "Flag to track if we're in a break period.")

;; Add our display to the mode line
(unless (member 'pomodoro-mode-line global-mode-string)
  (setq global-mode-string
        (append global-mode-string '(pomodoro-mode-line))))

;; Alert function that uses the alert package instead of direct sound playing
(defun pomodoro-play-alert (message)
  "Send a desktop notification with MESSAGE and play a sound when pomodoro period ends.
Uses Emacs' built-in notification system for better system integration."
  (when (fboundp 'notifications-notify)
    (notifications-notify
     :title "Pomodoro Timer"
     :body message
     :urgency 'critical
     :sound-file "~/Music/Bell.wav"
     :sound-name "alarm-clock-elapsed")))

;; Function to ensure the done.org file exists and has today's date
(defun pomodoro-ensure-done-file ()
  "Ensure the done.org file exists and has today's date heading."
  (let ((done-file (expand-file-name "~/org/done.org"))
        (today (format-time-string "* %Y-%m-%d %A")))
    (unless (file-exists-p done-file)
      (append-to-file "" nil done-file))

    (with-temp-buffer
      (when (file-exists-p done-file)
        (insert-file-contents done-file))
      (goto-char (point-min))
      (unless (search-forward today nil t)
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert today "\n")
        (write-region (point-min) (point-max) done-file nil 'quiet)))))

;; Enhanced logging function with better file handling
(defun pomodoro-log-session (task completed)
  "Log the completed pomodoro session to done.org."
  (let* ((done-file (expand-file-name "~/org/done.org"))
         (today (format-time-string "* %Y-%m-%d %A"))
         (start-time (format-time-string "%H:%M" pomodoro-start-time))
         (end-time (format-time-string "%H:%M" (current-time)))
         (entry (format "** %s-%s: %s\n   %s\n"
                        start-time end-time task completed)))
    (with-temp-buffer
      (when (file-exists-p done-file)
        (insert-file-contents done-file))
      (goto-char (point-min))
      (if (search-forward today nil t)
          (forward-line 1)
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert today "\n"))
      (insert entry)
      (write-region (point-min) (point-max) done-file nil 'quiet))))

;; Improved display update function with work/break indication
(defun pomodoro-update-display ()
  "Update the mode line display."
  (when pomodoro-end-time
    (let* ((remaining-seconds (round (float-time (time-subtract pomodoro-end-time (current-time)))))
           (remaining-minutes (/ remaining-seconds 60))
           (remaining-secs (mod remaining-seconds 60)))
      (if (> remaining-seconds 0)
          (setq pomodoro-mode-line
                (format " [%s %02d:%02d %s] "
                        (if pomodoro-break-p "☕" "✝")
                        remaining-minutes
                        remaining-secs
                        (if pomodoro-break-p "Break" pomodoro-task)))
        (setq pomodoro-mode-line "")))
    (force-mode-line-update)))

;; Core timer functions
(defun pomodoro-start ()
  "Start a new Pomodoro session."
  (interactive)
  (when pomodoro-update-timer
    (cancel-timer pomodoro-update-timer))
  (when pomodoro-timer
    (cancel-timer pomodoro-timer))
  (pomodoro-ensure-done-file)
  (setq pomodoro-task (read-string "What are you working on? "))
  (pomodoro-work-period))

(defun pomodoro-work-period ()
  "Start a work period."
  (setq pomodoro-break-p nil)
  (message "Starting %d minute work period on: %s" pomodoro-work-minutes pomodoro-task)
  (setq pomodoro-start-time (current-time))
  (setq pomodoro-end-time (time-add (current-time)
                                    (seconds-to-time (* pomodoro-work-minutes 60))))
  (when pomodoro-update-timer
    (cancel-timer pomodoro-update-timer))
  (setq pomodoro-update-timer (run-at-time nil 1 #'pomodoro-update-display))
  (when pomodoro-timer
    (cancel-timer pomodoro-timer))
  (setq pomodoro-timer
        (run-at-time (* pomodoro-work-minutes 60) nil #'pomodoro-work-done)))

(defun pomodoro-work-done ()
  "Handle work period completion."
  (pomodoro-play-alert "Work period complete!")
  (let ((completed (read-string "What did you accomplish? ")))
    (pomodoro-log-session pomodoro-task completed)
    (message "Work period complete! Accomplished: %s" completed)
    (pomodoro-break-period)))

(defun pomodoro-break-period ()
  "Start a break period."
  (setq pomodoro-break-p t)
  (message "Starting %d minute break" pomodoro-break-minutes)
  (setq pomodoro-end-time (time-add (current-time)
                                    (seconds-to-time (* pomodoro-break-minutes 60))))
  (when pomodoro-timer
    (cancel-timer pomodoro-timer))
  (setq pomodoro-timer
        (run-at-time (* pomodoro-break-minutes 60) nil #'pomodoro-break-done)))

(defun pomodoro-break-done ()
  "Handle break period completion."
  (pomodoro-play-alert "Break complete! Start new session?")
  (pomodoro-start))

;; Kill function to stop the timer
(defun pomodoro-kill ()
  "Kill the current pomodoro session."
  (interactive)
  (when pomodoro-update-timer
    (cancel-timer pomodoro-update-timer))
  (when pomodoro-timer
    (cancel-timer pomodoro-timer))
  (setq pomodoro-mode-line "")
  (force-mode-line-update)
  (message "Pomodoro timer stopped."))

;; Keybindings
(global-set-key (kbd "C-c p") 'pomodoro-start)
(global-set-key (kbd "C-c P") 'pomodoro-kill)

(provide 'pomodoro)
;;; pomodoro.el ends here
