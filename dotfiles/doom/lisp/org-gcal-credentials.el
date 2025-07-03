;;; org-gcal-credentials.el -*- lexical-binding: t; -*-
;;;
;;; This file contains private credentials for org-gcal and is not under version control.
;;; It's loaded by my main config.el file.

(after! org
  (require 'auth-source)  ; Make sure this is here
  (use-package! org-gcal
    :config
    (setq org-gcal-client-id (auth-source-pick-first-password :host "gcal" :user "client-id")
          org-gcal-client-secret (auth-source-pick-first-password :host "gcal" :user "client-secret")
          org-gcal-file-alist `((,(auth-source-pick-first-password :host "gcal" :user "calendar-email") . "~/org/calendar.org"))
          org-gcal-request-ptbr t
          org-gcal-token-file nil
          org-gcal-fetch-event-filters '((lambda (event)
                                           (let ((start (org-gcal--get-time-and-desc event 'start)))
                                             (time-less-p (current-time) (org-gcal--parse-date start)))))
          org-gcal-notify-p t)
    ;; Force account selection by modifying the auth URL
    (defadvice org-gcal--get-auth-url (after force-account-selection activate)
      "Add prompt=select_account to force Google account selection."
      (setq ad-return-value
            (concat ad-return-value "&prompt=select_account&access_type=offline")))))
(provide 'org-gcal-credentials)
