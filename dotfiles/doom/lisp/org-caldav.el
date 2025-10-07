;;; ../../nixos-config/dotfiles/doom/lisp/org-caldav.el -*- lexical-binding: t; -*-

(use-package! org-caldav
  :config
  (setq org-caldav-url "http://localhost:5232/joshua"
        org-caldav-calendar-id "09880ecf-4174-e3b4-706f-c504ecb5e19b"
        org-caldav-inbox "~/org/caldav-inbox.org"
        org-caldav-files '("~/org/calendar.org")
        org-caldav-sync-direction 'twoway
        org-caldav-delete-org-entries 'ask
        org-caldav-delete-calendar-entries 'ask
        org-icalendar-timezone "America/Edmonton"
        ;; CRITICAL: Allow broken links during icalendar export
        org-icalendar-include-todo nil
        org-export-with-broken-links t)  ; Global override

  ;; Force org-export to ignore broken links
  (advice-add 'org-export-data :around
              (lambda (orig-fun &rest args)
                (let ((org-export-with-broken-links t))
                  (apply orig-fun args)))))

;; Also set globally for all org exports
(after! org
  (setq org-export-with-broken-links t))

;; Auto-sync on save in calendar.org
(defun my/org-caldav-sync-calendar ()
  "Sync CalDAV when saving calendar.org."
  (when (and buffer-file-name
             (string-equal (file-name-nondirectory buffer-file-name)
                           "calendar.org"))
    (org-caldav-sync)))

(add-hook 'after-save-hook #'my/org-caldav-sync-calendar)

;; Optional: sync after capture if capturing to calendar.org
(defun my/org-caldav-sync-after-capture ()
  "Sync CalDAV after capturing an event."
  (when (and (boundp 'org-capture-mode)
             org-capture-mode
             (member (buffer-file-name)
                     (mapcar #'expand-file-name org-caldav-files)))
    (run-with-timer 1 nil #'org-caldav-sync)))  ; Delay 1 sec to let capture finish

(add-hook 'org-capture-after-finalize-hook #'my/org-caldav-sync-after-capture)


(defun my/org-contacts-to-radicale ()
  "Export org-contacts to vCard and upload to Radicale."
  (interactive)
  (let* ((url "http://localhost:5232/joshua/81d7c750-eb55-53e8-7af9-0d08e8121b64/")
         (username "joshua")
         (auth-info (auth-source-search :host "localhost"
                                        :port 5232
                                        :user username
                                        :require '(:secret)))
         (password (if auth-info
                       (funcall (plist-get (car auth-info) :secret))
                     (error "No credentials found for Radicale"))))
    (with-current-buffer (find-file-noselect "~/org/contacts.org")
      (let ((uploaded 0)
            (failed 0))
        (org-map-entries
         (lambda ()
           (when-let* ((id (org-entry-get nil "ID"))
                       (name (org-entry-get nil "ITEM"))
                       (vcard (my/org-contact-to-vcard-single)))
             (let* ((filename (format "%s.vcf" id))
                    (full-url (concat url filename))
                    (temp-file (make-temp-file "contact-" nil ".vcf")))
               ;; Write vCard to temp file
               (write-region vcard nil temp-file)
               ;; Upload via curl
               (let ((result (shell-command-to-string
                              (format "curl -s -w '%%{http_code}' -u %s:%s -X PUT -H 'Content-Type: text/vcard' --data-binary @%s '%s'"
                                      username password temp-file full-url))))
                 (delete-file temp-file)
                 (if (string-match "20[0-4]$" result)  ; Accept 200-204
                     (setq uploaded (1+ uploaded))
                   (progn
                     (setq failed (1+ failed))
                     (message "Failed to upload %s: %s" name result)))))))
         "LEVEL=1"
         'file)
        (message "Uploaded %d contacts, %d failed" uploaded failed)))))

(defun my/org-contact-to-vcard-single ()
  "Convert current org entry to vCard format."
  (let ((name (org-entry-get nil "ITEM"))
        (phone (org-entry-get nil "PHONE"))
        (email (org-entry-get nil "EMAIL"))
        (birthday (org-entry-get nil "BIRTHDAY"))
        (id (org-entry-get nil "ID")))
    (when name
      (string-join
       (delq nil
             (list
              "BEGIN:VCARD"
              "VERSION:3.0"
              (format "UID:urn:uuid:%s" id)
              (format "FN:%s" name)
              (format "N:%s;;;;" name)  ; Added surname field
              (when phone (format "TEL;TYPE=CELL:%s" phone))
              (when email (format "EMAIL:%s" email))
              (when birthday
                (format "BDAY:%s"
                        (format-time-string "%Y%m%d"
                                            (org-time-string-to-time birthday))))
              (format "REV:%s" (format-time-string "%Y%m%dT%H%M%SZ"))
              "END:VCARD"))
       "\n"))))

(defun my/auto-export-contacts-to-radicale ()
  "Auto-export contacts to Radicale on save."
  (when (and buffer-file-name
             (string-equal (file-name-nondirectory buffer-file-name)
                           "contacts.org"))
    (my/org-contacts-to-radicale)))

(add-hook 'after-save-hook #'my/auto-export-contacts-to-radicale)
