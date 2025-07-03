;;; ../../dotfiles/doom/.config/doom/lisp/mu4e-contact.el -*- lexical-binding: t; -*-

(defvar my/contacts-cache nil
  "Cache of contacts from the contacts.org file.")

(defvar my/contacts-file "~/org/contacts.org"
  "Path to the org file containing contacts.")

(defvar my/contacts-last-modified nil
  "Last modification time of the contacts file.")

(defun my/parse-contacts-file ()
  "Parse the contacts.org file and return a list of contacts with their emails."
  (let ((contacts '())
        (contact-pattern "^\\*\\* \\(.+\\)$")
        (email-pattern ":EMAIL: \\(.+\\)$")
        (current-name nil)
        (current-email nil)
        (file-mtime (nth 5 (file-attributes my/contacts-file))))

    ;; Only parse if cache is empty or file has been modified
    (when (or (null my/contacts-cache)
              (null my/contacts-last-modified)
              (time-less-p my/contacts-last-modified file-mtime))

      (setq my/contacts-last-modified file-mtime)

      (with-temp-buffer
        (insert-file-contents my/contacts-file)
        (goto-char (point-min))

        ;; Parse file line by line
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))

            ;; Check if this line contains a contact name
            (if (string-match contact-pattern line)
                (setq current-name (match-string 1 line))

              ;; Check if this line contains email addresses
              (when (and current-name (string-match email-pattern line))
                (setq current-email (match-string 1 line))

                ;; Handle multiple email addresses separated by commas
                (let ((emails (split-string current-email "," t "[ \t]+")))
                  (dolist (email emails)
                    (let ((formatted-entry (format "%s <%s>" current-name email)))
                      (push (cons formatted-entry
                                  (list :name current-name :email email :formatted formatted-entry))
                            contacts)))))))

          (forward-line 1)))

      ;; Sort contacts alphabetically by name
      (setq contacts (sort contacts (lambda (a b)
                                      (string< (car a) (car b)))))

      ;; Update cache
      (setq my/contacts-cache contacts))

    my/contacts-cache))

(defun my/insert-contact-email (&optional arg)
  "Insert an email address from contacts.org file with completion.
With prefix argument ARG, allow selecting multiple contacts."
  (interactive "P")
  (let* ((contacts (my/parse-contacts-file))
         (multiple-p (and arg t))
         (selected-contacts '())
         (candidates (mapcar #'car contacts)))

    ;; Offer completion if we found any contacts
    (if contacts
        (progn
          (if multiple-p
              ;; Multiple contact selection loop
              (while (let* ((prompt (format "Contact%s: (finish with empty input) "
                                            (if selected-contacts
                                                (format " [%d selected]" (length selected-contacts))
                                              "")))
                            (selection (completing-read prompt candidates nil t)))
                       (unless (string-empty-p selection)
                         (push selection selected-contacts)
                         t)))

            ;; Single contact selection
            (let ((selection (completing-read "Contact: " candidates nil t)))
              (push selection selected-contacts)))

          ;; Insert selected contacts
          (let ((emails (mapcar (lambda (contact)
                                  (plist-get (cdr (assoc contact contacts)) :email))
                                (nreverse selected-contacts))))
            (insert (string-join emails ", "))))

      (message "No contacts with email addresses found in %s" my/contacts-file))))

;; Function to filter contacts
(defun my/filter-contacts-by-name (name-filter)
  "Filter contacts by NAME-FILTER and select from the results."
  (interactive "sFilter contacts by name: ")
  (let* ((contacts (my/parse-contacts-file))
         (filtered-contacts
          (seq-filter (lambda (contact)
                        (string-match-p name-filter (car contact)))
                      contacts))
         (candidates (mapcar #'car filtered-contacts)))

    (if filtered-contacts
        (let* ((selection (completing-read "Contact: " candidates nil t))
               (email (plist-get (cdr (assoc selection filtered-contacts)) :email)))
          (insert email))
      (message "No contacts matching '%s' found" name-filter))))

;; Create company backend for contacts
(defun my/company-contacts (command &optional arg &rest ignored)
  "Company backend for contact completion."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'my/company-contacts))
    (prefix (and (eq major-mode 'message-mode)
                 (looking-back "\\(^\\(To\\|Cc\\|Bcc\\): .*\\|, *\\)\\([a-zA-Z0-9._%+-]+\\)" (line-beginning-position))
                 (match-string 3)))
    (candidates
     (let* ((contacts (my/parse-contacts-file))
            (prefix arg)
            (candidates (mapcar (lambda (contact)
                                  (plist-get (cdr contact) :formatted))
                                contacts)))
       (seq-filter (lambda (c) (string-prefix-p prefix c t)) candidates)))
    (annotation (format " [Contact]"))
    (sorted t)))

;; Set up keybindings and hooks
(with-eval-after-load 'message
  ;; Simple contacts insertion
  (define-key message-mode-map (kbd "C-c i") 'my/insert-contact-email)

  ;; Filtered contacts insertion
  (define-key message-mode-map (kbd "C-c f") 'my/filter-contacts-by-name)

  ;; Add company backend when in message-mode
  (with-eval-after-load 'company
    (add-hook 'message-mode-hook
              (lambda ()
                (add-to-list 'company-backends 'my/company-contacts)))))

;; Advice to enable multiple contact selection with prefix
(defadvice message-insert-formatted-citation-line (after my/setup-completion activate)
  "Set up contact completion after citation line is inserted."
  (setq-local company-idle-delay 0.2)
  (company-mode 1))
