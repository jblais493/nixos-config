;;; universal-launcher.el --- Optimized universal launcher

;;; Commentary:
;; Simplified version that uses the existing Emacs frame

;;; Code:

(require 'all-the-icons)
(require 'json)
(require 'url-util)
(require 'calc)

;; Pre-grouped category structure for aesthetic grouping
(defvar universal-launcher--categories
  '((:name "Active" :icon "device-desktop" :types (buffer running))
    (:name "Files & Apps" :icon "apps" :types (file app flatpak))
    (:name "Web" :icon "globe" :types (bookmark firefox-action))
    (:name "System" :icon "terminal" :types (command))
    (:name "Tools" :icon "wrench" :types (emoji calculator)))
  "Category definitions for the launcher.")

;; Enhanced cache system
(defvar universal-launcher--all-candidates nil "Pre-computed candidates.")
(defvar universal-launcher--last-update 0 "Last time candidates were updated.")
(defvar universal-launcher--update-interval 30 "Update interval in seconds.")
(defvar universal-launcher--previous-frame nil "The previous frame to return to.")

;; Emoji data
(defvar universal-launcher--common-emojis
  '(("Smiling Face" . "😊")
    ("Heart" . "❤️")
    ("Thumbs Up" . "👍")
    ("Thinking Face" . "🤔")
    ("Fire" . "🔥")
    ("Star" . "⭐")
    ("Check Mark" . "✅")
    ("Rocket" . "🚀")
    ("Party Popper" . "🎉")
    ("Eyes" . "👀")
    ("Laughing Face" . "😂")
    ("Clapping Hands" . "👏")
    ("Folded Hands" . "🙏")
    ("Muscle" . "💪")
    ("Sparkles" . "✨")
    ("Warning" . "⚠️")
    ("Information" . "ℹ️")
    ("Question Mark" . "❓")
    ("Prohibited" . "🚫")
    ("Calendar" . "📅")
    ("Clock" . "⏰")
    ("Mail" . "📧")
    ("Lock" . "🔒")
    ("Magnifying Glass" . "🔍")
    ("Light Bulb" . "💡"))
  "Common emojis for quick access.")

;; Icon cache with category-specific icons
(defvar universal-launcher--icon-cache
  (let ((cache (make-hash-table :test 'equal)))
    ;; Type icons with consistent styling
    (puthash 'buffer (all-the-icons-octicon "file-code" :face '(:foreground "#61afef" :height 0.9)) cache)
    (puthash 'running (all-the-icons-material "desktop_windows" :face '(:foreground "#98c379" :height 0.9)) cache)
    (puthash 'app (all-the-icons-faicon "cube" :face '(:foreground "#c678dd" :height 0.9)) cache)
    (puthash 'flatpak (all-the-icons-material "layers" :face '(:foreground "#56b6c2" :height 0.9)) cache)
    (puthash 'firefox (all-the-icons-faicon "firefox" :face '(:foreground "#e06c75" :height 0.9)) cache)
    (puthash 'bookmark (all-the-icons-octicon "bookmark" :face '(:foreground "#d19a66" :height 0.9)) cache)
    (puthash 'file (all-the-icons-faicon "file" :face '(:foreground "#abb2bf" :height 0.9)) cache)
    (puthash 'command (all-the-icons-octicon "terminal" :face '(:foreground "#98c379" :height 0.9)) cache)
    (puthash 'emoji (all-the-icons-material "insert_emoticon" :face '(:foreground "#e5c07b" :height 0.9)) cache)
    (puthash 'calculator (all-the-icons-faicon "calculator" :face '(:foreground "#56b6c2" :height 0.9)) cache)
    ;; Category icons with matching style
    (puthash "Active" (all-the-icons-material "dashboard" :face '(:foreground "#61afef" :weight bold :height 1.0)) cache)
    (puthash "Files & Apps" (all-the-icons-material "apps" :face '(:foreground "#c678dd" :weight bold :height 1.0)) cache)
    (puthash "Web" (all-the-icons-material "public" :face '(:foreground "#e06c75" :weight bold :height 1.0)) cache)
    (puthash "System" (all-the-icons-material "settings_applications" :face '(:foreground "#98c379" :weight bold :height 1.0)) cache)
    (puthash "Tools" (all-the-icons-material "build" :face '(:foreground "#d19a66" :weight bold :height 1.0)) cache)
    cache)
  "Pre-loaded icon cache with consistent styling.")

;; Add fallback icon function
(defun universal-launcher--get-icon-safe (type)
  "Get icon for TYPE with fallback."
  (condition-case nil
      (or (gethash type universal-launcher--icon-cache)
          (all-the-icons-octicon "dash" :face '(:foreground "#abb2bf" :height 0.9)))
    (error "")))

(defun universal-launcher--get-file-icon (filename)
  "Get appropriate icon for FILENAME based on its extension."
  (let ((ext (file-name-extension filename)))
    (cond
     ((null ext) (all-the-icons-faicon "file" :face 'font-lock-doc-face))
     ((string= ext "org") (all-the-icons-fileicon "org" :face 'org-level-1))
     ((member ext '("js" "jsx" "ts" "tsx")) (all-the-icons-alltheicon "javascript" :face 'font-lock-type-face))
     ((string= ext "py") (all-the-icons-alltheicon "python" :face 'font-lock-keyword-face))
     ((string= ext "rb") (all-the-icons-fileicon "ruby" :face 'font-lock-type-face))
     ((string= ext "java") (all-the-icons-fileicon "java" :face 'font-lock-function-name-face))
     ((string= ext "c") (all-the-icons-fileicon "c" :face 'font-lock-keyword-face))
     ((string= ext "cpp") (all-the-icons-fileicon "cpp" :face 'font-lock-keyword-face))
     ((string= ext "h") (all-the-icons-fileicon "h" :face 'font-lock-preprocessor-face))
     ((string= ext "go") (all-the-icons-alltheicon "go" :face 'font-lock-keyword-face))
     ((string= ext "svelte") (all-the-icons-fileicon "svelte" :face 'font-lock-type-face))
     ((string= ext "rs") (all-the-icons-alltheicon "rust" :face 'font-lock-type-face))
     ((string= ext "php") (all-the-icons-fileicon "php" :face 'font-lock-function-name-face))
     ((string= ext "el") (all-the-icons-fileicon "elisp" :face 'font-lock-variable-name-face))
     ((string= ext "clj") (all-the-icons-fileicon "clojure" :face 'font-lock-function-name-face))
     ((string= ext "hs") (all-the-icons-fileicon "haskell" :face 'font-lock-function-name-face))
     ((string= ext "sh") (all-the-icons-fileicon "powershell" :face 'font-lock-builtin-face))
     ((string= ext "css") (all-the-icons-alltheicon "css3" :face 'font-lock-variable-name-face))
     ((string= ext "html") (all-the-icons-faicon "html5" :face 'font-lock-function-name-face))
     ((string= ext "json") (all-the-icons-fileicon "jsonld" :face 'font-lock-constant-face))
     ((string= ext "md") (all-the-icons-octicon "markdown" :face 'markdown-header-face))
     ((string= ext "yml") (all-the-icons-fileicon "jsonld" :face 'font-lock-variable-name-face))
     ((string= ext "pdf") (all-the-icons-faicon "file-pdf-o" :face 'font-lock-doc-face))
     ((member ext '("jpg" "jpeg" "png" "gif" "svg")) (all-the-icons-faicon "file-image-o" :face 'font-lock-string-face))
     ((member ext '("zip" "tar" "gz" "rar" "7z")) (all-the-icons-faicon "file-archive-o" :face 'font-lock-preprocessor-face))
     ((member ext '("doc" "docx")) (all-the-icons-faicon "file-word-o" :face 'font-lock-keyword-face))
     ((member ext '("xls" "xlsx")) (all-the-icons-faicon "file-excel-o" :face 'font-lock-type-face))
     ((member ext '("ppt" "pptx")) (all-the-icons-faicon "file-powerpoint-o" :face 'font-lock-function-name-face))
     ((member ext '("mp3" "wav" "flac" "ogg")) (all-the-icons-faicon "file-audio-o" :face 'font-lock-builtin-face))
     ((member ext '("mp4" "avi" "mkv" "mov")) (all-the-icons-faicon "file-video-o" :face 'font-lock-constant-face))
     (t (all-the-icons-faicon "file" :face 'font-lock-doc-face)))))

(defun universal-launcher--grouped-candidates ()
  "Return candidates grouped by category."
  (let ((candidates '())
        (category-handlers (make-hash-table :test 'eq)))

    ;; Define handlers for each type
    (puthash 'buffer
             (lambda ()
               (mapcar (lambda (buffer)
                         (cons (format "%s Buffer: %s"
                                       (universal-launcher--get-icon 'buffer)
                                       (buffer-name buffer))
                               (list 'buffer buffer)))
                       (buffer-list)))
             category-handlers)

    (puthash 'running
             (lambda ()
               (mapcar (lambda (app)
                         (cons (format "%s Running: %s"
                                       (universal-launcher--get-icon 'running)
                                       (car app))
                               (list 'running (cdr app))))
                       (universal-launcher--get-running-applications)))
             category-handlers)

    (puthash 'file
             (lambda ()
               (mapcar (lambda (file)
                         (let ((filename (file-name-nondirectory file))
                               (directory (file-name-directory file)))
                           (cons (format "%s File: %s  %s"
                                         (universal-launcher--get-file-icon file)
                                         filename
                                         (propertize (abbreviate-file-name directory) 'face 'font-lock-comment-face))
                                 (list 'file file))))
                       recentf-list))
             category-handlers)

    (puthash 'app
             (lambda ()
               (mapcar (lambda (app)
                         (cons (format "%s %s"
                                       (universal-launcher--get-icon 'app)
                                       (car app))
                               (list 'app (cdr app))))
                       (universal-launcher--get-applications)))
             category-handlers)

    (puthash 'flatpak
             (lambda ()
               (mapcar (lambda (app)
                         (cons (format "%s Flatpak: %s"
                                       (universal-launcher--get-icon 'flatpak)
                                       (car app))
                               (list 'app (cdr app))))
                       (universal-launcher--get-flatpak-applications)))
             category-handlers)

    (puthash 'bookmark
             (lambda ()
               (mapcar (lambda (bookmark)
                         (cons (format "%s Bookmark: %s"
                                       (universal-launcher--get-icon 'bookmark)
                                       (car bookmark))
                               (list 'bookmark (cdr bookmark))))
                       (universal-launcher--parse-org-bookmarks
                        (expand-file-name "~/org/bookmarks.org"))))
             category-handlers)

    (puthash 'firefox-action
             (lambda ()
               (mapcar (lambda (action)
                         (cons (format "%s Firefox: %s"
                                       (universal-launcher--get-icon 'firefox)
                                       (car action))
                               (list 'firefox-action (cdr action))))
                       (universal-launcher--get-firefox-actions)))
             category-handlers)

    (puthash 'command
             (lambda ()
               (mapcar (lambda (cmd)
                         (cons (format "%s Command %s"
                                       (universal-launcher--get-icon 'command)
                                       cmd)
                               (list 'command cmd)))
                       (universal-launcher--get-system-commands)))
             category-handlers)

    ;; Add emoji handler
    (puthash 'emoji
             (lambda ()
               (mapcar (lambda (emoji)
                         (cons (format "%s Emoji: %s %s"
                                       (universal-launcher--get-icon 'emoji)
                                       (car emoji)
                                       (cdr emoji))
                               (list 'emoji (cdr emoji))))
                       universal-launcher--common-emojis))
             category-handlers)

    (puthash 'calculator
             (lambda ()
               (list (cons (format "%s Calculator: Enter math expression"
                                   (universal-launcher--get-icon 'calculator))
                           (list 'calculator 'ready))))
             category-handlers)


    ;; Process categories
    (dolist (category universal-launcher--categories)
      (let* ((cat-name (plist-get category :name))
             (cat-icon (gethash cat-name universal-launcher--icon-cache))
             (types (plist-get category :types))
             (section-items '()))

        (dolist (type types)
          (when-let ((handler (gethash type category-handlers)))
            (setq section-items (append section-items (funcall handler)))))

        (when section-items
          (push (cons (format "%s  %s " cat-icon cat-name) 'separator) candidates)
          (dolist (item section-items)
            (push (cons (concat "   " (car item)) (cdr item)) candidates)))))

    (nreverse candidates)))

(defun universal-launcher--update-candidates (&optional force)
  "Update cached candidates if needed or FORCE is non-nil."
  (when (or force
            (> (- (float-time) universal-launcher--last-update)
               universal-launcher--update-interval))
    (setq universal-launcher--all-candidates (universal-launcher--grouped-candidates))
    (setq universal-launcher--last-update (float-time))))

(defun universal-launcher--get-icon (type)
  "Get cached icon for TYPE instantly."
  (gethash type universal-launcher--icon-cache ""))

(defun universal-launcher--get-running-applications ()
  "Get list of currently running applications."
  (let ((apps '()))
    (with-temp-buffer
      (when (= 0 (call-process "wmctrl" nil t nil "-l"))
        (goto-char (point-min))
        (while (re-search-forward "^\\(0x[0-9a-f]+\\)\\s-+\\S-+\\s-+\\S-+\\s-+\\(.+\\)$" nil t)
          (let ((window-id (match-string-no-properties 1))
                (app-name (match-string-no-properties 2)))
            (unless (string-match-p "\\(Desktop\\|Dock\\|Emacs\\)" app-name)
              (push (cons app-name (list window-id app-name)) apps))))))
    apps))

(defun universal-launcher--get-applications ()
  "Get list of system applications from .desktop files."
  (let ((apps '())
        (dirs '("/usr/share/applications/"
                "/usr/local/share/applications/"
                "~/.local/share/applications/"
                "/var/lib/flatpak/exports/share/applications/"
                "~/.local/share/flatpak/exports/share/applications/")))
    (dolist (dir dirs)
      (when (file-directory-p (expand-file-name dir))
        (dolist (file (directory-files (expand-file-name dir) t "\\.desktop$"))
          (with-temp-buffer
            (insert-file-contents file)
            (when (re-search-forward "^Name=\\(.+\\)$" nil t)
              (let ((name (match-string 1))
                    exec-line)
                (goto-char (point-min))
                (when (re-search-forward "^Exec=\\(.+\\)$" nil t)
                  (setq exec-line (match-string 1))
                  (push (cons name (replace-regexp-in-string "%[FfUu]" "" exec-line))
                        apps))))))))
    apps))

(defun universal-launcher--get-flatpak-applications ()
  "Get list of installed Flatpak applications."
  (let ((apps '()))
    (when (executable-find "flatpak")
      (with-temp-buffer
        ;; Try both user and system installations
        (dolist (scope '("--user" "--system"))
          (erase-buffer)
          (when (= 0 (call-process "flatpak" nil t nil "list" "--app" scope "--columns=name,application"))
            (goto-char (point-min))
            ;; Skip the header line
            (when (looking-at "Name.*Application ID")
              (forward-line 1))
            (while (not (eobp))
              (let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
                     ;; Split on multiple spaces (2 or more) to handle column alignment
                     (parts (split-string line "[ \t]\\{2,\\}" t))
                     (name (when (>= (length parts) 1) (string-trim (nth 0 parts))))
                     (app-id (when (>= (length parts) 2) (string-trim (nth 1 parts)))))
                (when (and name app-id
                           (not (string-empty-p name))
                           (not (string-empty-p app-id))
                           ;; Ensure it looks like a proper app ID
                           (string-match-p "^[a-zA-Z][a-zA-Z0-9._-]*\\.[a-zA-Z][a-zA-Z0-9._-]*" app-id))
                  (push (cons (format "%s (Flatpak)" name)
                              (concat "flatpak run " app-id))
                        apps)))
              (forward-line 1))))))
    ;; Remove duplicates (in case app appears in both user and system)
    (cl-remove-duplicates apps :test (lambda (a b) (string= (cdr a) (cdr b))))))

;; TODO Calculator Module
;; Calculator Module
(defun universal-launcher--is-calculator-input (input)
  "Check if INPUT is a math expression."
  (and (not (string-empty-p input))
       (not (string-match-p "^[[:space:]]*$" input))
       ;; Allow more mathematical symbols and functions
       (string-match-p "^[0-9+\\-*/().,^ %!sincotaqrexplog]+$" input)
       ;; Must contain at least one operator or math function
       (or (string-match-p "[+\\-*/^%]" input)
           (string-match-p "\\(sin\\|cos\\|tan\\|sqrt\\|exp\\|log\\)" input))
       ;; Must contain at least one number
       (string-match-p "[0-9]" input)))

(defun universal-launcher--calculate (expr)
  "Calculate mathematical expression EXPR using calc."
  (condition-case err
      (let* ((clean-expr (string-trim expr))
             ;; Replace common notations
             (calc-expr (replace-regexp-in-string "\\^" "**" clean-expr))
             (calc-expr (replace-regexp-in-string "×" "*" calc-expr))
             (calc-expr (replace-regexp-in-string "÷" "/" calc-expr))
             (result (calc-eval calc-expr)))
        (if (and result
                 (stringp result)
                 (not (string= result ""))
                 (not (string-match-p "\\(Error\\|Bad\\)" result))
                 ;; Accept various number formats including scientific notation
                 (or (string-match-p "^[-+]?[0-9]+\\.?[0-9]*\\(?:[eE][-+]?[0-9]+\\)?$" result)
                     (string-match-p "^[-+]?[0-9]+/[0-9]+$" result))) ; fractions
            result
          nil))
    (error nil)))

(defun universal-launcher--copy-to-clipboard (text)
  "Copy TEXT to system clipboard, handling both X11 and Wayland."
  (cond
   ;; GUI Emacs - use built-in
   ((display-graphic-p)
    (gui-set-selection 'CLIPBOARD text))
   ;; Terminal with wl-copy (Wayland)
   ((executable-find "wl-copy")
    (let ((process (start-process "wl-copy" nil "wl-copy")))
      (process-send-string process text)
      (process-send-eof process)))
   ;; Terminal with xclip (X11)
   ((executable-find "xclip")
    (let ((process (start-process "xclip" nil "xclip" "-selection" "clipboard")))
      (process-send-string process text)
      (process-send-eof process)))
   ;; Fallback
   (t
    (kill-new text)
    (message "Copied to Emacs kill ring (install wl-copy or xclip for system clipboard)"))))

;; Enhanced calculator handler for the main popup function
(defun universal-launcher--handle-calculator-input (input)
  "Handle calculator INPUT with immediate calculation."
  (let ((result (universal-launcher--calculate input)))
    (if result
        (progn
          (universal-launcher--copy-to-clipboard result)
          (message "📊 %s = %s (copied to clipboard)" input result)
          ;; If in a buffer, optionally insert the result
          (when (and universal-launcher--previous-frame
                     (frame-live-p universal-launcher--previous-frame))
            (with-selected-frame universal-launcher--previous-frame
              (when (and (not (minibufferp))
                         (not buffer-read-only)
                         (y-or-n-p "Insert result at point? "))
                (insert result)))))
      (message "❌ Invalid expression: %s" input))))

(defun universal-launcher--get-system-commands ()
  "Get system commands from PATH."
  (let ((commands '()))
    (dolist (dir (parse-colon-path (getenv "PATH")))
      (when (file-directory-p dir)
        (dolist (file (directory-files dir t))
          (when (and (file-executable-p file)
                     (not (file-directory-p file))
                     (not (backup-file-name-p file)))
            (push (file-name-nondirectory file) commands)))))
    (cl-remove-duplicates commands :test #'string=)))

(defun universal-launcher--get-firefox-actions ()
  "Get list of Firefox actions."
  (let ((actions '()))
    (when (= 0 (call-process "pgrep" nil nil nil "-x" "firefox"))
      (push (cons "Focus Firefox window" '(focus-window)) actions)
      (push (cons "Open new tab" '(new-tab)) actions)
      (let ((common-sites '(("Google" . "https://www.google.com")
                            ("GitHub" . "https://github.com")
                            ("YouTube" . "https://www.youtube.com")
                            ("Wikipedia" . "https://en.wikipedia.org"))))
        (dolist (site common-sites)
          (push (cons (concat "Open " (car site))
                      (list 'open-url (cdr site)))
                actions))))
    actions))

(defun universal-launcher--parse-org-bookmarks (file)
  "Parse bookmarks from an org FILE with support for various formats."
  (let ((bookmarks '()))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (org-mode)
        ;; Use org-element-map to parse the entire buffer
        (org-element-map (org-element-parse-buffer) 'link
          (lambda (link)
            (when (member (org-element-property :type link) '("http" "https"))
              (let* ((raw-link (org-element-property :raw-link link))
                     ;; Extract just the URL part using regex, excluding initial [ or ]
                     (url-candidate (if (string-match "^\\(https?://[^]\\[]+\\)" raw-link)
                                        (match-string 1 raw-link)
                                      raw-link))
                     ;; Remove trailing slash if present and it's not the only char after "://"
                     (url (if (and url-candidate
                                   (> (length url-candidate) (if (string-prefix-p "https" url-candidate) 8 7)) ; "https://" is 8, "http://" is 7
                                   (string-suffix-p "/" url-candidate))
                              (substring url-candidate 0 -1)
                            url-candidate))
                     (desc (or (org-element-interpret-data
                                (org-element-contents link))
                               (universal-launcher--extract-domain url))))
                (when url ; Ensure URL is not nil
                  (push (cons (if (string-empty-p desc)
                                  (universal-launcher--extract-domain url)
                                desc)
                              url)
                        bookmarks))))))
        ;; Also parse plain URLs
        (goto-char (point-min))
        ;; Regex now excludes ']', '[', space, tab, and newline from the URL part
        (while (re-search-forward "\\bhttps?://[^]\\[ \t\n]+" nil t)
          (let* ((url-candidate (match-string-no-properties 0))
                 ;; Remove trailing slash if present
                 (url (if (and url-candidate
                               (> (length url-candidate) (if (string-prefix-p "https" url-candidate) 8 7))
                               (string-suffix-p "/" url-candidate))
                          (substring url-candidate 0 -1)
                        url-candidate)))
            (when url ; Ensure URL is not nil
              (unless (rassoc url bookmarks) ; Check against the processed URL
                (push (cons (universal-launcher--extract-domain url) url)
                      bookmarks)))))))
    ;; Sort by description and remove duplicates by URL
    (cl-remove-duplicates
     (sort bookmarks (lambda (a b) (string< (car a) (car b))))
     :test (lambda (a b) (string= (cdr a) (cdr b)))
     :from-end t)))

(defun universal-launcher--extract-domain (url)
  "Extract readable domain name from URL."
  (if (string-match "https?://\\([^/]+\\)" url)
      (let ((domain (match-string 1 url)))
        (if (string-match "^www\\." domain)
            (substring domain 4)
          domain))
    url))

(defun universal-launcher--focus-running-application (app-info)
  "Focus running application using APP-INFO."
  (let ((window-id (car app-info))
        (app-name (cadr app-info)))
    (condition-case nil
        (call-process "wmctrl" nil nil nil "-i" "-a" window-id)
      (error
       (call-process "wmctrl" nil nil nil "-a" app-name)))))

(defun universal-launcher--run-application (exec-string)
  "Run application with EXEC-STRING."
  (let* ((exec-parts (split-string exec-string))
         (cmd (car exec-parts))
         (proc (apply #'start-process cmd nil exec-parts)))
    ;; Capture cmd in a closure to avoid void-variable error
    (run-with-timer 0.5 nil
                    (lambda (command-name)
                      (call-process "wmctrl" nil nil nil "-a" command-name))
                    cmd)))

(defun universal-launcher--handle-firefox-action (action)
  "Handle firefox ACTION."
  (pcase (car action)
    ('focus-window
     (call-process "wmctrl" nil nil nil "-a" "Firefox"))
    ('new-tab
     (call-process "firefox" nil nil nil "--new-tab" "about:newtab"))
    ('open-url
     (let ((url (cadr action)))
       (call-process "firefox" nil nil nil "--new-tab" url)))))

(defun universal-launcher--handle-bookmark (url)
  "Open URL in the default browser."
  (browse-url url))

(defun universal-launcher--run-command (command)
  "Run COMMAND."
  (start-process command nil command))

;; Web search function
(defcustom universal-launcher-default-search-engine "DuckDuckGo"
  "Default search engine for web searches."
  :type 'string
  :group 'universal-launcher)

(defvar universal-launcher--last-search-engine nil
  "Last used search engine.")

(defun universal-launcher--web-search (query)
  "Search the web with QUERY using default browser."
  (let* ((search-engines
          '(("Google" . "https://www.google.com/search?q=")
            ("DuckDuckGo" . "https://duckduckgo.com/?q=")
            ("Wikipedia" . "https://en.wikipedia.org/w/index.php?search=")
            ("DevDocs.io" . "https://devdocs.io/#q=")
            ("Doom discourse" . "https://discourse.doomemacs.org/search?q=")
            ("Doom issues" . "https://github.com/doomemacs/doomemacs/issues?q=")
            ("GitHub" . "https://github.com/search?q=")
            ("Google Images" . "https://www.google.com/search?tbm=isch&q=")
            ("Google Maps" . "https://www.google.com/maps/search/")
            ("Internet Archive" . "https://archive.org/search.php?query=")
            ("Kagi" . "https://kagi.com/search?q=")
            ("MDN" . "https://developer.mozilla.org/en-US/search?q=")
            ("Project Gutenberg" . "https://www.gutenberg.org/ebooks/search/?query=")
            ("Rust Docs" . "https://doc.rust-lang.org/std/?search=")
            ("SourceGraph" . "https://sourcegraph.com/search?q=")
            ("StackOverflow" . "https://stackoverflow.com/search?q=")
            ("Wolfram Alpha" . "https://www.wolframalpha.com/input/?i=")
            ("YouTube" . "https://www.youtube.com/results?search_query=")
            ("Perplexity" . "https://www.perplexity.ai/search/new?q=")
            ("Bing" . "https://www.bing.com/search?q=")))
         (default-engine (or universal-launcher--last-search-engine
                             universal-launcher-default-search-engine
                             "Google"))
         (engine (completing-read
                  (format "Search with (default %s): " default-engine)
                  (mapcar #'car search-engines)
                  nil t nil nil default-engine))
         (url-base (cdr (assoc engine search-engines)))
         (encoded-query (url-hexify-string query)))
    (setq universal-launcher--last-search-engine engine)
    (browse-url (concat url-base encoded-query))))

;; Insert emoji function
(defun universal-launcher--insert-emoji (emoji)
  "Insert EMOJI at point and copy to clipboard."
  (let ((frame universal-launcher--previous-frame))
    (when (and frame (frame-live-p frame))
      (select-frame-set-input-focus frame))
    (gui-set-selection 'CLIPBOARD emoji)
    (message "Emoji '%s' copied to clipboard" emoji)))

(defun universal-launcher-popup ()
  "World-class launcher for Emacs."
  (interactive)

  ;; Store current frame
  (setq universal-launcher--previous-frame (selected-frame))

  ;; Force update if needed
  (universal-launcher--update-candidates)

  ;; Create candidates list with nil as completion table to allow any input
  (let* ((candidates (mapcar #'car universal-launcher--all-candidates))
         (prompt "🚀 Launch (or enter math expression): ")
         (selection
          (minibuffer-with-setup-hook
              (lambda ()
                ;; Allow any input, not just candidates
                (setq-local completion-styles '(substring partial-completion basic))
                (setq-local completion-category-overrides nil))
            (completing-read prompt
                             ;; Use a function that always returns all candidates
                             ;; This allows typing anything while still showing candidates
                             (lambda (string pred action)
                               (if (eq action 'metadata)
                                   '(metadata (category . universal-launcher))
                                 (all-completions string candidates pred)))
                             nil    ; predicate
                             nil    ; require-match = nil allows any input!
                             nil    ; initial-input
                             nil    ; hist
                             nil))) ; def
         (candidate (cdr (assoc selection universal-launcher--all-candidates))))

    (cond
     ;; Empty input - do nothing
     ((string-empty-p selection) nil)

     ;; Calculator check - prioritize this before other matches
     ((universal-launcher--is-calculator-input selection)
      (universal-launcher--handle-calculator-input selection))

     ;; Separator - do nothing
     ((eq candidate 'separator) nil)

     ;; Handle matched candidates
     (candidate
      (let ((type (car candidate))
            (item (cadr candidate)))
        (pcase type
          ('buffer (switch-to-buffer item))
          ('running (universal-launcher--focus-running-application item))
          ('app (universal-launcher--run-application item))
          ('firefox-action (universal-launcher--handle-firefox-action item))
          ('bookmark (universal-launcher--handle-bookmark item))
          ('file (find-file item))
          ('command (universal-launcher--run-command item))
          ('emoji (universal-launcher--insert-emoji item))
          ('calculator (message "🧮 Type a math expression like: 2+2, sqrt(16), sin(45)")))))

     ;; Web search fallback - only if not a calculator expression
     ((and (not candidate)
           (not (string-empty-p selection))
           (not (universal-launcher--is-calculator-input selection)))
      (universal-launcher--web-search selection)))

    ;; Return to previous frame
    (when (and universal-launcher--previous-frame
               (frame-live-p universal-launcher--previous-frame))
      (select-frame-set-input-focus universal-launcher--previous-frame))))

;; Set up background update timer
(run-with-timer universal-launcher--update-interval
                universal-launcher--update-interval
                #'universal-launcher--update-candidates)

(provide 'universal-launcher)
;;; universal-launcher.el ends here
