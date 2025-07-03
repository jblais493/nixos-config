;;; ../../Dotfiles/doom/.config/doom/lisp/popup-scratch.el -*- lexical-binding: t; -*-

(defun popup-scratch-for-web ()
  "Create a popup frame with an org buffer for web text editing.
Designed specifically for GNOME Wayland with Doom Emacs spell checking.
Called via Ctrl+Shift+n in GNOME shell"
  (interactive)
  (let* ((buffer-name "*web-compose*")
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (erase-buffer)
      ;; Use org mode
      (org-mode)
      ;; For Doom Emacs, enable appropriate spell checking mode
      (when (fboundp 'spell-checking-enable)
        (spell-checking-enable))
      ;; Add header with instructions
      (insert "#+TITLE: Web Composition\n")
      (insert "#+AUTHOR: Emacs User\n\n")
      (insert "* Instructions\n")
      (insert "- Press C-c C-c when done to copy text and close\n")
      (insert "- Use z= on misspelled words to see correction suggestions\n")
      (insert "- Text will be copied to clipboard for pasting\n\n")
      (insert "* Your Text\n\n")

      ;; Define our finish function
      (fset 'web-compose-finish
            (lambda ()
              (interactive)
              ;; Extract content (just the "Your Text" section)
              (let ((content (save-excursion
                               (let ((start nil) (end nil))
                                 ;; Find the Your Text heading
                                 (goto-char (point-min))
                                 (re-search-forward "^\\* Your Text$" nil t)
                                 (forward-line 1)
                                 ;; Skip any blank lines
                                 (while (and (not (eobp))
                                             (looking-at "^$"))
                                   (forward-line 1))
                                 (setq start (point))
                                 ;; Take everything until the end of buffer
                                 (goto-char (point-max))
                                 (setq end (point))
                                 ;; Extract the content
                                 (buffer-substring-no-properties start end)))))
                ;; Trim whitespace
                (setq content (string-trim content))
                ;; Copy to Emacs clipboard
                (kill-new content)
                ;; For Wayland - use wl-copy which is the most compatible
                (when (executable-find "wl-copy")
                  (call-process "wl-copy" nil nil nil content))
                ;; Message user about next steps
                (message "Text copied to clipboard! Ready to paste with Ctrl+V")
                ;; Store frame to close
                (let ((frame-to-close (selected-frame)))
                  ;; Close frame after a short delay
                  (run-with-timer 0.5 nil
                                  (lambda ()
                                    (delete-frame frame-to-close)))))))

      ;; Bind our function to the local map
      (local-set-key (kbd "C-c C-c") 'web-compose-finish)
      (local-set-key (kbd "C-c C-k") (lambda ()
                                       (interactive)
                                       (let ((frame-to-close (selected-frame)))
                                         (run-with-timer 0.1 nil
                                                         (lambda ()
                                                           (delete-frame frame-to-close))))))

      ;; Set up mode line to indicate Doom/Evil spell check is available
      (setq mode-line-format
            (list "-- WEB COMPOSE (ORG MODE) -- Use z= for spelling -- C-c C-c when done ")))

    ;; Create the frame
    (let ((frame (make-frame `((name . "Web Compose")
                               (width . 80)
                               (height . 30)
                               (minibuffer . nil)
                               (vertical-scroll-bars . nil)
                               (menu-bar-lines . 0)
                               (tool-bar-lines . 0)))))
      ;; Set up the frame
      (select-frame frame)
      (switch-to-buffer buffer-name)

      ;; Try to insert text from clipboard/selection
      (save-excursion
        (goto-char (point-min))
        (search-forward "* Your Text")
        (forward-line 2)
        (let ((start-pos (point))
              (selection-text nil))

          ;; Try to get text from primary selection
          (when-let ((selection (gui-get-selection 'PRIMARY 'UTF8_STRING)))
            (when (not (string-empty-p selection))
              (setq selection-text selection)))

          ;; If nothing from primary selection, try clipboard
          (when (and (not selection-text) (eq start-pos (point)))
            (when-let ((clipboard (gui-get-selection 'CLIPBOARD 'UTF8_STRING)))
              (when (not (string-empty-p clipboard))
                (setq selection-text clipboard))))

          ;; If still nothing, try wl-paste for Wayland
          (when (and (not selection-text) (eq start-pos (point)) (executable-find "wl-paste"))
            (with-temp-buffer
              (when (= 0 (call-process "wl-paste" nil t nil))
                (setq selection-text (buffer-string)))))

          ;; Insert the text, preserving src blocks
          (when selection-text
            (insert selection-text))))

      ;; Position cursor after instructions
      (goto-char (point-min))
      (search-forward "* Your Text")
      (forward-line 2)

      ;; Try different centering methods for Wayland
      (run-with-timer
       0.3 nil
       (lambda ()
         (when (frame-live-p frame)
           (cond
            ;; For sway
            ((executable-find "swaymsg")
             (shell-command "swaymsg '[title=\"Web Compose\"]' move position center"))
            ;; For GNOME/KDE Wayland using wmctrl (if it works with Wayland)
            ((executable-find "wmctrl")
             (shell-command "wmctrl -r 'Web Compose' -e 0,-1,-1,-1,-1"))
            ;; For Wayland using wtype
            ((executable-find "wtype")
             ;; This is less reliable but might work in some cases
             (shell-command "wtype -M alt -P space -m alt -p space"))))))

      ;; Use transient map for special first-key behavior
      (let ((keymap (make-sparse-keymap)))
        (define-key keymap (kbd "DEL") (lambda ()
                                         (interactive)
                                         (delete-region (point-min) (point-max))
                                         (goto-char (point-min))
                                         (insert "#+TITLE: Web Composition\n")
                                         (insert "#+AUTHOR: Emacs User\n\n")
                                         (insert "* Instructions\n")
                                         (insert "- Press C-c C-c when done to copy text and close\n")
                                         (insert "- Use z= on misspelled words to see correction suggestions\n")
                                         (insert "- Text will be copied to clipboard for pasting\n\n")
                                         (insert "* Your Text\n\n")
                                         (search-forward "* Your Text")
                                         (forward-line 2)))
        (define-key keymap (kbd "C-SPC") (lambda ()
                                           (interactive)
                                           (delete-region (point-min) (point-max))
                                           (goto-char (point-min))
                                           (insert "#+TITLE: Web Composition\n")
                                           (insert "#+AUTHOR: Emacs User\n\n")
                                           (insert "* Instructions\n")
                                           (insert "- Press C-c C-c when done to copy text and close\n")
                                           (insert "- Use z= on misspelled words to see correction suggestions\n")
                                           (insert "- Text will be copied to clipboard for pasting\n\n")
                                           (insert "* Your Text\n\n")
                                           (search-forward "* Your Text")
                                           (forward-line 2)))
        (set-transient-map keymap))

      ;; Enable org-tempo for easy block insertion if available
      (when (fboundp 'org-tempo-setup)
        (org-tempo-setup))

      ;; Message about usage
      (message "Type your text and press C-c C-c when done (C-c C-k to cancel)"))))
