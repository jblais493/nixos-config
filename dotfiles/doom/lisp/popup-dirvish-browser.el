;;; ../../dotfiles/doom/.config/doom/lisp/popup-dirvish-browser.el -*- lexical-binding: t; -*-

(defun popup-dirvish-browser ()
  "Create a new frame with Dirvish browser starting in home directory."
  (interactive)
  ;; Create the frame
  (let ((frame (make-frame '((name . "Dirvish Browser")
                             (width . 100)
                             (height . 35)
                             (minibuffer . t)
                             (vertical-scroll-bars . nil)
                             (menu-bar-lines . 0)
                             (tool-bar-lines . 0)))))

    ;; Select the frame and open Dirvish
    (select-frame frame)
    (dirvish "~/")

    ;; Add convenient keybindings
    (with-selected-frame frame
      (let ((map (copy-keymap dirvish-mode-map)))
        (define-key map (kbd "q") 'delete-frame)
        (define-key map (kbd "C-g") 'delete-frame)
        (use-local-map map)))

    ;; Use external window manager tools to center the frame
    (run-with-timer
     0.3 nil
     (lambda ()
       (when (frame-live-p frame)
         ;; Try different centering methods based on available tools
         (cond
          ;; First try wmctrl which works well with GNOME
          ((executable-find "wmctrl")
           (shell-command "wmctrl -r 'Dirvish Browser' -e 0,-1,-1,-1,-1"))

          ;; Fall back to xdotool if available
          ((executable-find "xdotool")
           (shell-command
            "xdotool search --name 'Dirvish Browser' windowmove $(xdotool getdisplaygeometry | awk '{print $1/2-500, $2/2-300}')"))))))

    ;; Add helpful message
    (message "Dirvish browser ready. Navigate with normal commands. Press 'N' for Nautilus, 'q' to close.")))
