;;; ../../dotfiles/doom/.config/doom/lisp/weather.el -*- lexical-binding: t; -*-

(defun my/weather (&optional city)
  "Display weather forecast using wttr.in in a vterm buffer.
If CITY is nil, uses IP-based location detection."
  (interactive "P")
  (require 'vterm)
  (let* ((use-location (not city))
         (city-input (if use-location ""
                       (read-string "Enter city name: ")))
         (buffer-name "*Weather*")
         (command (concat "curl wttr.in/" (url-hexify-string city-input))))

    ;; Create or switch to the weather buffer
    (if (get-buffer buffer-name)
        (switch-to-buffer buffer-name)
      (vterm buffer-name))

    ;; Clear the buffer and run the curl command
    (vterm-send-string "clear")
    (vterm-send-return)
    (vterm-send-string command)
    (vterm-send-return)))
