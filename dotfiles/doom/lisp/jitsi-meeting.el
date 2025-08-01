;;; ../../nixos-config/dotfiles/doom/lisp/jitsi-meeting.el -*- lexical-binding: t; -*-

;; define jitsi url
(defvar jitsi-base-url "https://meet.jit.si/"
  "link to jitsi meeting base url")

;; Create a random string with the date, month, year, and 10 random characters
(defun my/jitsi-link (date)
  ()
  )


;; Output it to the buffer


;; Map keys for jitsi link creation
(map! :leader
      (:prefix ("j" . "jitsi")
       :desc "Create jitsi link" "c" #'my/record-audio))
