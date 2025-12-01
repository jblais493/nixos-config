;;; ../../nixos-config/dotfiles/doom/lisp/gimp-tweet.el -*- lexical-binding: t; -*-

(defun jb/gimp-tweet ()
  "Open GIMP with tweet template for Instagram editing."
  (interactive)
  (let ((template "/home/joshua/Documents/Personal/Socials/tweet.xcf"))
    (if (file-exists-p template)
        (start-process "gimp" nil "gimp" template)
      (error "Template file not found: %s" template))))

(map! :leader
      :desc "Open GIMP tweet template" "o g" #'jb/gimp-tweet)
