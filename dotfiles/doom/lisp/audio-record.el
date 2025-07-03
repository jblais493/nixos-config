;;; ../../dotfiles/doom/.config/doom/lisp/audio-record.el -*- lexical-binding: t; -*-

(defvar my/audio-recordings-directory "~/recordings"
  "Directory to store audio recordings.")

(defun my/record-audio (filename)
  "Record audio from default sources and save to FILENAME in mono format."
  (interactive "sFilename (without extension): ")
  ;; Expand the directory path fully
  (let* ((expanded-dir (expand-file-name my/audio-recordings-directory))
         (full-path (expand-file-name (concat filename ".wav") expanded-dir)))

    ;; Create directory if it doesn't exist
    (unless (file-exists-p expanded-dir)
      (make-directory expanded-dir t)
      (message "Created directory: %s" expanded-dir))

    ;; Check if directory is writable
    (unless (file-writable-p expanded-dir)
      (error "Directory %s is not writable" expanded-dir))

    (message "Recording audio to %s... Press C-g to stop" full-path)

    ;; Use specific commands based on OS, with mono recording
    (if (eq system-type 'darwin)
        ;; macOS (using afrecord)
        (async-shell-command
         (format "afrecord -c 1 -f WAVE '%s'" full-path)
         "*Audio Recording*")
      ;; Linux (using arecord) - specify mono with -c 1
      (async-shell-command
       (format "arecord -f cd -c 1 -t wav '%s'" full-path)
       "*Audio Recording*"))

    (with-current-buffer "*Audio Recording*"
      (setq header-line-format
            (format "Recording to %s. Press C-g in this buffer to stop." full-path)))))

(defun my/stop-audio-recording ()
  "Stop the current audio recording process."
  (interactive)
  (when (get-buffer "*Audio Recording*")
    (let ((proc (get-buffer-process "*Audio Recording*")))
      (when proc
        (interrupt-process proc)
        (message "Recording stopped")))))

;; Bind keys (optional)
(map! :leader
      (:prefix ("R" . "recordings")
       :desc "Start recording" "r" #'my/record-audio
       :desc "Stop recording" "s" #'my/stop-audio-recording))
