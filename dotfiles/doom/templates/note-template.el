;;; templates/note-template.el -*- lexical-binding: t; -*-


(define-skeleton markdown-note-template
  "Insert blog post frontmatter"
  nil
  "---\n"
  "title: '" (file-name-base (buffer-name)) "'\n"
  "date: '" (format-time-string "%Y-%m-%dT%H:%M:%S.000Z") "'\n"
  "hubs: [\"\"]\n"
  "urls: - \n"
  "---\n")

(map! :leader
      :desc "Insert note template"
      "i n" #'markdown-note-template)
