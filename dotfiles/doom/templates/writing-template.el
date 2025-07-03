(define-skeleton markdown-blog-template
  "Insert blog post frontmatter"
  nil
  "---\n"
  "title: '" (file-name-base (buffer-name)) "'\n"
  "featuredImage: ''\n"
  "date: '" (format-time-string "%Y-%m-%dT%H:%M:%S.000Z") "'\n"
  "draft: false\n"
  "tags: [\"\"]\n"
  "showTableOfContents: true\n"
  "---\n")

(map! :leader
      :desc "Insert blog template"
      "i t" #'markdown-blog-template)
