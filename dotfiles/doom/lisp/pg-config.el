;;; ../../nixos-config/dotfiles/doom/lisp/pg-config.el -*- lexical-binding: t; -*-

;; Setup development SQL database
(setq sql-connection-alist
      '((dev-postgres
         (sql-product 'postgres)
         (sql-server "localhost")
         (sql-user "postgres")
         (sql-password "postgres")
         (sql-database "devdb")
         (sql-port 5432))))

;; Configure org-babel SQL connection parameters
(setq org-babel-default-header-args:sql
      '((:engine . "postgresql")
        (:dbhost . "localhost")
        (:dbuser . "postgres")
        (:dbpassword . "postgres")
        (:database . "devdb")))

;; Ensure we have org-babel SQL support
(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((sql . t))))

;; PGmacs setup
(use-package pgmacs
  :after pg
  :defer t
  :commands (pgmacs pgmacs-open-string pgmacs-open-uri)
  :config
  ;; Define a function to quickly connect to your development database
  (defun my-pgmacs-connect ()
    "Connect to the development database using PGmacs."
    (interactive)
    (pgmacs-open-string "user=postgres password=postgres dbname=devdb host=localhost port=5432"))

  ;; Set PGmacs customization options
  (setq pgmacs-default-display-limit 100)  ;; Default number of rows to show
  (setq pgmacs-widget-use-proportional-font nil))  ;; Use fixed-width font in widgets

;; Modified function to use existing SQL connection when available
(defun pg-query-to-orgtable (query &optional buffer-name)
  "Execute PostgreSQL QUERY and insert results as an Org table."
  (interactive "sSQL Query: \nsBuffer name (default *SQL Results*): ")
  (let ((buffer (get-buffer-create (or buffer-name "*SQL Results*"))))
    ;; Check if we have an active SQL connection
    (if (and (boundp 'sql-buffer) (buffer-live-p sql-buffer))
        ;; Use the SQL buffer method if we have a connection
        (progn
          (with-current-buffer buffer
            (erase-buffer)
            (org-mode)
            (insert "#+TITLE: SQL Query Results\n")
            (insert "#+DATE: " (format-time-string "%Y-%m-%d") "\n\n")
            (insert "#+BEGIN_SRC sql\n")
            (insert query "\n")
            (insert "#+END_SRC\n\n"))

          ;; Format the SQL output for better parsing
          (sql-send-string "\\a")  ;; Unaligned mode
          (sql-send-string "\\t")  ;; Tuples only
          (sql-send-string "\\f '|'")  ;; Field separator
          (sit-for 0.3)

          ;; Execute the query
          (sql-send-string query)
          (sit-for 1.0)

          ;; Add a marker to find the end of results
          (sql-send-string "SELECT '---RESULT-END---';")
          (sit-for 0.5)

          ;; Parse results from SQL buffer
          (with-current-buffer sql-buffer
            (save-excursion
              (goto-char (point-max))
              (when (search-backward "---RESULT-END---" nil t)
                (let ((end-pos (match-beginning 0)))
                  (search-backward query nil t)
                  (forward-line 1)
                  (let ((result-text (buffer-substring-no-properties (point) end-pos)))
                    (with-current-buffer buffer
                      (goto-char (point-max))
                      (let ((lines (split-string result-text "\n" t)))
                        (dolist (line lines)
                          (unless (string-match-p "^\\(devdb\\|Output\\|Tuples\\|Field\\)" line)
                            (unless (string-equal "" (string-trim line))
                              (insert "| ")
                              (insert (mapconcat 'identity
                                                 (split-string line "|")
                                                 " | "))
                              (insert " |\n"))))
                        (when (search-backward "|" nil t)
                          (org-table-align)))))))))

          ;; Reset SQL formatting
          (sql-send-string "\\a")
          (sql-send-string "\\t"))

      ;; Otherwise use org-babel with explicit connection parameters
      (with-current-buffer buffer
        (erase-buffer)
        (org-mode)
        (insert "#+TITLE: SQL Query Results\n")
        (insert "#+DATE: " (format-time-string "%Y-%m-%d") "\n\n")
        (insert "#+begin_src sql :engine postgresql :dbhost localhost :dbuser postgres :dbpassword postgres :database devdb :exports both\n")
        (insert query)
        (insert "\n#+end_src\n\n")
        (goto-char (point-min))
        (search-forward "#+begin_src")
        (forward-line 1)
        (org-babel-execute-src-block)))

    (switch-to-buffer buffer)
    (goto-char (point-min))))

;; Bridge function to export PGmacs data to Org documents
(defun my-pg-export-table-to-org (table-name)
  "Export a table from database to an Org document with query results."
  (interactive "sTable name: ")
  (pg-query-to-orgtable (format "SELECT * FROM %s LIMIT 100;" table-name)))

;; All our existing functions kept for backward compatibility
(defun pg-table-to-orgtable (table-name &optional limit-rows where-clause)
  "Select data from TABLE-NAME and display as an Org table.
Optionally limit results with LIMIT-ROWS and/or filter with WHERE-CLAUSE."
  (interactive
   (list (read-string "Table name: ")
         (read-string "Limit rows (default 100): " nil nil "100")
         (read-string "WHERE clause (optional): ")))
  (let ((query (format "SELECT * FROM %s%s%s"
                       table-name
                       (if (and where-clause (not (string-empty-p where-clause)))
                           (format " WHERE %s" where-clause)
                         "")
                       (if (and limit-rows (not (string-empty-p limit-rows)))
                           (format " LIMIT %s" limit-rows)
                         ""))))
    (pg-query-to-orgtable query (format "*Table: %s*" table-name))))

(defun pg-browse-table (table-name)
  "Browse a PostgreSQL table in Org mode."
  (interactive "sTable name: ")
  (pg-table-to-orgtable table-name))

(defun pg-list-tables ()
  "List tables in the PostgreSQL database and make them clickable."
  (interactive)
  (if (and (boundp 'sql-buffer) (buffer-live-p sql-buffer))
      (let ((buf (get-buffer-create "*PG Tables*")))
        (with-current-buffer buf
          (erase-buffer)
          (org-mode)
          (insert "#+TITLE: PostgreSQL Tables\n\n")

          ;; Send command to list tables
          (sql-send-string "\\dt")
          (sit-for 0.5)

          ;; Capture the results
          (with-current-buffer sql-buffer
            (let ((tables-text (buffer-substring-no-properties
                                (save-excursion
                                  (goto-char (point-max))
                                  (forward-line -15)
                                  (point))
                                (point-max))))
              (with-current-buffer buf
                (insert "| Schema | Table | Action |\n")
                (insert "|--------+-------+--------|\n")
                ;; Parse the table list
                (let ((lines (split-string tables-text "\n" t)))
                  (dolist (line lines)
                    (when (string-match "^ *\\([^ |]*\\) *| *\\([^ |]*\\)" line)
                      (let ((schema (match-string 1 line))
                            (table (match-string 2 line)))
                        (unless (or (string= schema "Schema")
                                    (string-match-p "^--" schema)
                                    (string-match-p "^(" schema))
                          (insert (format "| %s | %s | [[elisp:(pg-browse-table \"%s\")][Browse]] | [[elisp:(my-pg-export-table-to-org \"%s\")][Export]] | [[elisp:(pgmacs-display-table \"%s\")][PGmacs]] |\n"
                                          schema table table table table))))))))))
          (org-table-align))
        (switch-to-buffer buf))
    ;; Use org-babel if no SQL connection
    (let ((buf (get-buffer-create "*PG Tables*")))
      (with-current-buffer buf
        (erase-buffer)
        (org-mode)
        (insert "#+TITLE: PostgreSQL Tables\n\n")
        (insert "#+begin_src sql :engine postgresql :dbhost localhost :dbuser postgres :dbpassword postgres :database devdb :exports both\n")
        (insert "SELECT table_schema, table_name FROM information_schema.tables WHERE table_schema='public' ORDER BY table_name;\n")
        (insert "#+end_src\n\n")
        (goto-char (point-min))
        (search-forward "#+begin_src")
        (forward-line 1)
        (org-babel-execute-src-block)

        ;; Create links for each table - with additional options
        (when (search-forward "#+RESULTS:" nil t)
          (forward-line 1)
          (let ((start (point)))
            (forward-line)  ;; Skip header row
            (while (and (not (eobp)) (looking-at "^| "))
              (when (looking-at "| *\\([^ |]+\\) *| *\\([^ |]+\\) *|")
                (let ((schema (match-string-no-properties 1))
                      (table (match-string-no-properties 2)))
                  (delete-region (line-beginning-position) (line-end-position))
                  (insert (format "| %s | %s | [[elisp:(pg-browse-table \"%s\")][Browse]] | [[elisp:(my-pg-export-table-to-org \"%s\")][Export]] | [[elisp:(pgmacs-display-table \"%s\")][PGmacs]] |"
                                  schema table table table table))))
              (forward-line 1))
            (org-table-align))))
      (switch-to-buffer buf))))

(defun pg-describe-table (table-name)
  "Show detailed information about a table structure."
  (interactive "sTable name: ")
  (let ((buf (get-buffer-create (format "*Table Structure: %s*" table-name))))
    (with-current-buffer buf
      (erase-buffer)
      (org-mode)
      (insert (format "#+TITLE: Table Structure: %s\n\n" table-name))

      ;; Column information
      (insert "* Columns\n\n")
      (let ((query (format "SELECT column_name, data_type, is_nullable, column_default
FROM information_schema.columns
WHERE table_name = '%s'
ORDER BY ordinal_position;" table-name)))
        (pg-query-to-orgtable query))

      ;; Constraints
      (insert "\n* Constraints\n\n")
      (let ((query (format "SELECT c.conname AS constraint_name,
       CASE c.contype
         WHEN 'c' THEN 'check'
         WHEN 'f' THEN 'foreign_key'
         WHEN 'p' THEN 'primary_key'
         WHEN 'u' THEN 'unique'
       END AS constraint_type,
       pg_get_constraintdef(c.oid) AS constraint_definition
FROM pg_constraint c
JOIN pg_namespace n ON n.oid = c.connamespace
JOIN pg_class t ON t.oid = c.conrelid
WHERE t.relname = '%s'
  AND n.nspname = 'public';" table-name)))
        (pg-query-to-orgtable query))

      ;; Indexes
      (insert "\n* Indexes\n\n")
      (let ((query (format "SELECT indexname, indexdef
FROM pg_indexes
WHERE tablename = '%s';" table-name)))
        (pg-query-to-orgtable query)))
    (switch-to-buffer buf)))

(defun pg-sample-data (table-name)
  "Show sample data from a table with ability to filter."
  (interactive "sTable name: ")
  (let* ((where (read-string "WHERE clause (optional): "))
         (limit (read-string "Limit (default 10): " nil nil "10"))
         (query (format "SELECT * FROM %s%s LIMIT %s;"
                        table-name
                        (if (string-empty-p where) "" (format " WHERE %s" where))
                        limit)))
    (pg-query-to-orgtable query (format "*Sample: %s*" table-name))))

(defun pg-execute-buffer-query ()
  "Execute the current SQL buffer as a query and show results."
  (interactive)
  (pg-query-to-orgtable (buffer-string)))

(defun pg-execute-statement-at-point ()
  "Execute the SQL statement at point."
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'paragraph))
         (statement (buffer-substring-no-properties (car bounds) (cdr bounds))))
    (pg-query-to-orgtable statement)))

(defun pg-connect ()
  "Connect to PostgreSQL database."
  (interactive)
  (sql-connect 'dev-postgres))

;; Key bindings for SQL mode
(with-eval-after-load 'sql
  (define-key sql-mode-map (kbd "C-c C-c") 'pg-execute-buffer-query)
  (define-key sql-mode-map (kbd "C-c C-r") 'pg-execute-statement-at-point)
  (define-key sql-mode-map (kbd "C-c t") 'pg-list-tables)
  (define-key sql-mode-map (kbd "C-c d") 'pg-describe-table))

;; Global key bindings for database operations
(map! :leader
      (:prefix-map ("e" . "custom")
                   (:prefix ("d" . "database")
                    :desc "Connect to PGmacs" "c" #'my-pgmacs-connect
                    :desc "Open PGmacs" "p" #'pgmacs
                    :desc "List tables" "t" #'pg-list-tables
                    :desc "Connect to SQL" "s" #'pg-connect
                    :desc "Execute SQL query" "q" #'pg-query-to-orgtable)))
