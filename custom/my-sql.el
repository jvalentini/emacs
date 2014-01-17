;; SQL mode
(defcustom sql-user "jvalentini"
  "*Default username."
  :type 'string
  :group 'SQL)

(defcustom sql-database ""
  "*Default database."
  :type 'string
  :group 'SQL)

(defconst oracle-app-pass "d77bd3d51750e353cb02561024a842"
  "Oracle secret key")

(defvar sql-matter-history nil
  "History of matters used.")

(defvar sql-env-history nil
  "History of environments used.")

(defvar sql-prod-env-aliases '("p" "prod" "proddb" "production")
  "List of aliases that can be used to specify the production environment.")

(defvar sql-test-env-aliases '("q" "qa" "qadb" "t" "test" "testing")
  "List of aliases that can be used to specify the production environment.")

(defvar sql-dev-env-aliases '("d" "dev" "devdb" "development")
  "List of aliases that can be used to specify the production environment.")

(defvar sql-ut-env-aliases '("u" "ut")
  "List of aliases that can be used to specify the production environment.")

(defun my-sql-reconnect ()
  "Given a sql-mode buffer that has timed out, reconnect to the same matter and db."
  (interactive)
  (setq current-buffer-major-mode major-mode)
  (catch :exit-early
    (unless (string-equal current-buffer-major-mode "sql-interactive-mode")
      (message "Cannot reconnect to matter on non-sql-mode buffer.")
      (throw :exit-early nil))
    (setq sql-buffer-name (buffer-name))
    (setq sql-info (split-string sql-buffer-name "@"))
    (setq matter (pop sql-info))
    (setq sql-database (pop sql-info))
    (setq sql-oracle-options (list "@set_schema.sql" matter))
    (rename-buffer "*SQL*")
    (comint-kill-subjob)
    (sleep-for 0.6) ;; There is some race condition but fuck if I know
    (plist-put
     (cdr (assoc 'oracle sql-product-alist))
     :sqli-login
     '())
    (sql-oracle)
    (rename-buffer sql-buffer-name t)
    (plist-put
     (cdr (assoc 'oracle sql-product-alist))
     :sqli-login
     '(user name password))
    )
  )

;; (load "case-confs")

;; (setq base-db-name (replace-regexp-in-string "p00\\([0-9]+[a-z]*\\)app\n" "\\1" (shell-command-to-string (concat "~/scripts/sh/get_db.sh " (downcase my-sql-matter)))))
;; (shell-command-to-string (concat "~/scripts/sh/get_db.sh " (downcase "jpmc135")))
;; (shell-command-to-string (concat "~/scripts/sh/get_db.sh " (downcase "jpmc16")))
;; (replace-regexp-in-string "p0\\([0-9]+[a-z]*\\)app\n" "\\1" (shell-command-to-string (concat "~/scripts/sh/get_db.sh " (downcase "jpmc135"))))
;; (replace-regexp-in-string "p0\\([0-9]+[a-z]*\\)app\n" "\\1" (shell-command-to-string (concat "~/scripts/sh/get_db.sh " (downcase "jpmc16"))))
;; (replace-regexp-in-string "0\\([0-9]+[a-z]*\\)" "\\1" "10")

(setq my-secret-data (json-read-file (concat user-emacs-directory "custom/secret.json")))

(defun my-sql-connect ()
  "Given a matter and an environment (dev, qa, prod, ut), find the correct database to connect to."
  (interactive)
  (setq my-sql-matter (read-from-minibuffer "Matter: " nil nil nil '(sql-matter-history . 1)))
  (setq base-db-name (replace-regexp-in-string "p0\\([0-9]+[a-z]*\\)app\n" "\\1" (shell-command-to-string (concat "~/scripts/sh/get_db.sh " (downcase my-sql-matter)))))
  (let* ((env (read-from-minibuffer "Environment (d, q, p, u): " nil nil nil '(sql-env-history . 1)))
         (db-name (concat (cond ((member env sql-prod-env-aliases) (concat "p0" base-db-name "dev"))
                                ((member env sql-dev-env-aliases) (concat "devdb" (replace-regexp-in-string "0\\([0-9]+[a-z]*\\)" "\\1" base-db-name)))
                                ((member env sql-test-env-aliases) (concat "qadb" (replace-regexp-in-string "0\\([0-9]+[a-z]*\\)" "\\1" base-db-name)))
                                ((member env sql-ut-env-aliases) "ut")) ".amicillc.com"))
         (sql-user "jvalentini")
         (sql-password (cdr (assoc 'oracle-dev-pw my-secret-data))))
    (setq sql-database db-name)
    (setq sql-oracle-options (list "@set_schema.sql" (upcase my-sql-matter)))
    (let ((sql-buffer-name (concat sql-user "@" (downcase my-sql-matter) "@" sql-database)))
      (if (get-buffer sql-buffer-name)
          (switch-to-buffer sql-buffer-name)
        (sql-oracle)
        (switch-to-buffer "*SQL*")
        (rename-buffer sql-buffer-name t)))))

(defun my-sql-app-connect ()
  "Given an app_username, a matter, and an environment (dev, qa, prod, ut), find the correct database to connect to."
  (interactive)
  (setq my-sql-matter (read-from-minibuffer "Matter: " nil nil nil '(sql-matter-history . 1)))
  (setq base-db-name (replace-regexp-in-string "p0\\([0-9]+[a-z]*\\)app\n" "\\1" (shell-command-to-string (concat "~/scripts/sh/get_db.sh " (downcase my-sql-matter)))))
  (let* ((env (read-from-minibuffer "Environment (d, q, p, u): " nil nil nil '(sql-env-history . 1)))
         (db-name (concat (cond ((member env sql-prod-env-aliases) (concat "p0" base-db-name "dev"))
                                ((member env sql-dev-env-aliases) (concat "devdb" (replace-regexp-in-string "0\\([0-9]+[a-z]*\\)" "\\1" base-db-name)))
                                ((member env sql-test-env-aliases) (concat "qadb" (replace-regexp-in-string "0\\([0-9]+[a-z]*\\)" "\\1" base-db-name)))
                                ((member env sql-ut-env-aliases) "ut")) ".amicillc.com"))
         (sql-user "jvalentini_amicillccom")
         (sql-password (cdr (assoc 'oracle-app-pw my-secret-data))))
    (setq sql-database db-name)
    (setq sql-oracle-options (list "@set_app_schema.sql" (upcase my-sql-matter)))
    (let ((sql-buffer-name (concat sql-user "@" (downcase my-sql-matter) "@" sql-database)))
      (if (get-buffer sql-buffer-name)
          (switch-to-buffer sql-buffer-name)
        (sql-oracle)
        (switch-to-buffer "*SQL*")
        (rename-buffer sql-buffer-name t)))))

(add-hook 'sql-mode-hook 'font-lock-mode)
(add-hook 'sql-mode-hook
          (lambda () (setq fill-column 128)))

(eval-after-load "sql"
  (load-library "sql-indent"))
