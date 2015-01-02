;; SQL mode
(defcustom sql-user "jvalentini"
  "*Default username."
  :type 'string
  :group 'SQL)

(defcustom sql-database ""
  "*Default database."
  :type 'string
  :group 'SQL)

(defvar sql-matter-history nil
  "History of matters used.")

(defvar sql-env-history nil
  "History of environments used.")

(defvar sql-env-map '(("d" . "development") ("q" . "qa") ("p" . "production"))
  "Map of shorthand env key to full name")

(defvar sql-prod-env-aliases '("p" "prod" "proddb" "production")
  "List of aliases that can be used to specify the production environment.")

(defvar sql-test-env-aliases '("q" "qa" "qadb" "t" "test" "testing")
  "List of aliases that can be used to specify the qa environment.")

(defvar sql-dev-env-aliases '("d" "dev" "devdb" "development")
  "List of aliases that can be used to specify the dev environment.")

(defvar sql-ut-env-aliases '("u" "ut")
  "List of aliases that can be used to specify the unit test environment.")

(defvar sql-vm-env-aliases '("v" "vm")
  "List of aliases that can be used to specify the vagrant environment.")

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

(setq my-secret-data (json-read-file (concat user-emacs-directory "custom/secret.json")))

(require 'request)

(defun my-sql-matter-to-tns (matter env)
  (cond ((member env sql-ut-env-aliases)
         (setq sql-database "ut.amicillc.com"))
        ((member env sql-vm-env-aliases)
         (setq sql-database "13.242.230.43/omnix"
               sql-user "system"
               sql-password "Welcome01"))
        (t
         (request
          (concat "http://infrastructure.amicillc.com/matter_schema?matter=" matter "&environment=" (cdr (assoc env sql-env-map)))
          :parser 'json-read
          :success (function*
                    (lambda (&key data &allow-other-keys)
                      (setq sql-database (cdr (assoc 'tns (assoc 'database (elt data 0)))))))
          :sync t
          :timeout 2))))

(defun my-sql-connect (user password)
  "Given a matter and an environment (dev, qa, prod, ut), find the correct database to connect to."
  (interactive)
  (setq my-sql-matter (read-from-minibuffer "Matter: " nil nil nil '(sql-matter-history . 1)))
  (let* ((env (read-from-minibuffer "Environment (d, q, p, u, v): " nil nil nil '(sql-env-history . 1)))
         (sql-user user)
         (sql-password password))
    (my-sql-matter-to-tns my-sql-matter env)
    (setq sql-oracle-options (list "@set_schema.sql" (upcase my-sql-matter)))
    (let ((sql-buffer-name (concat sql-user "@" (downcase my-sql-matter) "@" sql-database)))
      (if (get-buffer sql-buffer-name)
          (switch-to-buffer sql-buffer-name)
        (with-current-buffer "*scratch*"
          (sql-oracle)
          (switch-to-buffer "*SQL*")
          (rename-buffer sql-buffer-name t))))))

(defun my-sql-dev-connect ()
  "Connect to a matter/schema with my dev account"
  (interactive)
  (let ((user "jvalentini")
        (password (cdr (assoc 'oracle-dev-pw my-secret-data))))
      (my-sql-connect user password)))

(defun my-sql-app-connect ()
  "Connect to a matter/schema with my app account"
  (interactive)
  (let ((user "jvalentini_amicillccom")
        (password (cdr (assoc 'oracle-app-pw my-secret-data))))
      (my-sql-connect user password)))

(add-hook 'sql-mode-hook 'font-lock-mode)
(add-hook 'sql-mode-hook
          (lambda () (setq fill-column 128)))

(eval-after-load "sql"
  (load-library "sql-indent"))
