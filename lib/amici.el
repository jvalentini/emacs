(defvar amici-php-command
  "/home/dev/bin/php"
  "PHP executable to use when executing php scripts")

(defvar amici-framework-version-alist
  '()
  "alist of cons cells (REGEXP . FRAMEWORK-DIR) where FRAMEWORK-DIR is the
 framework directory to use for application directories matching REGEXP")

(defvar amici-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<f5>") 'amici-toggle-class-test)
    (define-key map (kbd "<f6>") 'amici-test-run)
    (define-key map (kbd "<f7>") 'amici-run-test-at-point)
    (define-key map (kbd "C-x a f") 'amici-fixture-load)
    (define-key map (kbd "C-x a d") 'amici-fixture-teardown)
    map))


;; --- util functions ----------------------------------------------------------

(defun amici-framework-dir ()
  "Return the framework directory to use for the current buffer.
 Search amici-framework-version-alist for a regexp matching buffer-file-name.
 If no match is found, return the value of the AMICI_FRAMEWORK_DIR environment
 variable."
  (let ((alist amici-framework-version-alist)
        (framework-dir nil))
    (while (and alist (not framework-dir))
      (progn
        (setq regexp (caar alist)
              dir    (cdar alist)
              alist  (cdr alist))
        (if (string-match regexp buffer-file-name)
            (setq framework-dir dir))))
    (if framework-dir
        framework-dir
      (getenv "AMICI_FRAMEWORK_DIR"))))

(defun amici-root ()
  "Return the directory that is the root of the current branch else return nil"
  (let ((current-dir default-directory)
        (max 10)
        (found nil))
    (while (and (not found) (> max 0))
      (if (file-exists-p (concat current-dir "config/environment.php"))
          (setq found t)
        (setq current-dir (concat current-dir "../")
              max (- max 1))))
    (if found
        (if (amici-is-framework current-dir)
            (expand-file-name (concat current-dir "../"))
          (expand-file-name current-dir)))))

(defun amici-is-framework (filename)
  "Does FILENAME exist under a web framework branch? Very naive."
  (if (string-match ".*/web_framework/.*" filename) t nil))

(defun amici-relative-path (filename)
  "Return the path of FILENAME relative to the branch root"
  (replace-regexp-in-string (amici-root) "" filename))

(defun amici-absolute-path (filename)
  "Return the absolute path of the relative path FILENAME"
  (concat (amici-root) filename))

(defun amici-class-by-file (filename)
  "Return the class defined in FILENAME"
  (replace-regexp-in-string "\.php$" "" (file-name-nondirectory filename)))

(defun amici-fixture-by-file (filename)
  "Return the fixture defined in FILENAME"
  (replace-regexp-in-string "\.yml$" "" (file-name-nondirectory filename)))

(defun amici-is-class (filename)
  "Does FILENAME contain a model, controller or helper class?"
  (let ((file-path (amici-relative-path filename)))
    (or (string-match "app/models" file-path)
        (string-match "app/controllers" file-path)
        (string-match "app/helpers/" file-path)
        (string-match "lib" file-path))))

(defun amici-is-test (filename)
  "Does FILENAME contain a test class?"
  (if (string-match "Test\\.php$" (file-name-nondirectory filename)) t nil))

(defun amici-is-fixture (filename)
  "Is FILENAME a fixture file?"
  (if (string-match "tests/fixtures/.+\\.yml" filename) t nil))


(defun amici-php-script-command (script &optional parameters)
  "return a shell command that runs the php script SCRIPT with PARAMETERS in the framework environment"
  (format "%s %s %s"
          amici-php-command
          script
          (apply 'concat (mapcar (lambda (str)
                                   (if str (concat str " ") ""))
                                 parameters))))

(defun amici-php-script (script buffer &optional parameters)
  "Run the php script SCRIPT with optional PARAMETERS putting output into BUFFER.
set the AMICI_FRAMEWORK_DIR environment variable to the value of amici-framework-dir"
  (shell-command (amici-php-script-command script parameters) buffer))

;; --- fixture load/teardown ---------------------------------------------------

(defun amici-find-all-fixtures ()
  "get a list of all fixtures in the current branch"
  (mapcar (lambda (str)
            (amici-fixture-by-file str))
          (split-string (shell-command-to-string (concat "ls "
                                                         (amici-root)
                                                         "tests/fixtures")) "\n")))
(defvar amici-fixture-history nil
  "Completion history for fixture functions")

(defun amici-read-fixture (prompt)
  "Prompt with PROMPT and do a completing read for one or more fixtures in the
 current branch. Multiple fixtures can be entered seperated by ,"
  (let ((default (if (amici-is-fixture buffer-file-name)
                     (amici-fixture-by-file buffer-file-name))))
    (completing-read-multiple (if default
                                  (format "%s (default %s): " prompt default)
                                (format "%s: " prompt))
                              (amici-find-all-fixtures)
                              nil
                              nil
                              nil
                              amici-fixture-history
                              default)))

(defun amici-fixture-load (fixtures)
  "Load FIXTURES into unit test database.
If called interactively, prompt for a fixture file to load. If the current
buffer is a fixture file, default the prompt to that file."
  (interactive (list (amici-read-fixture "Load fixture(s)")))
  (amici-php-script (format "%sscripts/load_fixtures.php" (amici-root))
                              "*fixture-output*"
                              fixtures))

(defun amici-fixture-teardown (fixtures)
  "Teardown FIXTURES from the unit test database.
If called interactively, prompt for a fixture file to load. If the current
buffer is a fixture file, default the prompt to that file."
  (interactive (list (amici-read-fixture "Teardown fixture(s)")))
  (amici-php-script (format "%sscripts/teardown_fixtures.php" (amici-root))
                              "*fixture-output*"
                              fixtures))

;; --- unit testing functions --------------------------------------------------
(defvar phpunit-options nil
  "Default phpunit options.")

(defvar phpunit-options-history nil
  "History of phpunit options.")

(eval-when-compile
  (require 'compile))

(defvar amici-test-regexp-alist
  '(("^\\(/.+?\\):\\([0-9]+\\)" 1 2))
  "Regexp used to match test errors/failures")

(defvar amici-function-regexp
  "function \\(.*\\)("
  "Regexp used to match function definitions.")

(defun amici-set-phpunit-options-failures (test-buffer &rest status)
  "Finds the name of all tests that failed and sets the phpunit options list to just those tests."
  (interactive)
  (setq filter-prefix "--filter test\\(")
  (setq phpunit-options filter-prefix)
  (set-buffer test-buffer) ;;"*amici-test*"
  (goto-char (point-min))
  (while (search-forward-regexp "[0-9]+) \\w+::test\\([A-Za-z0-9_]+\\)" nil t)
    (setq phpunit-options (concat phpunit-options (match-string 1) "$\\|")))
  (if (eq phpunit-options filter-prefix)
      ;; No tests failed so set options to run all tests.
      (setq phpunit-options "")
      ;; Replace last pipe char with closing paren.
    (setq phpunit-options (concat (substring phpunit-options 0 (- (length phpunit-options) 1)) ")"))))

(define-compilation-mode amici-test-mode "Amici Test"
  "Mode derived from compilation-mode for PHPUnit test output"
  (set (make-local-variable 'compilation-error-regexp-alist) amici-test-regexp-alist)
  (set (make-local-variable 'compilation-finish-functions) 'amici-set-phpunit-options-failures))

(defun amici-test-run (phpunit-options)
  "run the unit test in the current buffer"
  (interactive (list (setq phpunit-options (read-from-minibuffer "Additional phpunit options: "
                                                                 phpunit-options
                                                                 nil
                                                                 nil
                                                                 'phpunit-options-history))))
  (setq amici-test-file-name (if (amici-is-class (buffer-file-name))
                                 (amici-absolute-path (amici-file-to-test-file (amici-relative-path buffer-file-name)))
                               (buffer-file-name)))
  (if (amici-is-test amici-test-file-name)
      (compilation-start (amici-php-script-command (concat (amici-root) "scripts/phpunit")
                                                             (list "--verbose"
                                                                   phpunit-options
                                                                   (amici-class-by-file amici-test-file-name)
                                                                   amici-test-file-name))
                         'amici-test-mode)))

(defun amici-file-to-test-file (filename)
  "Return the path to the unit test file for FILENAME relative to the branch"
  (let ((class-to-test-alist '(("app/models/" . "tests/unit/%sTest.php")
                               ("app/controllers/" . "tests/functional/%sTest.php")
                               ("app/helpers/" . "tests/helper/%sTest.php")
                               ("lib/" . "tests/lib/%sTest.php")))
        (test-file nil))
    (while class-to-test-alist
      (progn
        (setq class-path (caar class-to-test-alist)
              test-format (cdar class-to-test-alist)
              class-to-test-alist (cdr class-to-test-alist))
        (if (string-match class-path filename)
            (setq test-file (format test-format
                                    (replace-regexp-in-string class-path
                                                              ""
                                                              (replace-regexp-in-string "\\.php$"
                                                                                        ""
                                                                                        filename)))))))
    test-file))

(defun amici-test-file-to-file (filename)
  "Return the path to the class file for the unit test file FILENAME relative to the branch"
  (let ((test-to-class-alist '(("tests/unit/" . "app/models/%s.php")
                               ("tests/functional/" . "app/controllers/%s.php")
                               ("tests/helper/" . "app/helpers/%s.php")
                               ("tests/lib/" . "lib/%s.php")))
        class-file)
    (while test-to-class-alist
      (progn
        (setq test-path (caar test-to-class-alist)
              class-format (cdar test-to-class-alist)
              test-to-class-alist (cdr test-to-class-alist))
        (if (string-match test-path filename)
            (setq class-file (format class-format
                                     (replace-regexp-in-string test-path
                                                               ""
                                                               (replace-regexp-in-string "Test\\.php$"
                                                                                         ""
                                                                                         filename)))))))
    class-file))

(defun amici-find-test-file ()
  "Switch to the test for the current file"
  (interactive)
  (let ((test-file (amici-absolute-path (amici-file-to-test-file (amici-relative-path buffer-file-name)))))
    (if (file-exists-p test-file)
        (find-file test-file)
      (message "file %s does not exist" test-file))))

(defun amici-find-class-file ()
  "Switch to the class for the current test file"
  (interactive)
  (let ((class-file (amici-absolute-path (amici-test-file-to-file (amici-relative-path buffer-file-name)))))
    (if (file-exists-p class-file)
        (find-file class-file)
      (message "file %s does not exist" class-file))))

(defun amici-toggle-class-test ()
  "If the current buffer is a class, switch to it's test or visa-versa"
  (interactive)
  (if (amici-is-test buffer-file-name)
      (amici-find-class-file)
    (if (amici-is-class buffer-file-name)
        (amici-find-test-file))))

(defun amici-run-test-at-point ()
  "Run a PHPUnit test where point is within the test definition."
  (interactive)
  (save-excursion
    (search-backward-regexp "function \\(test.*\\)(" nil t)
    (amici-test-run (concat "--filter " (match-string 1) "$"))))

(defun amici-run-test-for-function-at-point ()
  "Run all PHPUnit tests where point is within a function definition. Uses @covers to determine which tests to run."
  (interactive)
  (save-excursion
    (catch 'no-matches
      (when (amici-is-class buffer-file-name)
        (let* ((function-name (progn (goto-char (point-at-bol))
                                     ;; Function name at point. If we're on a block comment line get the
                                     ;; function it belongs to. Otherwise find the first function above point.
                                     (if (search-forward-regexp "\*\\|function" (point-at-eol) t)
                                         (progn (goto-char (point-at-bol))
                                                (search-forward-regexp amici-function-regexp nil t))
                                       (search-backward-regexp amici-function-regexp nil t))
                                     (or (match-string 1)
                                         (throw 'no-matches (message "No function at point")))))
               (class-name (amici-class-by-file buffer-file-name)) ;; Class to which the function belongs
               (full-name (concat class-name "::" function-name))) ;; String to search for in our unit tests
          (amici-find-test-file)
          (goto-char (point-min))
          (setq amici-filter-string "\\(") ;; List of tests which cover the function at point
          (while (search-forward-regexp (concat "@covers " full-name) nil t)
            ;; This is very naive. We look for the @covers annotation and then find the function immediately following that.
            (search-forward-regexp amici-function-regexp nil t)
            (setq amici-filter-string (concat amici-filter-string (match-string 1) "$\\|")))
          (if (equal amici-filter-string "\\(")
              ;; No matches. Fail.
              (message "Could not find any tests which cover %s" full-name)
            ;; Replace last pipe char with closing paren.
            (setq amici-filter-string (concat (substring amici-filter-string 0 (- (length amici-filter-string) 1)) ")"))
            ;; Run the tests
            (amici-test-run (concat "--filter " amici-filter-string)))
          ;; The amici-find-test-file appears to be breaking the save-excursion so we must manually go back to the class file.
          (amici-toggle-class-test))))))

(defun amici-find-function-at-point ()
  "Go to the class definition of the function at point. Works in test or class files."
  (interactive)
  (let ((function-name (progn (if (equal "self" (current-word)) ;; At the beginning of a static function call.
                                  (forward-word))
                              (current-word))))
    (if (amici-is-test buffer-file-name)
        (amici-find-class-file))
    (if (amici-is-class buffer-file-name)
        (progn (goto-char (point-min))
               (search-forward (concat "function " function-name) nil t))
      (message "Not in a class file"))))

(defun amici-on ()
  "determine whether or not to enable amici-mode in this buffer"
  (if (amici-root)
      (amici-mode 1)))

(define-minor-mode amici-mode
  "Minor mode for activating convenience functions when editing application files."
  nil " amici" amici-mode-map)

(define-globalized-minor-mode amici-global-mode amici-mode amici-on)
