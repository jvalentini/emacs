(defun justin-find-fixture-file ()
  (interactive)
  "Prompt user for fixture file to open using the word at point
as a suggested fixture name"
  (setq filename (concat (concat (justin-branch-root)
                                 "amici_php/tests/fixtures/")
                         (read-from-minibuffer "Open fixture file: "
                                               (concat (current-word) ".yml"))))
  (if (and (amici-is-fixture filename)
           (file-exists-p filename))
      (find-file filename)
    (message "file %s does not exist" filename)))

(defun justin-switch-to-test-output ()
  (interactive)
  "Open the amici test output in another window"
  (switch-to-buffer-other-window "*amici-test*"))

(defun justin-find-main-file ()
  "Switch to the class in the main perforce branch for the current file"
  (interactive)
  (if (amici-is-test buffer-file-name)
      (amici-find-class-file))
  (let ((main-file (concat "/home/jvalentini/p4/amici/main/" (justin-relative-path buffer-file-name))))
    (if (file-exists-p main-file)
        (find-file main-file)
      (message "file %s does not exist" main-file))))

(defun justin-relative-path (filename)
  "Return the path of FILENAME relative to the branch root"
  (replace-regexp-in-string (justin-branch-root) "" filename))

(defun justin-branch-root ()
  "Get the branch root directory"
  (let ((current-dir default-directory)
        (max 10)
        (found nil))
    (while (and (not found) (> max 0))
      (if (file-exists-p (concat current-dir "app_builder"))
          (setq found t)
        (setq current-dir (concat current-dir "../")
              max (- max 1))))
    (if found (expand-file-name current-dir))))

(defun justin-blame ()
  "Call p4 blame and move point to line number in file.
The line is incrented by 2 due to a header added in p4-blame."
  (interactive)
  (setq point-line-num (line-number-at-pos))
  (p4-blame)
  (other-window 1)
  (goto-line (+ point-line-num 2))
  (forward-word)
  (search-backward-regexp "\\([0-9]\\{5\\}\\)")
  (p4-describe-internal (p4-make-list-from-string
                         (concat p4-default-diff-options
                                 (concat " " (match-string 1)))))
  (switch-to-buffer (other-buffer))
  (move-beginning-of-line 1)
  (setq justin-beg-point (point))
  (move-end-of-line 1)
  (buffer-substring justin-beg-point (point)))
