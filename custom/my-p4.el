(if (or (getenv "P4CONFIG") (getenv "P4CLIENT"))
    (progn
      (load-file (concat user-emacs-directory "lib/p4.el"))
      ;; shutup p4
      (setq p4-verbose nil)
      ;; Color customization
      (set-face-foreground 'p4-diff-head-face "black")
      (set-face-background 'p4-diff-file-face "blue")
      (set-face-background 'p4-diff-ins-face "blue")
      (set-face-foreground 'p4-diff-ins-face "cyan")
      (set-face-background 'p4-diff-del-face "red")
      (set-face-foreground 'p4-diff-del-face "white")
      (set-face-foreground 'p4-depot-unmapped-face "magenta")
      (global-set-key (kbd "C-x p t") 'justin-jira-link)))

(defun my-p4-opened-default ()
  "Only show files opened in the default changelist"
  (interactive)
  (p4-opened-internal (p4-make-list-from-string "-c default")))

(defun my-p4-opened-catchall ()
  "Show files opened in my catch-all changelist"
  (interactive)
  (p4-opened-internal (p4-make-list-from-string "-c 64766")))

(defun my-p4-changes-pending ()
  "Show my pending changelists"
  (interactive)
  (p4-file-change-log "changes" (p4-make-list-from-string "-m 100 -u jvalentini -s pending")))

;; (defun my-p4-last-submitted-change (arg)
;;   "Return the last changelist I submitted"
;;   (interactive "P")
;;   (print arg))
;;   (let ((max-length (if (not arg)
;;                         (1)
;;                       (arg))))
;;   (p4-file-change-log "changes" (p4-make-list-from-string (concat "-m " arg " -u jvalentini -s submitted"))


;; Override p4-depot-output to use my function which will fix
;; *P4 Output* buffers from being spammed.
(defun p4-depot-output (command &optional args)
  "Executes p4 command inside a buffer. Returns the buffer."
  ;; Fix: Replaced (generate-new-buffer) with (get-buffer-create) to
  ;; stop all the P4 Output buffers from being created.
  ;; Author: Justin Valentini
  (let ((buffer (get-buffer-create p4-output-buffer-name)))
    (p4-exec-p4 buffer (cons command args) t)
    buffer))


;; Print out ts link
(defun justin-ts-link ()
  "Print out a TrackStudio link to the specified changelist"
  (interactive)
  (let* ((changelist (read-from-minibuffer "Changelist: "))
         (arg-string (p4-make-list-from-string (concat "-s " changelist)))
         (buffer-name (concat "*P4 describe: " (p4-list-to-string arg-string) "*")))
    (p4-describe-internal arg-string)
    (set-buffer buffer-name)
    (setq buffer-read-only nil)
    (goto-char (point-max))
    (insert (concat "https://matrix.amicillc.com/perforce/changelist.php?changelistid=" changelist))
    (setq buffer-read-only t)))

;; Print out jira link
(defun justin-jira-link ()
  "Print out a JIRA link to the specified changelist"
  (interactive)
  (let* ((last-changelist (string-replace "\n" "" (shell-command-to-string "p4 changes -u $USER -s submitted -m 1 | awk '{print $2}'")))
         (changelist (read-from-minibuffer "Changelist: " last-changelist))
         (arg-string (p4-make-list-from-string (concat "-s " changelist)))
         (buffer-name (concat "*P4 describe: " (p4-list-to-string arg-string) "*")))
    (p4-describe-internal arg-string)
    (set-buffer buffer-name)
    (setq buffer-read-only nil)
    (goto-char (point-max))
    (insert (concat "https://matrix.amicillc.com/perforce/changelist.php?changelistid=" changelist))
    (let* ((reviewboard-output (shell-command-to-string (concat "post-review --repository-type=perforce -p " changelist)))
           (dummy (string-match "http://reviewboard.amicillc.com/r/[0-9]+" reviewboard-output))
           (reviewboard-link (match-string 0 reviewboard-output)))
      (insert (concat "\n\n" reviewboard-link))
      (message reviewboard-link))
    (setq buffer-read-only t)))

;; Remove the check that prompts if you want to sumbit files with empty diffs
(defp4cmd p4-submit (&optional arg)
  "submit" "To submit a pending change to the depot, type \\[p4-submit].\n"
  (interactive "P")
  (let (args
	(submit-buf-name "*P4 Submit*")
	(change-list (if (integerp arg) arg)))
    (if (buffer-live-p (get-buffer submit-buf-name))
	(switch-to-buffer-other-window (get-buffer submit-buf-name))
      (if change-list
	  (setq args (list "-c" (int-to-string change-list)))
	(if current-prefix-arg
	    (setq args (p4-make-list-from-string
			(p4-read-arg-string "p4 submit: " nil)))))
      (setq args (p4-filter-out (lambda (x) (string= x "-c")) args))
      (p4-save-opened-files)
	  (p4-async-process-command "change" "Description:\n\t"
				    submit-buf-name "submit" args))))

;; straight-forward solution doing just string manipulation
;; it's rather heavy handed
(defun string-replace (from to string &optional re)
  "Replace all occurrences of FROM with TO in STRING.
All arguments are strings.
When optional fourth argument is non-nil, treat the from as a regular expression."
  (let ((pos 0)
        (res "")
        (from (if re from (regexp-quote from))))
    (while (< pos (length string))
      (if (setq beg (string-match from string pos))
          (progn
            (setq res (concat res
                              (substring string pos (match-beginning 0))
                              to))
            (setq pos (match-end 0)))
        (progn
          (setq res (concat res (substring string pos (length string))))
          (setq pos (length string)))))
    res))
