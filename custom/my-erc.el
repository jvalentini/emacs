(require 'erc)
(require 'erc-button)

(erc-spelling-mode 1)
(setq erc-server "irc.amicillc.com"
      erc-port 7450
      erc-nick "jval"
      erc-full-name "Justin Valentini"
      erc-prompt-for-password nil
      erc-auto-query 'bury
      erc-autojoin-channels-alist '(("irc.amicillc.com" "#dev") ("irc.amicillc.com" "#pipes"))
      erc-fill-column 100
      erc-log-insert-log-on-open nil
      erc-log-channels-directory "~/.erc/logs/"
      erc-track-exclude-server-buffer t
      erc-track-shorten-start 5
      erc-track-shorten-cutoff 10
      erc-hide-list '("JOIN" "PART" "MODE" "QUIT"))

(add-hook 'erc-insert-post-hook 'erc-save-buffer-in-logs)
(add-hook 'erc-send-post-hook 'erc-save-buffer-in-logs)

(defface my-erc-header-line-disconnected
  '((t (:foreground "black" :background "indianred")))
  "Face to use when ERC is disconnected")

(defun my-erc-update-header-line-disconnected ()
  "Use a different face in the header line when disconnected"
  (erc-with-server-buffer
    (cond ((erc-server-process-alive) 'erc-header-line)
          (t 'my-erc-header-line-disconnected))))
(setq erc-header-line-face-method 'my-erc-update-header-line-disconnected)

;; timestamps go on the left, if this is the first message of the day
;; in this buffer, insert a line with the date
(make-variable-buffer-local
 (defvar erc-last-datestamp nil))

(defun my-insert-timestamp (string)
  (erc-insert-timestamp-left string)
  (let ((datestamp (erc-format-timestamp (current-time) erc-datestamp-format)))
    (unless (string= datestamp erc-last-datestamp)
      (erc-insert-timestamp-left datestamp)
      (setq erc-last-datestamp datestamp))))

(setq erc-fill-prefix "      "
      erc-timestamp-only-if-changed-flag nil
      erc-timestamp-format "%H:%M "
      erc-datestamp-format " === %Y-%m-%d %a ===\n"
      erc-insert-timestamp-function 'my-insert-timestamp)

;; if something looks like a jira task make it clickable and browse to it
(add-to-list 'erc-button-alist '("\\b\\(xls-\\)?\\([0-9]+\\)\\b" 0 t (lambda (issue-code)
                                                                       (browse-url
                                                                        (concat "http://jira.amicillc.com/browse/XLS-" issue-code))) 2))

(require 'notifications)
(defun erc-global-notify (match-type nick message)
  "Notify when a message is recieved."
  (notifications-notify
   :title nick
   :body message
   :app-icon "/usr/share/notify-osd/icons/gnome/scalable/status/notification-message-im.svg"
   :urgency 'low
   :timeout -1))
;; (add-hook 'erc-text-matched-hook 'erc-global-notify)

;;; Notify me when a keyword is matched (someone wants to reach me)

(defvar my-erc-page-message "%s is calling your name."
  "Format of message to display in dialog box")

(defvar my-erc-page-nick-alist nil
  "Alist of nicks and the last time they tried to trigger a
notification")

(defvar my-erc-page-timeout 30
  "Number of seconds that must elapse between notifications from
the same person.")

(defun my-erc-page-popup-notification (nick)
  (when window-system
    ;; must set default directory, otherwise start-process is unhappy
    ;; when this is something remote or nonexistent
    (let ((default-directory "~/"))
      ;; 8640000 milliseconds = 1 day
      (start-process "page-me" nil "notify-send"
                     "-u" "normal" "-t" "8640000" "ERC"
                     (format my-erc-page-message nick)))))

(defun my-erc-page-allowed (nick &optional delay)
  "Return non-nil if a notification should be made for NICK.
If DELAY is specified, it will be the minimum time in seconds
that can occur between two notifications.  The default is
`my-erc-page-timeout'."
  (unless delay (setq delay my-erc-page-timeout))
  (let ((cur-time (time-to-seconds (current-time)))
        (cur-assoc (assoc nick my-erc-page-nick-alist))
        (last-time nil))
    (if cur-assoc
        (progn
          (setq last-time (cdr cur-assoc))
          (setcdr cur-assoc cur-time)
          (> (abs (- cur-time last-time)) delay))
      (push (cons nick cur-time) my-erc-page-nick-alist)
      t)))

(defun my-erc-page-me (match-type nick message)
  "Notify the current user when someone sends a message that
matches a regexp in `erc-keywords'."
  (interactive)
  (when (and (eq match-type 'keyword)
             ;; I don't want to see anything from the erc server
             (not (string-match "\\`\\([sS]erver\\|localhost\\|root\\)" nick))
             ;; or bots
             (not (string-match "\\(^CIA[^!]*\\|bot\\|serv\\)!" nick))
             ;; or from those who abuse the system
             (my-erc-page-allowed nick))
    (my-erc-page-popup-notification nick)))

(add-hook 'erc-text-matched-hook 'my-erc-page-me)

(defun my-erc-page-me-PRIVMSG (proc parsed)
  (let ((nick (car (erc-parse-user (erc-response.sender parsed))))
        (target (car (erc-response.command-args parsed)))
        (msg (erc-response.contents parsed)))
    (when (and (erc-current-nick-p target)
               (not (erc-is-message-ctcp-and-not-action-p msg))
               (my-erc-page-allowed nick))
      (my-erc-page-popup-notification nick)
      nil)))
(add-hook 'erc-server-PRIVMSG-functions 'my-erc-page-me-PRIVMSG)

;; if problem then: killall notification-daemon
