(require 'erc)
(require 'erc-button)

(erc-spelling-mode 1)
(setq erc-server "irc.amicillc.com"
      erc-port 7450
      erc-nick "jval"
      erc-full-name "Justin Valentini"
      erc-prompt-for-password nil
      erc-auto-query 'bury
      erc-autojoin-channels-alist '(("irc.amicillc.com" "#dev"))
      erc-fill-column 100
      erc-log-insert-log-on-open t
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
                                                                        (concat "http://jira.amicillc.com:8080/browse/XLS-" issue-code))) 2))

(require 'notifications)
(defun erc-global-notify (match-type nick message)
  "Notify when a message is recieved."
  (notifications-notify
   :title nick
   :body message
   :app-icon "/usr/share/notify-osd/icons/gnome/scalable/status/notification-message-im.svg"
   :urgency 'low))

(add-hook 'erc-text-matched-hook 'erc-global-notify)