;; Needed for keybindings because screen sucks ass
;; -- below is not necessary for emacsclient, which is superior to screen anyway
;; (defvar real-keyboard-keys
;;   '(("M-<up>"        . "\M-[1;3A")
;;     ("M-<down>"      . "\M-[1;3B")
;;     ("M-<right>"     . "\M-[1;3C")
;;     ("M-<left>"      . "\M-[1;3D")
;;     ("C-<return>"    . "\C-j")
;;     ("C-<delete>"    . "\M-[3;5~")
;;     ("C-<up>"        . "\M-[1;5A")
;;     ("C-<down>"      . "\M-[1;5B")
;;     ("C-<right>"     . "\M-[1;5C")
;;     ("C-<left>"      . "\M-[1;5D")))
;;
;; (defun key (desc)
;;   (or (and window-system (read-kbd-macro desc))
;;       (or (cdr (assoc desc real-keyboard-keys))
;;           (read-kbd-macro desc))))

(global-set-key (kbd "M-[ B") (kbd "<down>"))
(global-set-key (kbd "M-[ C") (kbd "<right>"))
(global-set-key (kbd "M-[ D") (kbd "<left>"))
(global-set-key (kbd "M-[ a") (kbd "S-<up>"))
(global-set-key (kbd "M-[ b") (kbd "S-<down>"))
(global-set-key (kbd "M-[ c") (kbd "S-<right>"))
(global-set-key (kbd "M-[ d") (kbd "S-<left>"))
(global-set-key (kbd "M-O a") (kbd "C-<up>"))
(global-set-key (kbd "M-O b") (kbd "C-<down>"))
(global-set-key (kbd "M-O c") (kbd "C-<right>"))
(global-set-key (kbd "M-O d") (kbd "C-<left>"))
(global-set-key (kbd "ESC M-[ A") (kbd "M-<up>"))
(global-set-key (kbd "ESC M-[ B") (kbd "M-<down>"))
(global-set-key (kbd "ESC M-[ C") (kbd "M-<right>"))
(global-set-key (kbd "ESC M-[ D") (kbd "M-<left>"))

(global-set-key (kbd "<down>") 'next-line)

;; Better window navigation (beats the hell out of C-x o)
;; (require 'windmove)

;; (global-set-key (kbd "M-<left>") 'windmove-left)          ; move to left windnow
;; (global-set-key (kbd "M-<right>") 'windmove-right)        ; move to right window
;; (global-set-key (kbd "M-<up>") 'windmove-up)              ; move to upper window
;; (global-set-key (kbd "M-<down>") 'windmove-down)          ; move to downer window

(global-set-key (kbd "C-x v") 'ido-switch-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x i") 'ibuffer)
(global-set-key (kbd "C-x g") 'rgrep)
(global-set-key (kbd "C-x d") 'my-get-db)

;; Perforce
(global-set-key (kbd "C-x p o") 'my-p4-opened-default)
(global-set-key (kbd "C-x p O") 'my-p4-opened-default)
(global-set-key (kbd "C-x p C") 'my-p4-opened-catchall)
(global-set-key (kbd "C-x p P") 'my-p4-changes-pending)

(global-set-key (kbd "C-c g") 'goto-line)
(global-set-key (kbd "C-c r") 'query-replace)
(global-set-key (kbd "C-c a") 'align-regexp)
(global-set-key (kbd "C-c t") 'delete-trailing-whitespace)
(global-set-key (kbd "C-c w") 'backward-kill-word)
(global-set-key (kbd "C-c s") 'shrink-window)
(global-set-key (kbd "C-c e") 'enlarge-window)
(global-set-key (kbd "C-c C-u") 'uncomment-region)
(global-set-key (kbd "C-c b") 'bury-buffer)
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c o") 'sql-oracle)
(global-set-key (kbd "C-c i") 'delete-enclosed-text)
(global-set-key (kbd "M-O Q") 'my-sql-app-connect)
(global-set-key (kbd "M-O R") 'my-sql-dev-connect)
(global-set-key (kbd "C-c f") 'show-file-name)

(global-set-key (kbd "M-n") 'next-line)
(global-set-key (kbd "M-p") 'previous-line)
(global-set-key (kbd "M-k") 'copy-line)

(global-set-key (read-kbd-macro "<M-DEL>") 'backward-delete-word)

(global-set-key (kbd "S-<down>")     'scroll-one-line-up)
(global-set-key (kbd "S-<add>")      'scroll-one-line-up)
(global-set-key (kbd "S-<up>")       'scroll-one-line-down)
(global-set-key (kbd "S-<subtract>") 'scroll-one-line-down)

(define-key global-map "\C-cv" 'vc-prefix-map)
(global-set-key (kbd "C-c v v") 'vc-next-action)
(global-set-key (kbd "C-x m") 'magit-status)

;; (eval-after-load "lib/amici")
;;   '(progn
;;      (global-set-key (kbd "<f2>") 'my-sql-app-connect)
;;      (global-set-key (kbd "<f3>") 'my-sql-connect)
;;      (global-set-key (kbd "<f4>") 'my-sql-reconnect)
;;      (global-set-key (kbd "<f4>") 'amici-find-function-at-point)
;;      (global-set-key (kbd "<f5>") 'amici-toggle-class-test)
;;      (global-set-key (kbd "<f6>") 'amici-test-run)
;;      (global-set-key (kbd "<f7>") 'amici-run-test-at-point)
;;      (global-set-key (kbd "<f8>") 'amici-run-test-for-function-at-point)
;;      (global-set-key (kbd "<f9>") 'amici-find-function-at-point)
;;      (global-set-key (kbd "C-x a l") 'amici-fixture-load)
;;      (global-set-key (kbd "C-x a t") 'amici-fixture-teardown))

(global-set-key (kbd "<f2>") 'my-sql-app-connect)
(global-set-key (kbd "<f3>") 'my-sql-dev-connect)
(global-set-key (kbd "<f4>") 'my-sql-reconnect)
(global-set-key (kbd "<f4>") 'amici-find-function-at-point)
(global-set-key (kbd "<f5>") 'amici-toggle-class-test)
(global-set-key (kbd "<f6>") 'amici-test-run)
(global-set-key (kbd "<f7>") 'amici-run-test-at-point)
(global-set-key (kbd "<f8>") 'amici-run-test-for-function-at-point)
(global-set-key (kbd "<f9>") 'amici-find-function-at-point)
(global-set-key (kbd "C-x a l") 'amici-fixture-load)
(global-set-key (kbd "C-x a t") 'amici-fixture-teardown)

;; (eval-after-load (concat custom-dir "justin-amici")
;;   '(progn
;;      (global-set-key (kbd "<f4>") 'justin-toggle-selective-display)
;;      (global-set-key (kbd "<f7>") 'justin-switch-to-test-output)
;;      (global-set-key (kbd "<f8>") 'justin-find-main-file)
;;      (global-set-key (kbd "<f9>") 'justin-blame)
;;      (global-set-key (kbd "C-x a f") 'justin-find-fixture-file)))

;; Prompt before exiting Emacs.
(global-unset-key "\C-x\C-c")
(global-set-key "\C-x\C-c"
                (lambda ()
                  "Ask for confirmation before exiting emacs"
                  (interactive)
                  (if (yes-or-no-p "Are you sure you want to exit? ")
                      (save-buffers-kill-terminal))))
