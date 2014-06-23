(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'package)
(setq package-archives (cons '("tromey" . "http://tromey.com/elpa/") package-archives))
(package-initialize)

(let ((path (shell-command-to-string ". ~/dotfiles/.exports.sh; echo -n $PATH")))
  (setenv "PATH" path)
  (setq exec-path
        (append
         (split-string-and-unquote path ":")
         exec-path)))

(let ((ld-library-path (shell-command-to-string ". ~/dotfiles/.exports.sh; echo -n $LD_LIBRARY_PATH")))
  (if ld-library-path
      (setenv "LD_LIBRARY_PATH" ld-library-path)))

(let ((oracle-home (shell-command-to-string ". ~/dotfiles/.exports.sh; echo -n $ORACLE_HOME")))
  (if oracle-home
      (setenv "ORACLE_HOME" oracle-home)))

(let ((tns-admin (shell-command-to-string ". ~/dotfiles/.exports.sh; echo -n $TNS_ADMIN")))
  (if tns-admin
      (setenv "TNS_ADMIN" tns-admin)))

(let ((sqlpath (shell-command-to-string ". ~/dotfiles/.exports.sh; echo -n $SQLPATH")))
  (if sqlpath
      (setenv "SQLPATH" sqlpath)))

;; All proxy config set in /etc/environment

;; (require 'flx-ido)
;; (flx-ido-mode 1)
(setq ido-use-faces nil) ;; disable ido faces to see flx highlights.

;; Project management
(projectile-global-mode)
(setq projectile-enable-caching t)
(setq projectile-keymap-prefix (kbd "C-x p"))

(add-to-list 'load-path "~/.emacs.d")

(fset 'yes-or-no-p 'y-or-n-p)

;; Tab = 4 spaces
(setq default-tab-width 4
      tab-width 4)
(setq-default tab-width 4
              indent-tabs-mode nil)

(setq
 apropos-do-all t
 auto-save-default nil
 byte-compile-verbose nil
 byte-compile-warnings nil
 confirm-kill-emacs 'yes-or-no-p
 dabbrev-abbrev-skip-leading-regexp "\\$"
 dired-recursive-copies 'always
 dired-recursive-deletes 'always
 display-time-day-and-date t
 fill-column 128
 font-lock-maximum-decoration t
 global-subword-mode 1
 grep-command "grep -rin"
 hippie-expand-dabbrev-as-symbol nil
 ido-enable-flex-matching t
 ido-directory-too-big nil
 inhibit-splash-screen t
 inhibit-startup-message t
 make-backup-files nil
 mouse-yank-at-point t
 next-line-add-newlines nil
 nrepl-hide-special-buffers t
 plsql-indent 4
 require-final-newline nil
 save-interprogram-paste-before-kill t
 truncate-partial-width-windows nil
 x-select-enable-clipboard t
 x-select-enable-primary t)

(require 'smartparens-config)

(blink-cursor-mode t)
(column-number-mode t)
(delete-selection-mode t)
(desktop-save-mode t)
(global-font-lock-mode t)
(icomplete-mode t)
(ido-mode t)
(line-number-mode t)
(set-fringe-style -1)
(show-paren-mode t)
(smartparens-global-mode t)
(toggle-truncate-lines -1)
(tooltip-mode -1)
(transient-mark-mode t)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(google-this-mode 1)
(howdoi-minor-mode t)

;; Show menu bar in X. Hide in terminal.
(if (eq window-system 'x)
    (menu-bar-mode 1)
  (menu-bar-mode 0))

(require 'json)

;; Smart duplicate buffer renaming
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "|")
(setq uniquify-after-kill-buffer-p nil)
(setq uniquify-ignore-buffers-re "^\\*")

;; Save point for every file
(setq-default save-place t)
(require 'saveplace)

;; Disable warning for narrow to region.
(put 'narrow-to-region 'disabled nil)

;; Comments should be red.
(set-face-foreground 'font-lock-comment-face "red")

;; Allow me to use these.
(put 'downcase-region 'disabled nil)
(put 'upcase-region   'disabled nil)

;; Customize ibuffer formatting.
(setq ibuffer-formats '((mark modified read-only " "
                              (name 35 35 :left :elide)
                              " "
                              (size 9 -1 :right)
                              " "
                              (mode 16 16 :left :elide)
                              " " filename-and-process)
                        (mark " "
                              (name 16 -1)
                              " " filename)))

;; Highlight trailing whitespace in a hideous red color.
(add-hook 'find-file-hook
          (lambda ()
            "Highlight trailing whitespace in a hideous red color"
            (progn
              (show-paren-mode 1)
              (setq indicate-empty-lines t
                    show-trailing-whitespace t))))

(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

; Always delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(require 'org)
(require 'org-pomodoro)
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(setq org-clock-idle-time nil)
(setq org-completion-use-ido t)
(setq org-directory "~/Dropbox/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-work-file (concat org-directory "/work.org"))
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-co" 'org-agenda)
(define-key global-map (kbd "<f12>") 'org-agenda)
(define-key global-map (kbd "<f11>") 'org-clock-goto)
(define-key global-map (kbd "C-<f11>") 'org-clock-in)
(setq org-log-done 'time)
(setq org-startup-indented t)
(setq org-capture-templates
      '(("p" "Pipes tasks" entry (file+headline org-work-file "Pipes")
         "* TODO %?\n  %i")
        ("i" "Interview Feedback" entry (file+headline org-work-file "Interview Feedback"))
        ("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
         "* TODO %?\n  %i\n  %a")))

(add-hook 'python-mode-hook '(lambda ()
                               (setq python-indent-offset 4)
                               (require 'virtualenvwrapper)
                               (venv-initialize-interactive-shells)
                               (venv-initialize-eshell)
                               (setq venv-location "~/virtualenv/")
                               (setq
                                python-shell-interpreter "ipython"
                                python-shell-interpreter-args ""
                                python-shell-prompt-regexp "In \\[[0-9]+\\]: "
                                python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
                                python-shell-completion-setup-code
                                "from IPython.core.completerlib import module_completion"
                                python-shell-completion-module-string-code
                                "';'.join(module_completion('''%s'''))\n"
                                python-shell-completion-string-code
                                "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")))

(autoload 'ruby-mode "ruby-mode" nil t)
(add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec\\'" . ruby-mode))
(add-hook 'ruby-mode-hook '(lambda ()
                             (setq ruby-deep-arglist t)
                             (setq ruby-deep-indent-paren nil)
                             (setq c-tab-always-indent nil)
                             (require 'inf-ruby)
                             (require 'ruby-compilation)
                             (define-key ruby-mode-map (kbd "<f5>") 'rspec-toggle-spec-and-target)
                             (define-key ruby-mode-map (kbd "<f6>") 'rspec-verify-all)
                             (define-key ruby-mode-map (kbd "<f7>") 'rspec-verify)))

(defun rhtml-mode-hook ()
  (autoload 'rhtml-mode "rhtml-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . rhtml-mode))
  (add-to-list 'auto-mode-alist '("\\.rjs\\'" . rhtml-mode))
  (add-hook 'rhtml-mode '(lambda ()
                           (define-key rhtml-mode-map (kbd "M-s") 'save-buffer))))

(defun yaml-mode-hook ()
  (autoload 'yaml-mode "yaml-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode)))

(defun css-mode-hook ()
  (autoload 'css-mode "css-mode" nil t)
  (add-hook 'css-mode-hook '(lambda ()
                              (setq css-indent-level 2)
                              (setq css-indent-offset 2))))

(global-set-key (kbd "C-x v") 'switch-to-buffer)
(global-set-key (kbd "C-c g") 'goto-line)
(global-set-key (kbd "C-c r") 'query-replace-regexp)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; (setq cider-popup-stacktraces nil)
(add-hook 'cider-repl-mode-hook 'subword-mode)
(add-hook 'clojure-mode-hook 'cider-mode)

(load "lib/amici")
(load "lib/sql")
;; When starting a daemon loading lib/sqlplus causes an error
;; (load "lib/sqlplus")
(load "lib/plsql")
(load "custom/my-sql")
(load "custom/my-erc")
(load "custom/my-amici")
(load "custom/my-php-mode")
(load "custom/my-keys")

;; Deprecated. No longer using p4.
;; (load "lib/p4")
;; (load "custom/my-p4")

;; autoload modes
(autoload 'plsql-mode   "plsql")
(autoload 'sql-mode     "sql")
(autoload 'php-mode     "php-mode")
(autoload 'yaml-mode    "yaml-mode")
(autoload 'sqlplus-mode "sqlplus")
(autoload 'magit-status "magit" nil t)

(add-to-list 'auto-mode-alist '("\\.html$"   . sgml-mode))
(add-to-list 'auto-mode-alist '("\\.php$"    . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$"    . php-mode))
(add-to-list 'auto-mode-alist '("\\.rb$"     . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.css$"    . css-mode))
(add-to-list 'auto-mode-alist '("\\.js$"     . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.rhtml$"  . sgml-mode))
(add-to-list 'auto-mode-alist '("\\.sql$"    . sql-mode))
(add-to-list 'auto-mode-alist '("\\.sqp$"    . sqlplus-mode))
(add-to-list 'auto-mode-alist '("\\.pk[sb]$" . plsql-mode))
(add-to-list 'auto-mode-alist '("\\.yml$"    . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.list$"   . list-mode))
(add-to-list 'auto-mode-alist '("\\.org$"    . org-mode))
(add-to-list 'auto-mode-alist '("\\.py$"     . python-mode))
(add-to-list 'auto-mode-alist '("\\.hs$"     . haskell-mode))

;; YAML
(add-hook 'yaml-mode-hook 'c-subword-mode)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))

(defun delete-enclosed-text ()
  "Delete texts between any pair of delimiters."
  (interactive)
  (save-excursion
    (let (p1 p2)
      (skip-chars-backward "^([<>“'") (setq p1 (point))
      (skip-chars-forward "^)]<>”'") (setq p2 (point))
      (delete-region p1 p2))))

;; Backups
(defconst use-backup-dir t)
(setq backup-directory-alist (quote ((".*" . "~/.backups/")))
      version-control t                ; Use version numbers for backups
      kept-new-versions 16             ; Number of newest versions to keep
      kept-old-versions 2              ; Number of oldest versions to keep
      delete-old-versions t            ; Ask to delete excess backup versions?
      backup-by-copying-when-linked t) ; Copy linked files, don't rename.

;; unicode support
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(defun my-get-db ()
  "Given a matter schema, find the name of the production database."
  (interactive)
  (message (shell-command-to-string (concat "~/scripts/shell/get_db.sh " (read-from-minibuffer "Matter code: ") ""))))

;; Ansi colors for eshell
(require 'ansi-color) (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(setq comint-prompt-read-only t)

;; CIDER config
(setq cider-repl-pop-to-buffer-on-connect nil)
(setq cider-popup-stacktraces nil)
(setq cider-repl-popup-stacktraces t)
(setq cider-auto-select-error-buffer t)
(setq cider-repl-display-in-current-window t)
(setq cider-repl-print-length 100)
(setq cider-repl-wrap-history t)
(setq cider-repl-history-size 1000)
(add-hook 'cider-repl-mode-hook 'subword-mode)

(add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)

(defun magit-default-tracking-name-branch-unescaped
  (remote banch)
  "Use just the branch name for tracking branches."
  branch)

;; Paredit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'clojure-mode-hook #'enable-paredit-mode)

;; Add rainbow delimiters in all programming modes
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (load-theme 'solarized-dark t)))
  (load-theme 'solarized-dark t))

(set-frame-parameter nil 'fullscreen 'maximized)

(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r" "C-x /" "C-x a" "C-c p" "C-c C-x" "C-c /" "C-c C-o"))
(guide-key-mode 1)

(require 'auto-complete-config)
(ac-config-default)
(setq ac-show-menu-immediately-on-auto-complete t)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
(setq ac-use-menu-map t)
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)

(add-hook 'after-init-hook #'global-flycheck-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])
 '(comment-style (quote plain))
 '(custom-safe-themes (quote ("4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(erc-auto-query (quote frame))
 '(erc-email-userid "justin.valentini@gmail.com")
 '(erc-generate-log-file-name-function (quote erc-generate-log-file-name-network))
 '(erc-keywords (quote ("lunch" "coffee")))
 '(erc-modules (quote (autojoin button completion fill irccontrols list log match menu move-to-prompt netsplit networks noncommands notifications readonly replace ring scrolltobottom smiley stamp spelling track)))
 '(erc-user-full-name "Justin Valentini")
 '(ibuffer-saved-filter-groups (quote (("justin" ("P4 Output" (name . "*P4")) ("Models" (filename . "models")) ("Test PHP" (name . "test") (mode . php-mode)) ("Fixtures" (mode . yaml-mode)) ("Controllers" (filename . "controllers")) ("Helpers" (filename . "helpers")) ("Views" (mode . sgml-mode)) ("Framework" (filename . "framework")) ("PHP" (mode . php-mode)) ("SQL" (or (mode . plsql-mode) (mode . sql-mode))) ("Oracle Sessions" (mode . sql-interactive-mode)) ("Org Mode" (mode . org-mode)) ("Python" (mode . python-mode)) ("Javascript" (mode . js2-mode)) ("Emacs Config" (mode . emacs-lisp-mode)) ("SRC" (filename . "src/php")) ("Logs" (name . "\\.log")) ("Dired" (mode . dired-mode)) ("P4 Output" (name . "*P4"))) ("justin-default-buffer-groups" ("Models" (filename . "models")) ("Test PHP" (name . "test") (mode . php-mode)) ("Fixtures" (mode . yaml-mode)) ("Controllers" (filename . "controllers")) ("Helpers" (filename . "helpers")) ("Views" (mode . sgml-mode)) ("Framework" (filename . "framework")) ("PHP" (mode . php-mode)) ("SQL" (or (mode . plsql-mode) (mode . sql-mode))) ("Oracle Sessions" (mode . sql-interactive-mode)) ("Org Mode" (mode . org-mode)) ("Python" (mode . python-mode)) ("Javascript" (mode . js2-mode)) ("Emacs Config" (mode . emacs-lisp-mode)) ("SRC" (filename . "src/php")) ("Logs" (name . "\\.log")) ("Dired" (mode . dired-mode)) ("P4 Output" (name . "*P4"))) ("justin-default-buffer-groups" ("Models" (filename . "models")) ("Test PHP" (name . "test") (mode . php-mode)) ("Fixtures" (mode . yaml-mode)) ("Controllers" (filename . "controllers")) ("Helpers" (filename . "helpers")) ("Views" (mode . sgml-mode)) ("Framework" (filename . "framework")) ("PHP" (mode . php-mode)) ("SQL" (or (mode . plsql-mode) (mode . sql-mode))) ("Oracle Sessions" (mode . sql-interactive-mode)) ("Org Mode" (mode . org-mode)) ("Python" (mode . python-mode)) ("Javascript" (mode . js2-mode)) ("Emacs Config" (mode . emacs-lisp-mode)) ("SRC" (filename . "src/php")) ("Logs" (name . "\\.log")) ("Dired" (mode . dired-mode))))))
 '(ibuffer-saved-filters (quote (("P4 Output" ((name . "*P4 Output*"))) ("gnus" ((or (mode . message-mode) (mode . mail-mode) (mode . gnus-group-mode) (mode . gnus-summary-mode) (mode . gnus-article-mode)))) ("programming" ((or (mode . emacs-lisp-mode) (mode . cperl-mode) (mode . c-mode) (mode . java-mode) (mode . idl-mode) (mode . lisp-mode)))))))
 '(ido-cache-ftp-work-directory-time 0.1)
 '(ido-everywhere t)
 '(magit-default-tracking-name-function (quote magit-default-tracking-name-branch-unescaped))
 '(magit-diff-use-overlays nil)
 '(mode-require-final-newline nil)
 '(python-check-command "pylint")
 '(solarized-height-plus-1 1)
 '(solarized-height-plus-2 1)
 '(solarized-height-plus-3 1)
 '(solarized-height-plus-4 1)
 '(sql-product (quote oracle))
 '(tramp-completion-reread-directory-timeout 5)
 '(tramp-default-host "alyssa.amicillc.com")
 '(tramp-default-method "ssh")
 '(tramp-default-user "jvalentini"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit variable-pitch :foreground "#cb4b16" :height 1.0))))
 '(org-level-2 ((t (:inherit variable-pitch :foreground "#859900" :height 1.0))))
 '(org-level-3 ((t (:inherit variable-pitch :foreground "#268bd2" :height 1.0))))
 '(org-level-4 ((t (:inherit variable-pitch :foreground "#b58900" :height 1.0)))))
