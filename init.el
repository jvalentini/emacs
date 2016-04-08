(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

(when (and (= emacs-major-version 24) (>= emacs-minor-version 4))
  (add-to-list 'package-pinned-packages '(cider . "melpa-stable") t))

(let ((path (shell-command-to-string ". ~/dotfiles/.exports.sh; echo -n $PATH")))
  (setenv "PATH" path)
  (setq exec-path
        (append
         (split-string-and-unquote path ":")
         exec-path)))

(add-to-list 'load-path "~/.emacs.d/lib")
(add-to-list 'load-path "~/.emacs.d/custom")

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

(defmacro after (mode &rest body)
  "`eval-after-load' MODE evaluate BODY.
This allows us to define configuration for features that aren't
always installed and only eval that configuration after the feature is loaded.
ELPA packages usually provide an -autoloads feature which we can
use to determine if the package is installed/loaded."
  (declare (indent defun))
  `(eval-after-load (symbol-name ,mode)
     '(progn ,@body)))

;; disable ido faces to see flx highlights.
;; (after 'ido-ubiquitous-autoloads
;;   (setq ido-auto-merge-work-directories-length -1)
;;   (require 'flx-ido)
;;   (ido-mode 1)
;;   (ido-everywhere 1)
;;   (ido-ubiquitous-mode 1)
;;   (setq ido-enable-flex-matching t)
;;   (setq ido-use-faces nil)
;;   (flx-ido-mode 1))

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
 create-lockfiles nil
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
 truncate-lines t
 default-truncate-lines t
 x-select-enable-clipboard t
 x-select-enable-primary t
 hi-lock-auto-select-face t)

(setq initial-buffer-choice 'remember-notes
      remember-notes-buffer-name "*scratch*")

(require 'smartparens-config)
(sp-pair "/" nil :actions :rem)

(blink-cursor-mode t)
(column-number-mode t)
(delete-selection-mode t)
(desktop-save-mode t)
(global-font-lock-mode t)
(icomplete-mode t)
;; (ido-mode t)
(line-number-mode t)
(show-paren-mode t)
(smartparens-global-mode t)
(tooltip-mode -1)
(transient-mark-mode t)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(google-this-mode 1)
(require 'mustache-mode)
(global-auto-revert-mode nil)
(setq auto-revert-remote-files nil
      auto-revert-use-notify t)

;; (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

(define-globalized-minor-mode global-howdoi-minor-mode
  howdoi-minor-mode howdoi-minor-mode)
(global-howdoi-minor-mode 1)

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

;; Disable pager
(setenv "PAGER" (executable-find "cat"))

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

;; (add-hook 'python-mode-hook '(lambda ()
;;                                (setq python-indent-offset 4)
;;                                (require 'virtualenvwrapper)
;;                                (venv-initialize-interactive-shells)
;;                                (venv-initialize-eshell)
;;                                (setq venv-location "~/virtualenv/")
;;                                (setq
;;                                 python-shell-interpreter "ipython"
;;                                 python-shell-interpreter-args ""
;;                                 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
;;                                 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
;;                                 python-shell-completion-setup-code
;;                                 "from IPython.core.completerlib import module_completion"
;;                                 python-shell-completion-module-string-code
;;                                 "';'.join(module_completion('''%s'''))\n"
;;                                 python-shell-completion-string-code
;;                                 "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")))

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

(load "sql")
;; When starting a daemon loading sqlplus causes an error
;; (load "sqlplus")
(load "plsql")
(load "my-sql")
(load "my-erc")
(load "my-keys")

;; Deprecated. No longer using p4.
;; (load "p4")
;; (load "my-p4")

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
(add-to-list 'auto-mode-alist '("\\.org$"    . org-mode))
(add-to-list 'auto-mode-alist '("\\.py$"     . python-mode))
(add-to-list 'auto-mode-alist '("\\.hs$"     . haskell-mode))
(add-to-list 'auto-mode-alist '("test.log$"  . json-mode))

;; Haskell
(add-to-list 'auto-mode-alist '("xmobarrc" . haskell-mode))
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; YAML
(add-hook 'yaml-mode-hook 'subword-mode)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-10"))

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
(add-hook 'clojure-mode-hook 'cider-mode)
(add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook 'subword-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)

(defun magit-default-tracking-name-branch-unescaped
  (remote branch)
  "Use just the branch name for tracking branches."
  branch)

;; Paredit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'clojure-mode-hook #'enable-paredit-mode)
(add-hook 'json-mode-hook #'enable-paredit-mode)

;; Add rainbow delimiters in all programming modes
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (load-theme 'solarized-dark t)))
  (load-theme 'solarized-dark t))

(set-frame-parameter nil 'fullscreen 'maximized)

(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r" "C-x /" "C-x a" "C-c p" "C-c C-x" "C-c /" "C-c C-o" "C-c" "C-c C-d" "C-c h"))
(guide-key-mode 1)

(require 'auto-complete-config)
(ac-config-default)
(setq ac-show-menu-immediately-on-auto-complete t)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
(setq ac-use-menu-map t)
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)

(add-hook 'after-init-hook #'global-flycheck-mode)

;; tramp
(after 'tramp
  (setq tramp-default-method "ssh"))

(after 'tramp-sh
  (when (and (= emacs-major-version 24) (< emacs-minor-version 4))
    ;; fix for http://debbugs.gnu.org/cgi/bugreport.cgi?bug=17238 until it's released in 24.4
    (defun tramp-sh-handle-file-truename (filename &optional counter prev-dirs)
      "Like `file-truename' for Tramp files."
      (with-parsed-tramp-file-name (expand-file-name filename) nil
        (tramp-make-tramp-file-name method user host
                                    (with-tramp-file-property v localname "file-truename"
                                      (let ((result nil)) ; result steps in reverse order
                                        (tramp-message v 4 "Finding true name for `%s'" filename)
                                        (cond
                                         ;; Use GNU readlink --canonicalize-missing where available.
                                         ((tramp-get-remote-readlink v)
                                          (setq result
                                                (tramp-send-command-and-read
                                                 v
                                                 (format "echo \"\\\"`%s --canonicalize-missing %s`\\\"\""
                                                         (tramp-get-remote-readlink v)
                                                         (tramp-shell-quote-argument (tramp-shell-quote-argument localname))))))
                                         ;; Use Perl implementation.
                                         ((and (tramp-get-remote-perl v)
                                               (tramp-get-connection-property v "perl-file-spec" nil)
                                               (tramp-get-connection-property v "perl-cwd-realpath" nil))
                                          (tramp-maybe-send-script
                                           v tramp-perl-file-truename "tramp_perl_file_truename")
                                          (setq result
                                                (tramp-send-command-and-read
                                                 v
                                                 (format "tramp_perl_file_truename %s"
                                                         (tramp-shell-quote-argument localname)))))
                                         ;; Do it yourself. We bind `directory-sep-char' here for
                                         ;; XEmacs on Windows, which would otherwise use backslash.
                                         (t (let* ((directory-sep-char ?/)
                                                   (steps (tramp-compat-split-string localname "/"))
                                                   (localnamedir (tramp-run-real-handler
                                                                  'file-name-as-directory (list localname)))
                                                   (is-dir (string= localname localnamedir))
                                                   (thisstep nil)
                                                   (numchase 0)
                                                   ;; Don't make the following value larger than
                                                   ;; necessary. People expect an error message in
                                                   ;; a timely fashion when something is wrong;
                                                   ;; otherwise they might think that Emacs is hung.
                                                   ;; Of course, correctness has to come first.
                                                   (numchase-limit 20)
                                                   symlink-target)
                                              (while (and steps (< numchase numchase-limit))
                                                (setq thisstep (pop steps))
                                                (tramp-message
                                                 v 5 "Check %s"
                                                 (mapconcat 'identity
                                                            (append '("") (reverse result) (list thisstep))
                                                            "/"))
                                                (setq symlink-target
                                                      (nth 0 (file-attributes
                                                              (tramp-make-tramp-file-name
                                                               method user host
                                                               (mapconcat 'identity
                                                                          (append '("")
                                                                                  (reverse result)
                                                                                  (list thisstep))
                                                                          "/")))))
                                                (cond ((string= "." thisstep)
                                                       (tramp-message v 5 "Ignoring step `.'"))
                                                      ((string= ".." thisstep)
                                                       (tramp-message v 5 "Processing step `..'")
                                                       (pop result))
                                                      ((stringp symlink-target)
                                                       ;; It's a symlink, follow it.
                                                       (tramp-message
                                                        v 5 "Follow symlink to %s" symlink-target)
                                                       (setq numchase (1+ numchase))
                                                       (when (file-name-absolute-p symlink-target)
                                                         (setq result nil))
                                                       ;; If the symlink was absolute, we'll get a
                                                       ;; string like "/user@host:/some/target";
                                                       ;; extract the "/some/target" part from it.
                                                       (when (tramp-tramp-file-p symlink-target)
                                                         (unless (tramp-equal-remote filename symlink-target)
                                                           (tramp-error
                                                            v 'file-error
                                                            "Symlink target `%s' on wrong host"
                                                            symlink-target))
                                                         (setq symlink-target localname))
                                                       (setq steps
                                                             (append (tramp-compat-split-string
                                                                      symlink-target "/")
                                                                     steps)))
                                                      (t
                                                       ;; It's a file.
                                                       (setq result (cons thisstep result)))))
                                              (when (>= numchase numchase-limit)
                                                (tramp-error
                                                 v 'file-error
                                                 "Maximum number (%d) of symlinks exceeded" numchase-limit))
                                              (setq result (reverse result))
                                              ;; Combine list to form string.
                                              (setq result
                                                    (if result
                                                        (mapconcat 'identity (cons "" result) "/")
                                                      "/"))
                                              (when (and is-dir
                                                         (or (string= "" result)
                                                             (not (string= (substring result -1) "/"))))
                                                (setq result (concat result "/"))))))
                                        (tramp-message v 4 "True name of `%s' is `%s'" localname result)
                                        result)))))))


;; (require 'multiple-cursors)
;; (global-set-key (kbd "C-c m") 'mc/edit-lines)
;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; Old M-x
;; (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(defadvice smex (around space-inserts-hyphen activate compile)
        (let ((ido-cannot-complete-command
               `(lambda ()
                  (interactive)
                  (if (string= " " (this-command-keys))
                      (insert ?-)
                    (funcall ,ido-cannot-complete-command)))))
          ad-do-it))

(eval-after-load 'tramp
  '(vagrant-tramp-enable))

(require 'smart-compile)


(require 'compile)
(defun my-php-hook-function ()
  (set (make-local-variable 'compile-command) (format "phpcs --report=emacs --standard=PSR2 %s" (buffer-file-name))))
(add-hook 'php-mode-hook (lambda ()
                           (setq show-trailing-whitespace t)
                           (subword-mode 1)))

(defun upcase-rectangle (b e)
  "change chars in rectangle to uppercase"
  (interactive "r")
  (apply-on-rectangle 'upcase-rectangle-line b e))

(defun upcase-rectangle-line (startcol endcol)
  (when (= (move-to-column startcol) startcol)
    (upcase-region (point)
                   (progn (move-to-column endcol 'coerce)
                          (point)))))

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))
(add-hook 'web-mode-hook 'my-web-mode-hook)

(require 'helm)
(require 'helm-config)
(require 'helm-ls-git)
(require 'helm-descbinds)
;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-d") 'helm-browse-project)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
(global-unset-key (kbd "C-x c"))
(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))
(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-mode-fuzzy-match t
      helm-completion-in-region-fuzzy-match t)
(helm-mode 1)
(helm-descbinds-mode)

;; Project management
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(setq projectile-enable-caching t)
(setq projectile-keymap-prefix (kbd "C-x p"))
(global-set-key (kbd "C-c p g") 'projectile-grep)

(require 'haml-mode)

(require 'editorconfig)
(editorconfig-mode 1)

(add-to-list 'load-path "~/tern/emacs/")
(autoload 'tern-mode "tern.el" nil t)
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))

(set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:"))))
(eval-after-load "tramp"
  '(progn
     (defvar sudo-tramp-prefix
       "/sudo:"
       (concat "Prefix to be used by sudo commands when building tramp path "))
     (defun sudo-file-name (filename)
       (set 'splitname (split-string filename ":"))
       (if (> (length splitname) 1)
         (progn (set 'final-split (cdr splitname))
                (set 'sudo-tramp-prefix "/sudo:")
                )
         (progn (set 'final-split splitname)
                (set 'sudo-tramp-prefix (concat sudo-tramp-prefix "root@localhost:")))
         )
       (set 'final-fn (concat sudo-tramp-prefix (mapconcat (lambda (e) e) final-split ":")))
       (message "splitname is %s" splitname)
       (message "sudo-tramp-prefix is %s" sudo-tramp-prefix)
       (message "final-split is %s" final-split)
       (message "final-fn is %s" final-fn)
       (message "%s" final-fn)
       )

     (defun sudo-find-file (filename &optional wildcards)
       "Calls find-file with filename with sudo-tramp-prefix prepended"
       (interactive "fFind file with sudo ")
       (let ((sudo-name (sudo-file-name filename)))
         (apply 'find-file
                (cons sudo-name (if (boundp 'wildcards) '(wildcards))))))

     (defun sudo-reopen-file ()
       "Reopen file as root by prefixing its name with sudo-tramp-prefix and by clearing buffer-read-only"
       (interactive)
       (let*
           ((file-name (expand-file-name buffer-file-name))
            (sudo-name (sudo-file-name file-name)))
         (progn
           (setq buffer-file-name sudo-name)
           (rename-buffer sudo-name)
           (setq buffer-read-only nil)
           (message (concat "File name set to " sudo-name)))))

     ;;(global-set-key (kbd "C-c o") 'sudo-find-file)
     (global-set-key (kbd "C-c o") 'sudo-reopen-file)))

;; (global-wakatime-mode)

;; (set-fringe-style -1)

;; Ctrl+space, Ctrl + space not working then install dconf-editor,
;; open it, nav to desktop >> ibus >> general >> hotkey, remove
;; 'Ctrl+Space' from trigger and triggers values.
;; http://askubuntu.com/questions/243639/ctrlspace-has-been-bound-to-invoke-some-input-method-and-does-not-work-in-ema

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(browse-url-browser-function (quote browse-url-firefox))
 '(comment-style (quote plain))
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(erc-auto-query (quote frame))
 '(erc-email-userid "justin.valentini@gmail.com")
 '(erc-generate-log-file-name-function (quote erc-generate-log-file-name-network))
 '(erc-keywords (quote ("lunch" "coffee")))
 '(erc-modules
   (quote
    (autojoin button completion fill irccontrols list log match menu move-to-prompt netsplit networks noncommands notifications readonly replace ring scrolltobottom smiley stamp spelling track)))
 '(erc-user-full-name "Justin Valentini")
 '(flycheck-check-syntax-automatically (quote (save new-line mode-enabled)))
 '(grep-files-aliases
   (quote
    (("all" . "* .*")
     ("el" . "*.el")
     ("ch" . "*.[ch]")
     ("c" . "*.c")
     ("cc" . "*.cc *.cxx *.cpp *.C *.CC *.c++")
     ("cchh" . "*.cc *.[ch]xx *.[ch]pp *.[CHh] *.CC *.HH *.[ch]++")
     ("hh" . "*.hxx *.hpp *.[Hh] *.HH *.h++")
     ("h" . "*.h")
     ("l" . "[Cc]hange[Ll]og*")
     ("m" . "[Mm]akefile*")
     ("tex" . "*.tex")
     ("texi" . "*.texi")
     ("asm" . "*.[sS]")
     ("php" . "*.php *.inc"))))
 '(ibuffer-saved-filter-groups
   (quote
    (("justin-default-buffer-groups"
      ("Models"
       (filename . "models"))
      ("Test PHP"
       (name . "test")
       (mode . php-mode))
      ("Fixtures"
       (mode . yaml-mode))
      ("Controllers"
       (filename . "controllers"))
      ("Helpers"
       (filename . "helpers"))
      ("Views"
       (mode . sgml-mode))
      ("Framework"
       (filename . "framework"))
      ("PHP"
       (mode . php-mode))
      ("SQL"
       (or
        (mode . plsql-mode)
        (mode . sql-mode)))
      ("Oracle Sessions"
       (mode . sql-interactive-mode))
      ("Org Mode"
       (mode . org-mode))
      ("Python"
       (mode . python-mode))
      ("Javascript"
       (mode . js2-mode))
      ("Emacs Config"
       (mode . emacs-lisp-mode))
      ("SRC"
       (filename . "src/php"))
      ("Logs"
       (name . "\\.log"))
      ("Dired"
       (mode . dired-mode))
      ("P4 Output"
       (name . "*P4"))))))
 '(ibuffer-saved-filters
   (quote
    (("P4 Output"
      ((name . "*P4 Output*")))
     ("gnus"
      ((or
        (mode . message-mode)
        (mode . mail-mode)
        (mode . gnus-group-mode)
        (mode . gnus-summary-mode)
        (mode . gnus-article-mode))))
     ("programming"
      ((or
        (mode . emacs-lisp-mode)
        (mode . cperl-mode)
        (mode . c-mode)
        (mode . java-mode)
        (mode . idl-mode)
        (mode . lisp-mode)))))))
 '(ido-cache-ftp-work-directory-time 0.1)
 '(magit-push-always-verify nil)
 '(magit-branch-arguments (quote ("--track")))
 '(magit-commit-arguments (quote ("--gpg-sign=73DF50970839B87E")))
 '(magit-default-tracking-name-function (quote magit-default-tracking-name-branch-unescaped))
 '(magit-diff-use-overlays nil)
 '(magit-fetch-arguments (quote ("--prune")))
 '(magit-pull-arguments (quote ("--rebase")))
 '(magit-push-arguments (quote ("--set-upstream")))
 '(magit-use-overlays nil)
 '(mode-require-final-newline nil)
 '(projectile-globally-ignored-directories
   (quote
    (".idea" ".eunit" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".tox" ".svn" "build" "node_modules" "vendor")))
 '(python-check-command "pylint")
 '(safe-local-variable-values (quote ((firestarter . ert-run-tests-interactively))))
 '(smart-compile-alist
   (quote
    ((emacs-lisp-mode emacs-lisp-byte-compile)
     (html-mode browse-url-of-buffer)
     (nxhtml-mode browse-url-of-buffer)
     (html-helper-mode browse-url-of-buffer)
     (octave-mode run-octave)
     ("\\.c\\'" . "gcc -O2 %f -lm -o %n")
     ("\\.[Cc]+[Pp]*\\'" . "g++ -O2 %f -lm -o %n")
     ("\\.m\\'" . "gcc -O2 %f -lobjc -lpthread -o %n")
     ("\\.java\\'" . "javac %f")
     ("\\.php\\'" . "phpunit --filter testMe %f")
     ("\\.cron\\(tab\\)?\\'" . "crontab %f")
     ("\\.tex\\'" tex-file)
     ("\\.texi\\'" . "makeinfo %f")
     ("\\.rb\\'" . "ruby %f"))))
 '(solarized-height-plus-1 1)
 '(solarized-height-plus-2 1)
 '(solarized-height-plus-3 1)
 '(solarized-height-plus-4 1)
 '(sql-product (quote oracle))
 '(helm-ag-base-command "ag --nocolor --nogroup --ignore-case"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-face-tag ((t (:inherit variable-pitch :foreground "#6c71c4" :weight normal :height 1.2))))
 '(custom-group-tag ((t (:inherit variable-pitch :foreground "#268bd2" :height 1.2))))
 '(custom-variable-tag ((t (:inherit variable-pitch :foreground "#2aa198" :height 1.2))))
 '(org-level-1 ((t (:inherit variable-pitch :foreground "#cb4b16" :height 1.0))))
 '(org-level-2 ((t (:inherit variable-pitch :foreground "#859900" :height 1.0))))
 '(org-level-3 ((t (:inherit variable-pitch :foreground "#268bd2" :height 1.0))))
 '(org-level-4 ((t (:inherit variable-pitch :foreground "#b58900" :height 1.0)))))
