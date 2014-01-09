;; make indent 4 spaces in php-mode
(c-add-style "k&r-mods"
             '("k&r"
               (c-basic-offset . 4)
               (c-offsets-alist . ((case-label . +)
                                   (arglist-close . 0)
                                   (inline-open . 0)))
               (c-hanging-braces-alist
                (defun-open after)
                (inline-open after)
                (substatement-open after))))

;; (setq php-mode-user-hook nil)
;; (setq php-mode-hook nil)

(add-hook 'php-mode-hook
          '(lambda () (c-set-style "k&r-mods")))

(add-hook 'php-mode-hook
          '(lambda () (setq fill-column 80)))

(add-hook 'php-mode-hook
          '(lambda () (define-abbrev php-mode-abbrev-table "ex" "extends")))

(add-hook 'php-mode-hook
               (lambda () (subword-mode 1)))

;; allows camel case words to be treated as separate words when using forward-word and backward-word
;; (add-hook 'php-mode-hook
;;           '(lambda () (c-subword-mode 1)))

;; New completion list
(setq php-manual-path (concat lisp-lib-dir "/php-manual")
      php-completion-file (concat custom-dir "/php_function_list.txt"))
