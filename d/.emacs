; -*- emacs-lisp -*-

;; Platform-agnostic stuff before loading .emacs.<os>
(add-to-list 'exec-path (expand-file-name "~/bin"))
(add-to-list 'load-path (expand-file-name "~/elisp"))

(defmacro load-custom (f)
  "Load a local configuration file in ~/.emacs.d/."
   `(load-file
     (concat ,(expand-file-name "~/.emacs.d/.emacs.") ,f)))

(defun shell-find-in-dir (expr dir)
  "Find all files or directories in DIR that match the shell glob EXPR."
  (interactive)
  (split-string
   (shell-command-to-string
    (concat "find " dir " -name " expr)) "\n" t))

(defun count-words-region (beginning end)
  "Print number of words in the region."
  (interactive "r")
  (message "Counting words in region ... ")
  (save-excursion
    (let ((count 0))
      (goto-char beginning)
      (while (and (< (point) end)
                  (re-search-forward "\\w+\\W*" end t))
        (setq count (1+ count)))
      (cond ((zerop count)
             (message
              "The region does NOT have any words."))
            ((= 1 count)
             (message
              "The region has 1 word."))
            (t
             (message
              "The region has %d words." count))))))

(column-number-mode 1)
(setq display-time t
      display-time-24hr-format t)
(display-time)
(prefer-coding-system 'utf-8)
(setq mail-user-agent 'gnus-user-agent)

(setq aar-favorite-freenode-chans
      '("#gnus" "#ledger" "#nashdl" "#emacs" "#org-mode" "#scsh"
        "#clojure" "##spiderpig" "#nihilist" "#running" "##1452"))

; Custom keys
(global-set-key (kbd "M-<f4>") 'find-file-at-point)
;(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key (kbd "M-s") 'isearch-forward-regexp)
(global-set-key (kbd "M-r") 'isearch-backward-regexp)

; Diverge for a moment into platform-dependent stuff
(require 'cl) ; need this for (case)
(load-custom
  (case system-type
   ('darwin "mac")
   ('gnu/linux "linux")
   ('windows-nt "win32")))

;; (load-custom "circe")
(load-custom "clojure")
(load-custom "crypt")
(load-custom "env")
(load-custom "erc")
(load-custom "erlang")
(load-custom "gmane")
(load-custom "haskell")
(load-custom "html")
;; (load-custom "jdee")
(load-custom "js")
(load-custom "ledger")
(load-custom "lua")
(load-custom "org")
(load-custom "php")
;; (load-custom "planner")
(load-custom "python")
(load-custom "ruby")
;; (load-custom "slime")
(load-custom "vc")
(load-custom "w3m")
(load-custom "xml")

(defun aar/lispy-parens ()
  "Set up parens for lispish modes."
  (require 'paredit)
  (paredit-mode 1)
  (make-variable-buffer-local 'show-paren-mode)
  (show-paren-mode 1)
  (setq show-trailing-whitespace t))
(add-hook 'emacs-lisp-mode-hook 'aar/lispy-parens)

; Markdown
(add-to-list 'auto-mode-alist '("\\.mtxt$" . text-mode))

; Textile
(require 'textile-mode)

; CSS
(require 'css-mode)
;(setq cssm-newline-before-closing-bracket nil)
(setq css-indent-offset 2)
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))

; Django
(require 'django-html-mode)

; Perl
(fset 'perl-mode 'cperl-mode)

; PL/SQL
(require 'plsql)
(add-to-list 'auto-mode-alist '("\\.pls$" . plsql-mode))

; bbdb stuff
(add-to-list 'load-path (expand-file-name "~/elisp/bbdb/lisp"))
(require 'bbdb)
(bbdb-initialize)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
; Work around silly double-quoting issue
(add-hook 'bbdb-mail-abbrev-expand-hook
          (lambda (foo bar) ; don't need them
            (save-excursion
              (save-match-data
                (let ((end (save-excursion (search-backward ":" nil t))))
                  (while (search-backward "\"\"" end t)
                    (replace-match "\"")))))))
(setq bbdb-always-add-addresses nil
      bbdb-use-pop-up nil
      bbdb-completion-display-record nil
      bbdb-quiet-about-name-mismatches 0.01
      bbdb-file-coding-system 'utf-8
      bbdb-completion-type 'primary-or-name)

; G-client
;;; (add-to-list 'load-path (expand-file-name "~/elisp/g-client"))
;;; (load-library "g")
;;; (setq g-user-email "aaraines@gmail.com")

; Color theme
(add-to-list 'load-path (expand-file-name "~/elisp/color-theme"))
(require 'color-theme)
(color-theme-initialize)

;; Backup files:
;; Put them in one nice place if possible
(if (file-directory-p "~/.backup")
    (setq backup-directory-alist '(("." . "~/.backup")))
  (message "Directory does not exist: ~/.backup"))

(add-hook 'text-mode-hook '(lambda () (setq fill-column 63)))
;(add-hook 'text-mode-hook 'auto-fill-mode)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(calc-float-format (quote (fix 2)) t)
 '(canlock-password "9a765f623398f9e6a768c158f2fca8588d732be7")
 '(cua-mode nil nil (cua-base))
 '(current-language-environment "UTF-8")
 '(cursor-in-non-selected-windows nil)
 '(dired-recursive-deletes (quote top))
 '(enable-recursive-minibuffers t)
 '(global-font-lock-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-echo-area-message t)
 '(inhibit-startup-screen t)
 '(make-backup-files nil)
 '(menu-bar-mode nil nil (menu-bar))
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 1) ((control)))))
 '(safe-local-variable-values (quote ((prompt-to-byte-compile))))
 '(scroll-bar-mode nil)
 '(sh-indentation 2)
 '(show-paren-mode nil)
 '(tool-bar-mode nil nil (tool-bar))
 '(transient-mark-mode nil)
 '(truncate-lines nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(circe-my-message-face ((t (:foreground "DarkRed")))))

(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

