;; startup

(defmacro load-custom (f)
  "Load a local configuration file in ~/.emacs.d/."
  `(load (concat ,(expand-file-name "~/.emacs.d/") ,f)))

(defmacro add-lisp-dir (d)
  "Add dir to load-path."
  `(add-to-list 'load-path (concat ,(expand-file-name "~/.emacs.d/lisp/") ,d)))

(require 'cl) ; need this for (case)
(load-custom
 (case system-type
   ('darwin "darwin")
   ('gnu/linux "linux")
   ('windows-nt "win32")))
(load-custom "funs")
(load-custom "sonian")

(add-to-list 'exec-path (expand-file-name "~/bin"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

(add-to-list 'default-frame-alist '(width . 103))
(add-to-list 'default-frame-alist '(height . 35))

(when (and (>= (string-to-number emacs-version) 23)
           (fboundp 'server-running-p))
  (server-start))
(require 'ffap)
(require 'saveplace)
(require 'package)
(package-initialize)
(toggle-uniquify-buffer-names)
(recentf-mode 1)
(ido-mode t)
(column-number-mode 1)
(setq display-time t
      display-time-24hr-format t)
(display-time)
(prefer-coding-system 'utf-8)

(global-set-key (kbd "C-x C-i") 'ido-imenu)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c C-n") 'clean-up-buffer)
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
(global-set-key (kbd "C-x F") 'find-function)
(global-set-key (kbd "C-M-h") 'backward-kill-word)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-x O")
                (lambda ()
                  (interactive) (other-window -1))) ;; back one
(global-set-key (kbd "C-x C-o")
                (lambda ()
                  (interactive) (other-window 2))) ;; forward two
(global-set-key (kbd "C-x m") 'eshell)
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))
(global-set-key (kbd "C-x M-m") 'shell)
(global-set-key (kbd "C-x 4 t") 'aar/transpose-windows)
(global-set-key (kbd "C-x 4 i") 'aar/irc-home)
(global-set-key (kbd "C-x 4 o") 'aar/irc-home-freenode)
(global-set-key (kbd "C-x 4 8") 'aar/irc-go-to-balls)

(when (equalp "DUMB" (getenv "TERM"))
  (setenv "PAGER" "cat"))

(eval-after-load 'diff-mode
  '(progn (set-face-foreground 'diff-added "green4")
          (set-face-foreground 'diff-removed "red3")))

;; pre-gnus
(setq gnus-home-directory "~/.gnus.d/")
(add-to-list 'load-path (expand-file-name "~/src/gnus/lisp"))
(require 'info)
(add-to-list 'Info-default-directory-list (expand-file-name "~/src/gnus/texi/"))

;; elmer

(require 'elmer)

;; scpaste

(autoload 'scpaste "scpaste" "Paste the current buffer." t nil)
(setq scpaste-http-destination "http://www.draines.com/tmp"
      scpaste-scp-destination "draines:/www/htdocs/draines/tmp")

;; pomodoro
(require 'pomodoro)

;; w3m
(add-lisp-dir "emacs-w3m")
(require 'w3m)

;; magit

(add-lisp-dir "magit")
(require 'magit)
(global-set-key "\C-xg" 'magit-status)
(eval-after-load 'magit
  '(progn (set-face-background 'magit-item-highlight "white")
          (set-face-foreground 'magit-diff-add "green3")
          (set-face-foreground 'magit-diff-del "red3")))


;; lisp

(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)

(defun aar/lispy-parens ()
  "Set up parens for lispish modes."
  (require 'paredit)
  (paredit-mode 1)
  (define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp)
  (define-key paredit-mode-map (kbd "M-(") 'paredit-forward-barf-sexp)
  (show-paren-mode 1))
(add-hook 'emacs-lisp-mode-hook 'aar/lispy-parens)
(add-hook 'emacs-lisp-mode-hook 'whitespace-mode)
(add-hook 'clojure-mode-hook 'aar/lispy-parens)
(add-hook 'clojure-mode-hook 'whitespace-mode)

;; clojure

(add-lisp-dir "slime")
(add-lisp-dir "clojure-mode")
(add-lisp-dir "swank-clojure")
(require 'slime)
(slime-setup '(slime-repl))
(require 'clojure-mode)
(require 'clojurescript-mode)
(require 'clojure-test-mode)
(require 'durendal)
(load "swank-clojure")
(setq slime-net-coding-system 'utf-8-unix)
(setq slime-protocol-version 'ignore)

(setq swank-clojure-binary (expand-file-name "~/bin/clojure"))
(require 'assoc)  ;; swank-clojure-project fails without

(defun classpathize (paths)
  (mapconcat 'expand-file-name paths ":"))

(defmacro add-clojure-project (project path)
  (let* ((fullpath (expand-file-name path))
         (classpaths (mapcar
                      (lambda (p)
                        (concat fullpath p))
                      '("/lib/*"
                        "/src")))
         (clj (expand-file-name "~/bin/clojure")))
    `(add-to-list 'slime-lisp-implementations
                  (backquote
                   (,project
                    ("env"
                     ,(concat "CLASSPATH=" (classpathize classpaths))
                     ,clj)
                    :init swank-clojure-init)) t)))

(progn
  (setq slime-lisp-implementations '())
  (add-clojure-project clojure "~/src/scratch"))

;; geiser
(add-lisp-dir "geiser")
(eval-after-load 'geiser
  '(setq geiser-scheme-dir
         (expand-file-name "~/.emacs.d/share/geiser/scheme")))
(require 'geiser)

;; haskell

(load "~/.emacs.d/lisp/haskell-mode/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

;; erlang

(add-lisp-dir "erlang")
(require 'erlang-start)

;; lua
(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

;; jinja2
(require 'jinja2-mode)
(setq auto-mode-alist (cons '("\\.j2$" . jinja2-mode) auto-mode-alist))

;; nxhtml
;;(load-custom "lisp/nxhtml/autostart") please no debug-on-error

;; go
(require 'go-mode-load)
(add-hook 'go-mode-hook (lambda () (setq tab-width 2)))

;; groovy
(require 'groovy-mode)
(setq auto-mode-alist (cons '("\\.groovy$" . groovy-mode) auto-mode-alist))

;; perl
(add-lisp-dir "sepia")
(setq sepia-perl5lib (list
                      (expand-file-name "~/.emacs.d/lisp/sepia/lib")))
(defalias 'perl-mode 'sepia-mode)
(require 'sepia)

;; python
(require 'python)

;; javascript
(setq js-indent-level 2)

;; scala
(add-lisp-dir "scala-mode")
(require 'scala-mode-auto)

;; org

(org-remember-insinuate)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key [(control meta ?r)] 'org-remember)
(setq org-directory "~/.org"
      org-agenda-files (quote ("~/.org/PERSONAL.org"
                               "~/.org/clients"))
      org-log-done t
      org-agenda-show-all-dates t
      org-agenda-ndays 7
      org-agenda-start-on-weekday nil
      org-export-publishing-directory "~/.org/export"
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-reverse-note-order t
      org-clock-modeline-total 'current
      org-mobile-directory "~/Dropbox/MobileOrg"
      org-mobile-inbox-for-pull "~/.org/mobile-inbox.org")
(setq org-todo-keywords
      '((sequence "TODO" "WAITING" "DEFERRED"
                  "|" "DONE" "DELEGATED" "CANCELED")))

(setq org-default-notes-file "~/.org/notes.org")

(setq org-remember-default-headline "Tasks"
      org-remember-templates
      '(("Personal task" 112 "* TODO %?\n  %u" "PERSONAL.org")
        ("NotifyMD" 109 "* TODO %?\n  %u" "clients/NMD.org")
        ("Trinity" 116 "* TODO %?\n  %u" "clients/TRINITY.org")
        ("Sonian" 115 "* TODO %?\n  %u" "clients/SONIAN.org")
        ("Note" 110 "* %u %?" "notes.org" 'bottom)))

(add-hook 'org-mode-hook (lambda () (setq fill-column 80)))
(add-hook 'org-mode-hook 'auto-fill-mode)
(require 'epresent)

;; ledger

(require 'ledger)
(eval-after-load 'ledger
  '(progn
     (define-key ledger-mode-map
       [(control ?c) (control ?n)] 'ledger-clean-up-transaction)
     (define-key ledger-mode-map
       [(control ?c) (control ?b)] 'ledger-bal-region)))

;; markdown
(require 'markdown-mode)
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.markdown" . markdown-mode) auto-mode-alist))


;; erc

(require 'erc)
(require 'erc-highlight-nicknames)
(add-to-list 'erc-modules 'log)
(add-to-list 'erc-modules 'highlight-nicknames)

(setq erc-server "irc.us.freenode.net"
      erc-port 6667
      erc-nick "drewr"
      erc-user-full-name "Drew Raines"
      erc-email-userid "drew"       ; for when ident is not activated
      erc-fill-column 86
      erc-fill-prefix "   "
      erc-auto-query 'window-noselect
      erc-prompt-for-nickserv-password nil
      erc-nickserv-passwords nil
      erc-auto-discard-away t
      erc-autoaway-idle-seconds 600
      erc-log-insert-log-on-open nil
      erc-log-channels-directory "~/tmp/irc/log"
      erc-kill-queries-on-quit nil
      erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT" "TOPIC"
                                "NAMES" "324" "329" "332" "333" "353" "477"
                                "NOTICE")
      erc-encoding-coding-alist '(("#emacs" . utf-8))
      erc-current-nick-highlight-type 'nick
      erc-max-buffer-size 100000
      erc-join-buffer 'bury
      erc-server-send-ping-interval 10
      erc-server-send-ping-timeout 20
      erc-server-reconnect-timeout 30
      erc-server-reconnect-attempts 100
      erc-log-p nil
      erc-prompt (lambda () (concat (erc-default-target) ">"))
      erc-favorite-channel
      "\\(84115\\|#search\\|safe\\|devs\\)")

(setq-default erc-ignore-list '("^xah_?" "^jordanb_?"))

(load "~/.erc-auth.el")

;; edit-server
(require 'edit-server)


;; Customize

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
 '(safe-local-variable-values
   (quote
    ((prompt-to-byte-compile)
     (before-save-hook delete-trailing-whitespace))))
 '(global-font-lock-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-echo-area-message t)
 '(inhibit-startup-screen t)
 '(make-backup-files nil)
 '(menu-bar-mode nil nil (menu-bar))
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 1) ((control)))))
 '(scroll-bar-mode nil)
 '(sh-indentation 2)
 '(show-paren-mode nil)
 '(tool-bar-mode nil nil (tool-bar))
 '(transient-mark-mode nil)
 '(truncate-lines nil)
 '(require-final-newline t)
 '(whitespace-style '(face trailing lines space-before-tab
                           indentation space-after-tab))
 '(whitespace-line-column 80)
 '(hfy-default-face-def
   (quote ((t :foreground "black" :background "white" :family "monospace"))))
 '(hfy-display-class
   (quote ((type . x-toolkit) (class . color) (background . light)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(circe-my-message-face ((t (:foreground "DarkRed")))))

(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
